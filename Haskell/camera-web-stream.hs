{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types 
  ( status200, status404
  , hContentType
  )
import Control.Monad (forever, unless)
import Control.Concurrent (threadDelay)
import System.IO (hSetBuffering, BufferMode(..))
import System.Process 
  ( createProcess, proc, StdStream(..), waitForProcess
  , std_out, createPipe
  )
import qualified Data.ByteString as B
import Data.ByteString.Builder 
  ( Builder, byteString, stringUtf8 )

-- 配置参数
captureWidth :: String
captureWidth = "640"

captureHeight :: String
captureHeight = "480"

port :: Int
port = 8080

-- HTML 页面内容
htmlPage :: Builder
htmlPage = stringUtf8 $ unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
  , "  <title>树莓派摄像头实时流</title>"
  , "  <meta charset=\"UTF-8\">"
  , "  <style>"
  , "    body { margin: 0; background: #000; }"
  , "    #video { width: 100%; height: 100vh; object-fit: contain; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <img id=\"video\" src=\"/stream\">"
  , "</body>"
  , "</html>"
  ]

-- 摄像头进程初始化
startCameraProcess :: IO (IO B.ByteString)
startCameraProcess = do
  (_, Just hout, _, phandle) <- createProcess 
    (proc "libcamera-jpeg" 
      [ "-o", "-", "--width", captureWidth
      , "--height", captureHeight, "--timelapse", "33"
      , "--nopreview", "--flush", "--framerate", "30"
    ]) { std_out = CreatePipe }
  
  hSetBuffering hout NoBuffering
  return $ B.hGetSome hout 8192

-- MJPEG 流响应
mjpegResponse :: IO Response
mjpegResponse = do
  getFrame <- startCameraProcess
  return $ responseStream 
    status200 
    [("Content-Type", "multipart/x-mixed-replace; boundary=frame")] 
    $ \writeChunk _flush -> forever $ do
        frame <- getFrame
        unless (B.null frame) $ do
          writeChunk $ mconcat
            [ stringUtf8 "--frame\r\n"
            , stringUtf8 "Content-Type: image/jpeg\r\n\r\n"
            , byteString frame
            , stringUtf8 "\r\n"
            ]

-- 应用路由
app :: Application
app req respond = case pathInfo req of
  []       -> respond $ responseBuilder status200 [htmlHeader] htmlPage
  ["stream"] -> do
    resp <- mjpegResponse
    respond resp
  _        -> respond $ responseBuilder status404 [] (stringUtf8 "Not Found")
  where
    htmlHeader = (hContentType, "text/html; charset=utf-8")

main :: IO ()
main = do
  putStrLn $ "服务已启动: http://0.0.0.0:" ++ show port
  run port app