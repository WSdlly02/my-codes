{-# LANGUAGE OverloadedStrings #-}

-- 导入JuicyPixels
import Codec.Picture
import Control.Monad (forever, replicateM)
import Data.Binary.Get
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import System.IO (BufferMode (..), hFlush, hSetBuffering, stdin, stdout)

-- 数据解析函数
parseThermalData :: Get (Double, [Float])
parseThermalData = do
  timestamp <- getDoublele
  temperatures <- replicateM 768 getFloatle -- 现在 replicateM 已可用
  return (timestamp, temperatures)

arrayToMatrix_32 :: [Float] -> [[Float]]
arrayToMatrix_32 [] = []
arrayToMatrix_32 xs = take 32 xs : arrayToMatrix_32 (drop 32 xs)

normalize :: [Float] -> [Float] -- 温度数据归一函数
normalize = map normalize_step2
 where
  normalize_step2 :: Float -> Float
  normalize_step2 x
    | x <= 10 = 0
    | x <= 40 = (x - 10) / 30 -- 主要监控10-40度
    | otherwise = 1

toJetColor :: Float -> (Int, Int, Int) -- 温度数据映射至RGB
toJetColor x = (round (clamp r * 255), round (clamp g * 255), round (clamp b * 255))
 where
  -- 分段计算RGB分量
  (r, g, b)
    | x < 0.125 = (0, 0, 0.5 + 4 * x) -- 深蓝到蓝
    | x < 0.375 = (0, 4 * (x - 0.125), 1.0) -- 蓝到青
    | x < 0.625 = (4 * (x - 0.375), 1.0, 1.0 - 4 * (x - 0.375)) -- 青到黄
    | x < 0.875 = (1.0, 1.0 - 4 * (x - 0.625), 0) -- 黄到红
    | x <= 1 = (max (1.0 - 4 * (x - 0.875)) 0.5, 0, 0) -- 红到暗红
    | otherwise = (1, 1, 1) -- 将特殊值映射成白色
  clamp = max 0 . min 1

-- scaleMatrix :: Int -> [[(Int, Int, Int)]] -> [[(Int, Int, Int)]] -- 缩放矩阵
-- scaleMatrix factor = concatMap (replicate factor) . map (concatMap (replicate factor)) -- 不全调用
-- scaleMatrix' :: Int -> [[Int]] -> [[Int]]
-- scaleMatrix' factor = concatMap (replicate factor) . map (concatMap (replicate factor))

tempRecogAlgo :: [[Int]] -> [[Int]] -- 危险温度矩阵转换图像识别
tempRecogAlgo = map tempRecogAlgo_step2 -- 脱一层外壳,长度24
 where
  tempRecogAlgo_step2 ys = map (encircleAlgo ys) [0 .. 31] -- 长度32
   where
    encircleAlgo zs n
      | n == 0 && zs !! n /= 0 = 3 -- 边界检查
      | n == 0 && zs !! n == 0 = zs !! n -- 边界检查
      | n == 31 && zs !! n /= 0 = 3 -- 边界检查
      | n == 31 && zs !! n == 0 = zs !! n -- 边界检查
      | zs !! n == zs !! (n - 1) && zs !! n < zs !! (n + 1) = 3 -- 002
      | zs !! n == zs !! (n - 1) && zs !! n == zs !! (n + 1) = zs !! n -- 222/000
      | zs !! n < zs !! (n - 1) && zs !! n == zs !! (n + 1) = 3 -- 200
      | zs !! n > zs !! (n - 1) && zs !! n == zs !! (n + 1) = zs !! n -- 022
      | zs !! n == zs !! (n - 1) && zs !! n > zs !! (n + 1) = zs !! n -- 220
      | otherwise = zs !! n

filterUselessWarning :: [[Int]] -> [[Int]] -- 过滤危险温度矩阵中不是3的元素
filterUselessWarning = map filterUselessWarning_step2 -- 尾递归
 where
  filterUselessWarning_step2 :: [Int] -> [Int]
  filterUselessWarning_step2 = reverse . go []
   where
    go :: [Int] -> [Int] -> [Int]
    go acc [] = acc
    go acc (y : ys)
      | y == 3 = go (3 : acc) ys
      | otherwise = go (0 : acc) ys

mergeMatrices :: [[(Int, Int, Int)]] -> [[Int]] -> [[(Int, Int, Int)]]
mergeMatrices = zipWith mergeRow -- 逐行合并
 where
  -- 行合并逻辑
  mergeRow :: [(Int, Int, Int)] -> [Int] -> [(Int, Int, Int)]
  mergeRow = zipWith mergePixel

  -- 像素合并规则
  mergePixel :: (Int, Int, Int) -> Int -> (Int, Int, Int)
  mergePixel aPixel 0 = aPixel -- b为0时保留原值
  mergePixel _ _ = (0, 0, 0) -- b非0时置黑
  ---------------------------
  -- 将矩阵转换为 PNG 图像

matrixToPng :: [[(Int, Int, Int)]] -> BL.ByteString
matrixToPng frame = encodePng $ generateImage pixelRenderer 32 24
 where
  pixelRenderer x y
    | y < rows && x < cols =
        let (r, g, b) = frameSafe !! y !! x
         in PixelRGB8 (clamp r) (clamp g) (clamp b)
    | otherwise = PixelRGB8 0 0 0 -- 超界处理
   where
    rows = length frame
    cols = if null frame then 0 else length (head frame)
    frameSafe = map (take cols) (take rows frame) -- 确保每行长度一致
  clamp = fromIntegral . max 0 . min 255

----------------------------
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  let chunkSize = 8 + 4 * 768

  forever $ do
    bytes <- B.hGet stdin chunkSize
    case runGetOrFail parseThermalData (BL.fromStrict bytes) of
      Left (_, _, err) ->
        putStrLn $ "解析错误: " ++ err
      Right (_, _, (_, temps)) -> do
        let tempNormalization = normalize temps -- 将温度数据归一至0-1
        let tempNormalizationMatrix = arrayToMatrix_32 tempNormalization -- 将768个归一化的温度数据存进32*24的二维矩阵
        let tempColorMap = refillMatrixWithColor tempNormalizationMatrix -- 将归一化的矩阵数据转为RGB矩阵
            refillMatrixWithColor :: [[Float]] -> [[(Int, Int, Int)]]
            refillMatrixWithColor = map refillArrayWithColor
             where
              refillArrayWithColor :: [Float] -> [(Int, Int, Int)]
              refillArrayWithColor = reverse . go []
               where
                go :: [(Int, Int, Int)] -> [Float] -> [(Int, Int, Int)]
                go acc [] = acc
                go acc (y : ys) = go (toJetColor y : acc) ys
        -- let scaledTempColorMap = scaleMatrix 10 tempColorMap -- 76800个rgb矩阵
        -- 基本图像处理完成
        let warningTempMatrix = refillMatrixWithTemp tempNormalizationMatrix -- 过滤32*24矩阵，记录危险温度
            refillMatrixWithTemp :: [[Float]] -> [[Int]]
            refillMatrixWithTemp = map refillArrayWithTemp -- 脱一层外壳
             where
              refillArrayWithTemp :: [Float] -> [Int]
              refillArrayWithTemp = reverse . go []
               where
                go :: [Int] -> [Float] -> [Int]
                go acc [] = acc
                go acc (y : ys)
                  | y < 2 / 3 = go (0 : acc) ys -- 小于30度
                  | y < 1 = go (1 : acc) ys -- 小于40度 为测试方便，将1、2全部当成危险温度识别
                  | otherwise = go (2 : acc) ys -- 大于等于40度
        let recogRectangle = tempRecogAlgo warningTempMatrix
        let filteredRecogRectangle = filterUselessWarning recogRectangle -- 最终的overlay层
        let frame = mergeMatrices tempColorMap filteredRecogRectangle
        let pngData = matrixToPng frame
        putStr "--FRAME--\n"
        BL.putStr pngData -- 输出帧数据
        hFlush stdout
