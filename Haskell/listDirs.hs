import Control.Monad (foldM, when)
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  getFileSize,
  listDirectory,
 )
import System.FilePath ((</>))

-- 定义存储文件信息的元组类型
type FileInfo = ([FilePath], [Integer])

-- 递归遍历并收集文件信息
listFilesRecursively :: FilePath -> IO FileInfo
listFilesRecursively path = do
  entries <- listDirectory path
  foldM processEntry ([], []) entries -- 初始为空列表，逐步累积结果
 where
  processEntry (paths, sizes) entry = do
    let filePath = path </> entry
    isFile <- doesFileExist filePath
    isDir <- doesDirectoryExist filePath

    if isFile
      then do
        size <- getFileSize filePath
        let newPaths = paths ++ [filePath] -- 追加路径到列表
            newSizes = sizes ++ [size] -- 追加大小到列表
        return (newPaths, newSizes)
      else
        if isDir
          then do
            (subPaths, subSizes) <- listFilesRecursively filePath -- 递归处理子目录
            return (paths ++ subPaths, sizes ++ subSizes)
          else return (paths, sizes) -- 忽略非常规文件

-- 示例：从当前目录开始遍历并打印结果
main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  (paths, sizes) <- listFilesRecursively currentDir
  putStrLn "文件路径列表:"
  print paths
  putStrLn "\n文件大小列表:"
  print sizes