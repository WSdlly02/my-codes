import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  listDirectory,
 )
import System.FilePath ((</>))

-- 递归遍历并打印文件路径
listFilesRecursively :: FilePath -> IO ()
listFilesRecursively path = do
  entries <- listDirectory path
  mapM_ processEntry entries
 where
  processEntry entry = do
    let fullPath = path </> entry
    isFile <- doesFileExist fullPath
    isDir <- doesDirectoryExist fullPath
    if isFile
      then putStrLn fullPath -- 打印文件路径
      else
        if isDir
          then listFilesRecursively fullPath -- 递归处理子目录
          else return () -- 忽略非常规文件（如符号链接）

-- 示例：从当前目录开始遍历
main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  listFilesRecursively currentDir