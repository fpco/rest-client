import Control.Monad
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Directory

main = defaultMainWithHooks simpleUserHooks { postInst = myPostInst }

myPostInst ::
  Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostInst _ _ pkg_descr lbi = do
  copyDirectory "docs" flat_htmldir
  where InstallDirs { htmldir = flat_htmldir } =
          absoluteInstallDirs pkg_descr lbi NoCopyDest

copyDirectory ::  FilePath -> FilePath -> IO ()
copyDirectory src dst = do
  content <- getDirectoryContents src
  forM_ (filter (`notElem` [".", ".."]) content) $ \name -> do
    let srcPath = src ++ "/" ++ name
        dstPath = dst ++ "/" ++ name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then do targetExists <- doesDirectoryExist dstPath
              unless targetExists $ createDirectory dstPath
              copyDirectory srcPath dstPath
      else copyFile srcPath dstPath

-- Setup.hs ends her