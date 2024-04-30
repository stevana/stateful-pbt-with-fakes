module Example.FileSystem.Real where

import System.Directory
import System.IO

import Example.FileSystem.Fake

------------------------------------------------------------------------

root :: FilePath
root = "/tmp/qc-test"

rMkDir :: Dir -> IO ()
rMkDir d = createDirectory (dirFP root d)

rOpen :: File -> IO Handle
rOpen f = openFile (fileFP root f) AppendMode

rWrite :: Handle -> String -> IO ()
rWrite h s = hPutStr h s

rClose :: Handle -> IO ()
rClose h = hClose h

rRead :: File -> IO String
rRead f = readFile (fileFP root f)
