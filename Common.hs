module Common where

import Control.Applicative hiding ((<|>),many)
import Control.Monad (forM_)
import Control.Proxy
import Data.Functor.Identity
import Data.List (intercalate)
import System.Directory
import System.FilePath.Posix
import System.IO
import Text.Parsec
import qualified Data.Map as DM


import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding.Error as DTEE

import qualified Data.ByteString as BS
-- import qualified System.IO.Strict as S

data PhotoDateTime = PhotoDateTime { photoYear      :: String
                                   , photoMonth     :: String
                                   , photoDay       :: String
                                   , photoHour      :: String
                                   , photoMinute    :: String
                                   , photoSecond    :: String
                                   , photoSubsecond :: Maybe String
                                   }
    deriving (Show)

-- http://stackoverflow.com/questions/14259229/streaming-recursive-descent-of-a-directory-in-haskell/14261710#14261710
getRecursiveContents :: (Proxy p) => FilePath -> () -> Producer p FilePath IO ()
getRecursiveContents topPath () = runIdentityP $ do
  names <- lift $ getDirectoryContents topPath
  let properNames  = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- lift $ doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path ()
      else respond path

-- Note on execWriterT/raiseK: http://ocharles.org.uk/blog/posts/2012-12-16-24-days-of-hackage-pipes.html
getRecursiveContentsList :: FilePath -> IO [FilePath]
getRecursiveContentsList path =
    execWriterT $ runProxy $ raiseK (getRecursiveContents path) >-> toListD >>= return

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle handle = do
    ineof <- hIsEOF handle
    if ineof
        then return ""
        else do x <- BS.hGetContents handle
                return $ DT.unpack $ DTE.decodeUtf8With DTEE.lenientDecode x  

finalFilename (PhotoDateTime year month day hour minute second (Just e))
    = (intercalate "-" [year, month, day]) ++ "++" ++ (intercalate "-" [hour, minute, second]) ++ "-" ++ e ++ ".jpg"
finalFilename (PhotoDateTime year month day hour minute second Nothing)
    = (intercalate "-" [year, month, day]) ++ "++" ++ (intercalate "-" [hour, minute, second]) ++ ".jpg"

safeRename :: FilePath -> FilePath -> IO ()
safeRename old new = do exists <- doesFileExist new

                        if exists then putStrLn $ "error: can't rename " ++ old ++ " because target " ++ new ++ " exists"
                                  else do renameFile old new 
                                          putStrLn $ old ++ " -> " ++ new

