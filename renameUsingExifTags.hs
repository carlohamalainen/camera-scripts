import Control.Applicative hiding ((<|>),many)
-- import Control.Exception
import Control.Monad (forM_)
import Control.Monad.IO.Class
-- import Control.Monad.Identity
-- import Control.Monad.Reader
import Control.Proxy
-- import Control.Proxy.Trans.Writer
import Data.Maybe
-- import Data.Char (toLower)
import Data.List (intercalate)
import System.Directory
-- import System.Environment ( getArgs )
import System.FilePath.Posix
import System.IO
import System.Process
import Control.Monad
-- import Control.Monad.State
import Data.Functor.Identity
import System.Directory
import System.FilePath.Posix
import System.IO
import System.Process
import Text.Parsec
import Text.Parsec.String
import Data.Either

import Common

createdTime :: ParsecT String u Data.Functor.Identity.Identity (PhotoDateTime)
createdTime = do
    string "Image Created: "
   
    year  <- many (noneOf ":")
    char ':'
    month <- many (noneOf ":")
    char ':'
    day   <- many (noneOf " ")
    char ' '

    hour   <- many (noneOf ":")
    char ':'
    minute <- many (noneOf ":")
    char ':'
    second <- many (noneOf "\n")
    char '\n'

    return $ PhotoDateTime year month day hour minute second Nothing

parseCreatedTimeFromExif :: FilePath -> IO (Maybe PhotoDateTime)
parseCreatedTimeFromExif f = do
    (Just hin, Just hout, Just herr, pid) <- liftIO $ createProcess (proc "exiftags" [f]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdout <- liftIO $ readRestOfHandle hout
    -- stderr <- liftIO $ readRestOfHandle herr  -- FIXME we're ignoring stderr...

    let linesWithTerminatingNewlines = map (++ "\n") (lines stdout)
        parseResults = rights $ map (parse createdTime "") linesWithTerminatingNewlines

    case length parseResults of 1 -> return $ Just $ head parseResults
                                _ -> return $ Nothing

main = do
    files <- getRecursiveContentsList "."

    forM_ files (\f -> do print f
                          parseCreatedTimeFromExif f >>= print)




