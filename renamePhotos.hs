import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Control.Monad.IO.Class
import Control.Proxy
import Data.Either
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Maybe
import System.Directory
import System.FilePath.Posix
import System.IO
import System.Process
import Text.Parsec
import Text.Parsec.String

import qualified Data.ByteString as BS
import qualified Data.Map as DM
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding.Error as DTEE

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
    execWriterT $ runProxy $ raiseK (getRecursiveContents path) >-> toListD

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
    = intercalate "-" [year, month, day] ++ "++" ++ intercalate "-" [hour, minute, second] ++ "-" ++ e ++ ".jpg"
finalFilename (PhotoDateTime year month day hour minute second Nothing)
    = intercalate "-" [year, month, day] ++ "++" ++ intercalate "-" [hour, minute, second] ++ ".jpg"

safeRename :: FilePath -> FilePath -> IO ()
safeRename old new = do exists <- doesFileExist new

                        if exists then putStrLn $ "error: can't rename " ++ old ++ " because target " ++ new ++ " exists"
                                  else do renameFile old new 
                                          putStrLn $ old ++ " -> " ++ new


subsecond :: ParsecT String u Data.Functor.Identity.Identity String
subsecond = do
    char '_'
    many (noneOf ".")

{-
samsungPhotoFile parses a filename from my Samsung Galaxy S3, as seen on the 
internal storage via sshfs.

A normal file looks like this:

    20130519_140044.jpg

If two files have the same datestamp down to the second, they are differentiated by
a _{n} suffix (which is parsed by subsecond):

    20130504_121734_1.jpg
    20130504_121734_2.jpg
-}

samsungPhotoFile = do
    year1 <- digit
    year2 <- digit
    year3 <- digit
    year4 <- digit

    month1 <- digit
    month2 <- digit

    day1 <- digit
    day2 <- digit

    char '_'

    hour1 <- digit
    hour2 <- digit

    minute1 <- digit
    minute2 <- digit

    second1 <- digit
    second2 <- digit

    end <- optionMaybe (try subsecond)

    string ".jpg"

    let year    = [year1, year2, year3, year4]
        month   = [month1, month2]
        day     = [day1, day2]
        hour    = [hour1, hour2]
        minute  = [minute1, minute2]
        second  = [second1, second2]

    case end of Just e  -> return $ PhotoDateTime year month day hour minute second (Just e)
                Nothing -> return $ PhotoDateTime year month day hour minute second Nothing

createdTime :: ParsecT String u Data.Functor.Identity.Identity PhotoDateTime
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

parseCreatedTimeFromExif :: String -> IO (Maybe PhotoDateTime)
parseCreatedTimeFromExif f = do
    (Just hin, Just hout, Just herr, pid) <- liftIO $ createProcess (proc "exiftags" [f]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdout <- liftIO $ readRestOfHandle hout
    -- stderr <- liftIO $ readRestOfHandle herr  -- FIXME we're ignoring stderr...

    let linesWithTerminatingNewlines = map (++ "\n") (lines stdout)
        parseBlah = map (parse createdTime "") linesWithTerminatingNewlines
        parseResults = rights parseBlah

    case length parseResults of 1 -> return $ Just $ head parseResults
                                _ -> return Nothing

-- Work out new file name by parsing a samsung-like name (handles the subsecond issue)
newFilenameFromSamsung :: FilePath -> Maybe FilePath
newFilenameFromSamsung f = case new of (Right new') -> Just (joinPath [fst $ splitFileName f, new'])
                                       _            -> Nothing
    where new = finalFilename <$> parse samsungPhotoFile "" (snd $ splitFileName f)

-- Workout the new filename using exif tags (does not handle the subsecond issue)
newFilenameFromExif :: FilePath -> IO (Maybe FilePath)
newFilenameFromExif f = do new <- parseCreatedTimeFromExif f

                           case new of (Just new') -> return $ Just (joinPath [fst $ splitFileName f, finalFilename new']) 
                                       _           -> return Nothing

main = do
    files <- getRecursiveContentsList "."
    forM_ files (\f -> do let new1 = newFilenameFromSamsung f
                          new2 <- newFilenameFromExif f

                          let news = catMaybes [new1, new2]

                          if null news then putStrLn $ "skipping " ++ f
                                       else safeRename f (head news))
