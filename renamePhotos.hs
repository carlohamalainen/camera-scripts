{-# LANGUAGE FlexibleContexts, RankNTypes #-}

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (toLower)
import Data.Either
import Data.Functor.Identity
import Data.List (intercalate, isSuffixOf)
import Data.Maybe
import System.Directory
import System.FilePath.Posix
import System.IO
import System.Process
import Text.Parsec
import Text.Parsec.String
import Text.Printf (printf)

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

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle handle = do
    ineof <- hIsEOF handle
    if ineof
        then return ""
        else do x <- BS.hGetContents handle
                return $ DT.unpack $ DTE.decodeUtf8With DTEE.lenientDecode x

finalFilename :: PhotoDateTime -> String
finalFilename (PhotoDateTime year month day hour minute second (Just e))
    = intercalate "-" [year, month, day] ++ "++" ++ intercalate "-" [hour, minute, second] ++ "-" ++ e ++ ".jpg"
finalFilename (PhotoDateTime year month day hour minute second Nothing)
    = intercalate "-" [year, month, day] ++ "++" ++ intercalate "-" [hour, minute, second] ++ ".jpg"

safeRename :: FilePath -> FilePath -> IO ()
safeRename old new = do exists <- doesFileExist new
                        if exists then safeRename' 0 old new
                                  else do renameFile old new
                                          putStrLn $ old ++ " -> " ++ new
  where
    newName n f = let (f', ext) = splitExtension f in
                    printf "%s_%06d%s" f' n ext

    -- Target exists, so make a new name.
    safeRename' :: Int -> FilePath -> FilePath -> IO ()
    safeRename' n old new = do
        let new' = newName n new

        exists <- doesFileExist new'

        if exists
            then safeRename' (n + 1) old new
            else do renameFile old new
                    putStrLn $ old ++ " -> " ++ new

subsecond :: ParsecT String u Data.Functor.Identity.Identity String
subsecond = do
    char '_'
    many1 (noneOf ".")

{-
samsungPhotoFile parses a filename from my Samsung Galaxy S3, as seen on the
internal storage via sshfs.

A normal file looks like this:

    IMG_20130519_114522216.jpg
-}

samsungPhotoFile :: forall u. ParsecT String u Identity PhotoDateTime
samsungPhotoFile = do
    string "IMG_"

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

manyThenChar :: Stream s m Char => Char -> ParsecT s u m String
manyThenChar c = do
    x <- many (noneOf [c])
    char c
    return x

createdTime :: ParsecT String u Data.Functor.Identity.Identity PhotoDateTime
createdTime = do
    string "Image Created: "

    year  <- manyThenChar ':'
    month <- manyThenChar ':'
    day   <- manyThenChar ' '

    hour   <- manyThenChar ':'
    minute <- manyThenChar ':'
    second <- manyThenChar '\n'

    return $ PhotoDateTime year month day hour minute second Nothing

-- "    date:create: 2016-03-05T15:59:33+08:00"
createdTimeIdentifyRaw :: ParsecT String u Data.Functor.Identity.Identity PhotoDateTime
createdTimeIdentifyRaw = do
    string "    date:create: "

    year  <- manyThenChar '-'
    month <- manyThenChar '-'
    day   <- manyThenChar 'T'

    hour   <- manyThenChar ':'
    minute <- manyThenChar ':'
    second <- manyThenChar '+'

    _ <- manyThenChar ':'
    _ <- manyThenChar '\n'

    return $ PhotoDateTime year month day hour minute second Nothing

-- "    exif:DateTime: 2016:03:04 18:05:25"
createdTimeIdentifyExif :: ParsecT String u Data.Functor.Identity.Identity PhotoDateTime
createdTimeIdentifyExif = do
    string "    exif:DateTime: "

    year  <- manyThenChar ':'
    month <- manyThenChar ':'
    day   <- manyThenChar ' '

    hour   <- manyThenChar ':'
    minute <- manyThenChar ':'
    second <- manyThenChar '\n'

    return $ PhotoDateTime year month day hour minute second Nothing

parseCreatedTimeFromExif :: String -> IO (Maybe PhotoDateTime)
parseCreatedTimeFromExif f = do
    (_, Just hout, _, _) <- liftIO $ createProcess (proc "exiftags" [f]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdOut <- liftIO $ readRestOfHandle hout
    -- stderr <- liftIO $ readRestOfHandle herr  -- FIXME we're ignoring stderr...

    let linesWithTerminatingNewlines = map (++ "\n") (lines stdOut)
        parseBlah = map (parse createdTime "") linesWithTerminatingNewlines
        parseResults = rights parseBlah

    case length parseResults of 1 -> return $ Just $ head parseResults
                                _ -> return Nothing

isImageFile :: String -> Bool
isImageFile f = isJpg || isPNG
  where
    isJpg   = "jpg"  `isSuffixOf` f'
    isPNG   = "png"  `isSuffixOf` f'

    f' = map toLower f

parseCreatedTimeFromIdentify :: String -> IO (Maybe PhotoDateTime)
parseCreatedTimeFromIdentify f =
    if isImageFile f then do (_, Just hout, _, _) <- liftIO $ createProcess (proc "identify" ["-verbose", f]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

                             stdOut <- liftIO $ readRestOfHandle hout
                             -- stderr <- liftIO $ readRestOfHandle herr  -- FIXME we're ignoring stderr...

                             let linesWithTerminatingNewlines = map (++ "\n") (lines stdOut)
                                 parseBlah  = map (parse createdTimeIdentifyRaw  "") linesWithTerminatingNewlines
                                 parseBlah' = map (parse createdTimeIdentifyExif "") linesWithTerminatingNewlines
                                 parseResults = rights $ parseBlah' ++ parseBlah :: [PhotoDateTime]

                             case parseResults of
                                 (x:_)   -> return $ Just x
                                 []      -> return Nothing
                     else return Nothing

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

-- Use imagemagick's 'identify -verbose' to find a date.
newFilenameFromIdentify :: FilePath -> IO (Maybe FilePath)
newFilenameFromIdentify f = do new <- parseCreatedTimeFromIdentify f

                               case new of (Just new') -> return $ Just (joinPath [fst $ splitFileName f, finalFilename new'])
                                           _           -> return Nothing

main :: IO ()
main = do
    files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents "."

    forM_ files (\f -> do -- possibleNewNames <- catMaybes <$> sequence [return $ newFilenameFromSamsung f, newFilenameFromExif f, newFilenameFromIdentify f]
                          possibleNewNames <- doblah f
                          putStrLn $ "Processing: " ++ f ++ " :: " ++ show possibleNewNames
                          if null possibleNewNames then putStrLn $ "skipping " ++ f
                                                   else safeRename f (head possibleNewNames))



  where
    doblah :: FilePath -> IO [FilePath]
    doblah f = do
        let r1 = newFilenameFromSamsung f

        case r1 of
            Just r1'    -> return [r1']
            Nothing     -> do r2 <- newFilenameFromExif f
                              case r2 of
                                Just r2'    -> return [r2']
                                Nothing     -> do r3 <- newFilenameFromIdentify f
                                                  case r3 of
                                                    Just r3'    -> return [r3']
                                                    Nothing     -> return []
