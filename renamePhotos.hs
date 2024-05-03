{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-cse #-}

import Control.Monad ( MonadPlus(mzero), forM_, when )
import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import System.Directory
    ( doesFileExist, getDirectoryContents, renameFile )
import System.FilePath.Posix ( splitExtension )
import System.IO ( hIsEOF, Handle, stdout )
import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_in, std_out, std_err),
      StdStream(CreatePipe) )
import Text.Printf (printf)
import Data.Time.Format
    ( formatTime, defaultTimeLocale, parseTimeM )

import qualified Data.ByteString as BS
import qualified Data.Map as DM
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding.Error as DTEE
import Data.Time.LocalTime ( LocalTime )

import qualified Graphics.HsExif as E

import qualified Data.Aeson as A
import           Data.Aeson (Value(..), (.:), (.:?))
import           Data.Aeson.Lens (key, _Array, _String)

import qualified Data.Text as T
import qualified Control.Applicative as App
import qualified Data.ByteString.Lazy as B

import Control.Applicative

import Control.Lens

import Control.Lens.TH
import Graphics.HsExif (subSecTimeOriginal)

import qualified System.Console.CmdArgs as CmdArgs

import qualified Data.Time as Time
import qualified Data.Time.Format as Time
import Data.Aeson.Types (parse)

newtype Args = Args { dryRun :: Bool }
  deriving (Show, CmdArgs.Data, CmdArgs.Typeable)

newtype ExifDateTime = ExifDateTime LocalTime
  deriving Show

newtype ExifSubSecDateTime = ExifSubSecDateTime LocalTime
  deriving Show

data ExifTool = ExifTool
    { _exifSourceFile               :: FilePath     -- e.g. "/home/carlo/incoming/IMG_8971.HEIC"
    , _exifDileName                 :: FilePath     -- e.g. "IMG_8971.HEIC"
    , _exifDirectory                :: FilePath     -- e.g. "/home/carlo/incoming"
    , _exifDateTimeOriginal         :: Maybe ExifDateTime
    , _exifSubSecDateTimeOriginal   :: Maybe ExifSubSecDateTime
    , _exifCreationDate             :: Maybe ExifDateTime
    , _exifFileModifyDate           :: Maybe ExifDateTime
    }
  deriving Show

$(makePrisms ''ExifDateTime)
$(makePrisms ''ExifSubSecDateTime)
$(makeLenses ''ExifTool)

instance A.FromJSON ExifDateTime where
    parseJSON (String s) = let d0 = parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S%z" (T.unpack s)
                               d1 = parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S"   (T.unpack s)
                            in case d0 <|> d1 of
                                Nothing -> fail $ "Couldn't parse PhotoDateTime <<<" ++ T.unpack s ++ ">>>"
                                Just x  -> pure $ ExifDateTime x
    parseJSON _ = mzero

instance A.FromJSON ExifSubSecDateTime where
    parseJSON (String s) = case parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S%Q%z" (T.unpack s) of
                            Nothing -> fail $ "Couldn't parse PhotoDateTime <<<" ++ T.unpack s ++ ">>>"
                            Just x  -> pure $ ExifSubSecDateTime x
    parseJSON _ = mzero

instance A.FromJSON ExifTool where
    parseJSON (Object v) = ExifTool
        <$> v .: "SourceFile"
        <*> v .: "FileName"
        <*> v .: "Directory"
        <*> v .:? "DateTimeOriginal"
        <*> v .:? "SubSecDateTimeOriginal"
        <*> v .:? "CreationDate"
        <*> v .:? "FileModifyDate"
    parseJSON _ = mzero

readRestOfHandle' :: Handle -> IO B.ByteString
readRestOfHandle' handle = do
    ineof <- hIsEOF handle
    if ineof
        then return ""
        else B.hGetContents handle

safeRename :: FilePath -> FilePath -> IO ()
safeRename old new = do exists <- doesFileExist new
                        if exists then safeRename' 0 old new
                                  else do renameFile old new
                                          putStrLn $ old ++ " 1-> " ++ new
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
            else do renameFile old new'
                    putStrLn $ old ++ " 2-> " ++ new'

isImageFile :: String -> Bool
isImageFile f = prefix f && (is "gif" || is "jpg" || is "jpeg" || is "png" || is "heic" || is "mov" || is "mp4" || is "webp")
  where
    prefix f = "IMG" `isPrefixOf` f || head f `elem` ['A'..'Z'] -- someone makes weird files like TRJQ8200.JPG
    is ext = ext `isSuffixOf` map toLower f

imageMagickCreatedTime :: FilePath -> IO (Maybe LocalTime)
imageMagickCreatedTime f = do
    let f' = f <> "[1x1+0+0]" -- https://www.imagemagick.org/discourse-server/viewtopic.php?t=30987

    (_, Just hout, _, _) <- createProcess (proc "convert" [f', "json:"]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    stdOut <- readRestOfHandle' hout

    let json :: Either String Value
        json = A.eitherDecode stdOut

    -- [0].image.properties["date:create"]
    return $ json ^? _Right
            . _Array . ix 0 . key "image" . key "properties" . key "date:create" . _String
            . to T.unpack
            . to (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z")
            . _Just

main :: IO ()
main = do
    args <- CmdArgs.cmdArgs $ Args
                { dryRun = False CmdArgs.&= CmdArgs.explicit CmdArgs.&= CmdArgs.name "dry-run" CmdArgs.&= CmdArgs.help "Dry run, don't rename files."
                }

    files <- filter isImageFile <$> getDirectoryContents "."

    print files
    forM_ files $ \f -> do
        x <- E.parseFileExif f
        let dto = x ^? _Right . to E.getDateTimeOriginal . _Just

        dateCreated <- imageMagickCreatedTime f

        (_, Just hout, _, _) <- createProcess (proc "exiftool" ["-j", f]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
        stdOut <- readRestOfHandle' hout

        let j :: Maybe [ExifTool]
            j = A.decode stdOut

        let exifWithSubsecond = j ^? _Just . _head . exifSubSecDateTimeOriginal . _Just . _ExifSubSecDateTime
            exifWithout       = j ^? _Just . _head . exifDateTimeOriginal       . _Just . _ExifDateTime
            created           = j ^? _Just . _head . exifCreationDate           . _Just . _ExifDateTime -- Apple MOV files have these
            modified          = j ^? _Just . _head . exifFileModifyDate         . _Just . _ExifDateTime -- Fallback for things like Strava-share images.

        let d = exifWithSubsecond
             <|> exifWithout
             <|> dto
             <|> created
             <|> modified
             <|> dateCreated

        -- Making sure that Graphics.HsExif parses the same date as the external Perl program exiftools.
        case sequenceA [dto, exifWithout] of
            Just [dto', exifWithout'] -> when (dto' /= exifWithout') $
                error $ "Base date mismatch: " ++ show dto' ++ " " ++ show exifWithout'
            _ -> return ()

        case d of
            Nothing -> do
                print stdOut
                print j
                error f
            Just d' -> do
                let f' = formatTime defaultTimeLocale "%Y-%m-%d++%H-%M-%S%q%z" d' ++ "_" ++ f
                    (f0, ext) = splitExtension f'

                    f'' = f0 ++ map toLower ext

                if dryRun args
                    then putStrLn $ "DRY RUN " ++ f ++ "\t\t=>\t\t" ++ f''
                    else safeRename f f''

-- TODO HsExif doesn't know about PNG and MOV files.