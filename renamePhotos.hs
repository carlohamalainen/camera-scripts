{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad ( MonadPlus(mzero), forM_, when )
import Data.Char (toLower)
import Data.List (intercalate, isSuffixOf)
import System.Directory
    ( doesFileExist, getDirectoryContents, renameFile )
import System.FilePath.Posix ( splitExtension )
import System.IO ( hIsEOF, Handle )
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

import qualified Data.Text as T
import qualified Control.Applicative as App
import qualified Data.ByteString.Lazy as B

import Control.Applicative

import Control.Lens
import Control.Lens.TH
import Graphics.HsExif (subSecTimeOriginal)

newtype ExifDateTime = ExifDateTime LocalTime
  deriving Show

newtype ExifSubSecDateTime = ExifSubSecDateTime LocalTime
  deriving Show

data ExifTool = ExifTool
    { _exifSourceFile                :: FilePath     -- e.g. "/home/carlo/incoming/IMG_8971.HEIC"
    , _exifDileName                  :: FilePath     -- e.g. "IMG_8971.HEIC"
    , _exifDirectory                 :: FilePath     -- e.g. "/home/carlo/incoming"
    , _exifDateTimeOriginal          :: ExifDateTime
    , _exifSubSecDateTimeOriginal    :: Maybe ExifSubSecDateTime
    }
  deriving Show

$(makePrisms ''ExifDateTime)
$(makePrisms ''ExifSubSecDateTime)
$(makeLenses ''ExifTool)

instance A.FromJSON ExifDateTime where
    parseJSON (String s) = case parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S" (T.unpack s) of
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
        <*> v .: "DateTimeOriginal"
        <*> v .:? "SubSecDateTimeOriginal"
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
isImageFile f = isJpg || isPNG || isHEIC
  where
    isJpg   = "jpg"  `isSuffixOf` f'
    isPNG   = "png"  `isSuffixOf` f'
    isHEIC  = "heic" `isSuffixOf` f'

    f' = map toLower f

main :: IO ()
main = do
    files <- filter isImageFile <$> getDirectoryContents "."

    forM_ files $ \f -> do
        x <- E.parseFileExif f
        let dto = x ^? _Right . to E.getDateTimeOriginal . _Just

        (_, Just hout, _, _) <- createProcess (proc "exiftool" ["-j", f]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
        stdOut <- readRestOfHandle' hout

        let j :: Maybe [ExifTool]
            j = A.decode stdOut

        let exifWithSubsecond = j ^? _Just . _head . exifSubSecDateTimeOriginal . _Just . _ExifSubSecDateTime
            exifWithout       = j ^? _Just . _head . exifDateTimeOriginal               . _ExifDateTime

        let d = exifWithSubsecond <|> exifWithout <|> dto

        -- FIXME both? beside?
        case (dto, exifWithout) of
            (Just dto', Just exifWithout') -> when (dto' /= exifWithout') $
                error $ "Base date mismatch: " ++ show dto' ++ " " ++ show exifWithout'
            _ -> return ()

        case d of
            Nothing -> error f
            Just d' -> do
                let f' = formatTime defaultTimeLocale "%Y-%m-%d++%H-%M-%S%q%z" d' ++ "_" ++ f
                safeRename f f'

-- TODO HsExif doesn't know about PNG files.



