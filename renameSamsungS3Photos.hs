import Control.Applicative hiding ((<|>),many)
import Control.Monad (forM_)
import Control.Proxy
import Data.Functor.Identity
import Data.List (intercalate)
import System.Directory
import System.FilePath.Posix
import Text.Parsec

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

subsecond :: ParsecT String u Data.Functor.Identity.Identity (String)
subsecond = do
    char '_'
    s <- many (noneOf ".")
    return s

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

finalFilename (PhotoDateTime year month day hour minute second (Just e))
    = (intercalate "-" [year, month, day]) ++ "++" ++ (intercalate "-" [hour, minute, second]) ++ "-" ++ e ++ ".jpg"
finalFilename (PhotoDateTime year month day hour minute second Nothing)
    = (intercalate "-" [year, month, day]) ++ "++" ++ (intercalate "-" [hour, minute, second]) ++ ".jpg"

main = do
    files <- getRecursiveContentsList "."

    forM_ files (\f -> do let f' = finalFilename <$> parse samsungPhotoFile "" (snd $ splitFileName f)
                          case f' of (Right f'')    -> do renameFile f (joinPath [fst $ splitFileName f, f''])
                                                          putStrLn $ f ++ " -> " ++ f''
                                     (Left err)     -> putStrLn $ "ignoring: " ++ f)
