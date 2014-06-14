import Control.Monad.Identity
import System.Environment ( getArgs )
import System.FilePath.Posix
import System.IO
import System.Process

go :: [String] -> IO ()
go [f] = do
    unless ('.' `elem` (takeFileName f)) (error $ "file does not have an extension: " ++ show f)

    let webm = (reverse $ tail $ dropWhile (/= '.') (reverse f)) ++ ".webm"

    putStrLn $ "ffmpeg -i " ++  f ++ " -qscale 0 " ++ webm

go _ = do
    putStrLn "Usage:"
    putStrLn ""
    putStrLn "    runhaskell transcodeToWebm.hs foo.mov"
    putStrLn ""

main :: IO ()
main = getArgs >>= go

