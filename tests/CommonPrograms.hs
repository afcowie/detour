{-# LANGUAGE DeriveFunctor #-}

import System.Exit
import Control.Monad.Free
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

data Command x
  = List FilePath x
  | Touch FilePath x
  | Help x
  | Exit Int
  deriving Functor

ls :: FilePath -> Free Command ByteString
ls path = liftF $ List path (S.pack path)

touch :: FilePath -> Free Command ()
touch path = liftF $ Touch path ()

exit :: Int -> Free Command Int
exit code = liftF $ Exit code

help :: Free Command ()
help = liftF $ Help ()

-- Collapse our IOFree DSL into IO monad actions.
interpretActual :: Free Command a -> IO a
interpretActual (Pure r) = return r
interpretActual (Free x) = case x of
    List path k -> do
                    putStrLn ("I would list " ++ path)
                    interpretActual k

    Touch path k -> do
                    putStrLn ("I would touch " ++ path)
                    interpretActual k

    Help k      -> putStrLn "Help for all" >> interpretActual k

    Exit code   -> case code of 
                    0    -> exitWith ExitSuccess
                    _    -> exitWith (ExitFailure code)

directoryListing :: Free Command ()
directoryListing = do
    ls "."
    touch "./junk"
    exit 1
    help

main :: IO ()
main = interpretActual directoryListing

