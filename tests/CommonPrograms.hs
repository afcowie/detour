--
-- Unshipping Docker
--
-- Copyright © 2014 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

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
interpretDebug :: Free Command a -> IO a
interpretDebug (Pure r) = return r
interpretDebug (Free x) = case x of
    List path k -> do
                    putStrLn ("I would list " ++ path)
                    interpretDebug k

    Touch path k -> do
                    putStrLn ("I would touch " ++ path)
                    interpretDebug k

    Help k      -> putStrLn "Help for all" >> interpretDebug k

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
main = interpretDebug directoryListing

