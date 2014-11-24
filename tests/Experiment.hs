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

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pipes
import qualified Pipes.Prelude as P
import System.Process.Streaming
import System.Exit
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Void
import Data.Monoid


p :: Producer ByteString IO () -> IO (Either e ByteString)
p input = do
    b <- P.fold f mempty g input
    return (Right b)
  where
    f :: Builder -> ByteString -> Builder
    f acc x = acc `mappend` Builder.fromByteString x

    g :: Builder -> ByteString
    g = Builder.toByteString


s :: Siphon ByteString Void ByteString
s = siphon p

main :: IO ()
main = do
    (code, result) <- execute (pipeo s) (shell "ls -l")
    S.putStr result
    exitWith code
