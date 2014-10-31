--
-- Unshipping Docker
--
-- Copyright © 2014 Operational Dynamics Consulting, Pty Ltd and Others
-- Copyright © 2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Data.Monoid
import Options.Applicative
import System.Log.Logger

import Package (package, version)
import Linux.Program

--
-- Component line option parsing
--

data Options = Options
  { broker    :: String
  , debug     :: Bool
  , component :: Component }

data Component = 
                 Status
               | Two { raw   :: Bool }

(<+>) :: Monoid θ => θ -> θ -> θ
(<+>) = mappend

commandLineParser :: ParserInfo Options
commandLineParser = info (helper <*> optionsParser) fullDesc

optionsParser :: Parser Options
optionsParser = Options <$> parseSocket
                        <*> parseDebug
                        <*> parseComponents
  where
    parseSocket = strOption $
        long "socket" <+>
        short 's' <+>
        metavar "SOCKET" <+>
        value "/var/run/docker.sock" <+>
        showDefault <+>
        help "Location of Docker's remote control API endpoint"

    parseDebug = switch $
        long "debug" <+>
        short 'd' <+>
        help "Output lots of debugging information"

    parseComponents = subparser
       (parseOneComponent <+>
        parseTwoComponent)

    parseOneComponent =
        componentHelper "status" (pure Status) "Get status of currently running containers"

    parseTwoComponent =
        componentHelper "two" readOptionsParser "Takes two to tango"

    componentHelper cmd_name parser desc =
        command cmd_name (info (helper <*> parser) (progDesc desc))



readOptionsParser :: Parser Component
readOptionsParser = Two <$> parseRaw
  where
    parseRaw = switch $
        long "raw"
        <> short 'r'
        <> help "Output values in a raw form (human-readable otherwise)"

--
-- Actual tools
--

--
-- Main program entry point
--

main :: IO ()
main = do
    Options{..} <- execParser commandLineParser

    let level = if debug
        then Debug
        else Quiet

    quit <- initializeProgram (package ++ "-" ++ version) level

    -- Run selected component.
    debugM "Main.main" "Running command"

    -- Although none of the commands are daemons, we get off of the main thread
    -- so that we can block the main thread on the quit semaphore, such that a
    -- user interrupt will kill the program.

    a <- async $ do
        case component of
            Status ->
                putStrLn "Checking status"
            Two _ ->
                undefined

    _ <- wait a
    debugM "Main.main" "End"
