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

import Control.Concurrent
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
                 One
               | Two { raw   :: Bool }

commandLineParser :: ParserInfo Options
commandLineParser = info (helper <*> optionsParser) fullDesc

optionsParser :: Parser Options
optionsParser = Options <$> parseSocket
                        <*> parseDebug
                        <*> parseComponents
  where
    parseSocket = strOption $
        long "socket" `mappend`
        short 's' `mappend`
        metavar "SOCKET" `mappend`
        value "/var/run/docker.sock" `mappend`
        showDefault `mappend`
        help "Location of Docker remote control API endpoint"

    parseDebug = switch $
        long "debug" `mappend`
        short 'd' `mappend`
        help "Output lots of debugging information"

    parseComponents = subparser
       (parseOneComponent `mappend`
        parseTwoComponent)

    parseOneComponent =
        componentHelper "one" (pure One) "Number 1"

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

    -- Although none of the components are running in the background, we get off
    -- of the main thread so that we can block the main thread on the quit
    -- semaphore, such that a user interrupt will kill the program.

    forkIO $ do
        case component of
            One ->
                undefined
            Two raw ->
                undefined
        putMVar quit ()

    takeMVar quit
    debugM "Main.main" "End"
