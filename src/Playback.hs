{-# LANGUAGE OverloadedStrings #-}

module Playback
  ( play
  ) where

import Control.Monad
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS
       (ByteString, concat, hPut)
import Data.Function
import Data.Int (Int16)
import Data.List (unfoldr)
import Synth
import System.Environment
import System.IO
import System.Process

play :: [FiniteSignal] -> IO ()
play fs = do
  args <- getArgs
  let sampleRate =
        case args of
          [sr] -> read sr
          _ -> 44100
      buffers = renderBuffers sampleRate fs
      cp =
        (shell "/home/sven/.nix-profile/bin/play -x -r 44100 -b 16 -c1  -e signed-integer -t raw -")
        {std_in = CreatePipe}
  withCreateProcess
    cp
    (\(Just soxPlayStdIn) _ _ ph -> do
       hSetBuffering soxPlayStdIn (BlockBuffering Nothing)
       mapM_ (BS.hPut soxPlayStdIn) buffers
       hFlush soxPlayStdIn
       hClose soxPlayStdIn
       void (waitForProcess ph))

-- * Rendering

type DiscreteAmplitude = Int16

toDiscreteAmplitude :: Amplitude -> DiscreteAmplitude
toDiscreteAmplitude x =
  truncate (minSig + ((x + 1.0) / 2.0 * (maxSig - minSig)))
  where
    minSig = fromIntegral (minBound :: DiscreteAmplitude)
    maxSig = fromIntegral (maxBound :: DiscreteAmplitude)

-- |Samples the signal over a specified time range with given sample rate.
renderBuffers :: Frequency -> [FiniteSignal] -> [BS.ByteString]
renderBuffers sampleRate = unfoldr renderNext
  where
    renderNext [] = Nothing
    renderNext ((buffDur, signal):parts) = Just (buf, parts)
      where
        buf = BS.concat $ map encode render
        -- |Samples the signal over a specified time range with given sample rate.
        render =
          [int16signal (sample * samplePeriod) | sample <- [0 .. totalSamples]]
          where
            int16signal = toDiscreteAmplitude . clipped -- a function of Time -> Int16
              where
                clipped = clip (-1.0) 1.0 signal -- the same signal clipped to stay within [-1; 1]
            totalSamples = buffDur / samplePeriod -- total number of samples to render
            samplePeriod = 1.0 / sampleRate -- time interval between two sample points
