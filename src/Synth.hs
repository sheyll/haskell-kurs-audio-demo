module Synth where

import Data.Int (Int16)
import System.Random









type Value = Double

type Amplitude = Value

type Time = Value

type Frequency = Value

type Signal = Time -> Value

type FiniteSignal = (Time, Signal)










-- |A sound from outer space.
silence :: Signal
silence = const 0
















-- |Oscillate in a form of a sine wave at 'freq' Hz.
sine :: Frequency -> Signal
sine freq t = sin (freq * (2.0 * pi) * t)

-- |Square wave at 'freq' Hz.
square :: Frequency -> Signal
square freq t =
  if odd i
    then 1.0
    else -1.0
  where
    (i, _) = properFraction (t * freq)
    i :: Integer



-- |Multiplies the signal by a fixed value.
volume :: Amplitude -> Signal -> Signal
volume x s t = s t * x

-- |Limits the signal's amplitude to not leave specified range.
clip :: Amplitude -> Amplitude -> Signal -> Signal
clip low high signal = max low . min high . signal

-- |Controls the amplitude of one signal by value of another signal.
amp :: Signal -> Signal -> Signal
amp x y t = x t * y t

-- | Bend time to change the pitch
pitch :: Amplitude -> Signal -> Signal
pitch factor signal = signal . (* factor)

-- | Use one signal to modify another signal
modulate :: (Time -> Value) -> (Value -> Signal) -> Signal
modulate s f t = f (s t) t

-- | Mix two signals together by adding amplitudes.
mix :: Signal -> Signal -> Signal
mix x y t = x t + y t

-- |Mix several signals together by adding amplitudes.
mixMany :: [Signal] -> Signal
mixMany = foldr mix silence

-- |Emits a control signal for an exponential fade out.
fade :: Time -> Signal
fade speed = exp . (* speed) . (* (-1.0))

-- |Emits a control signal for an exponential fade out.
unfade :: Time -> Signal
unfade speed = (1 -) . fade speed

-- |Delays a signal by a given time.
delay :: Time -> Signal -> Signal
delay delayTime s t =
  if t >= delayTime
    then s (t - delayTime)
    else 0.0

noise :: Signal
noise t = randomRs (-1, 1) (mkStdGen 0) !! round (t * 44100)


samples :: Time -> Time -> Int -> Signal -> [Amplitude]
samples startTime endTime sampleCount signal = map signal sampleTimes
  where
    deltaTime = (endTime - startTime) / fromIntegral sampleCount
    sampleTimes = map sampleIndexToSampleTime [0 .. sampleCount]
      where
        sampleIndexToSampleTime index =
          startTime + deltaTime * fromIntegral index
