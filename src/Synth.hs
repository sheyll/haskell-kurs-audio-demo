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
volume :: Amplitude -> Signal -> Time -> Value
volume x s t = s   t * x

-- |Limits the signal's amplitude to not leave specified range.
clip :: Amplitude -> Amplitude -> Signal -> Signal
clip low high signal = max low . min high . signal

-- |Controls the amplitude of one signal by value of another signal.
amp :: Signal -> Signal -> Signal
amp x y t = x t * y t

-- | Bend time to change the pitch
pitch :: Amplitude -> Signal -> Signal
pitch factor signal = signal . (* factor)

-- | Apply a function that returns a signal to the value a control signal
modulate :: Signal -> (Value -> Signal) -> Signal
modulate s f t = f (s t) t

addiere10 t = t + 10

multiplire x b = x * b

-- | An ADSR envolope
adsr :: Time -> Time -> Time -> Time -> Amplitude -> Signal
adsr attackTime decayTime sustainTime releaseTime sustainLevel t
  | t >= tAttack && t < tDecay = t / attackTime
  | t >= tDecay && t < tSustain =
    let x = (t - tDecay) / decayTime
    in 1 + x * (sustainLevel - 1)
  | t >= tSustain && t < tRelease = sustainLevel
  | t >= tRelease && t < tOff =
    let x = (t - tRelease) / releaseTime
    in sustainLevel * (1 - x)
  | otherwise = 0
  where
    tAttack = 0
    tDecay = tAttack + attackTime
    tSustain = tDecay + decayTime
    tRelease = tSustain + sustainTime
    tOff = tRelease + releaseTime

-- | Offset all values of a signal
dcOffset :: Amplitude -> Signal -> Signal
dcOffset o f t = f t + o

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

-- | FM Synth
fmSynth :: Frequency -> Frequency -> Value -> Signal
fmSynth fCarrier fModulator modulationDepth t =
  sin (2 * pi * fCarrier * t + modulationDepth * sin (2 * pi * fModulator * t))

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

--isBigger links rechts =
data TagesZeit
  = Morgens
  | Mittags
  | Abends


data Antwort = Ja | Nein Mahlzeit

instance Show Antwort where
  show antwort =
    case antwort of
      Ja               -> "Ja, du bimst eins nicer eater am been. ich hoffe den essen ist lekka vong der tastigkeit her AMK"
      Nein Fruhstuck   -> "du bimst wrong am been!!!!!ELF!!1 du mustt Frthstäkk essen du behindata"
      Nein Mittagessen -> "du bimst wrong am been!!!!!ELF!!1 du mustt Miitag essen du behindata"
      Nein Abendessen  -> "du bimst wrong am been!!!!!ELF!!1 du mustt evenink essen du behindata I BIMS 1 gammmelwain"

data Mahlzeit
  = Fruhstuck
  | Mittagessen
  | Abendessen
  deriving (Eq, Show)

darfIchEsJetztEssen t e =
   let erlaubt = case t of
                  Morgens -> Fruhstuck
                  Mittags -> Mittagessen
                  Abends  -> Abendessen
   in if erlaubt == e then Ja
                      else Nein erlaubt

adsrFmSynth duration freq =
  modulate
    (adsr
       (duration * 0.333)
       (duration * 0.0)
       (duration * 0.333)
       (duration * 0.333)
       1)
    synth
  where
    synth x = volume x (fmSynth freq (freq * 1 / 2 + x) (5))
