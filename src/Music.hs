module Music where

import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS
       (ByteString, concat, putStr)
import Data.Function
import Data.Int (Int16)
import Data.List (unfoldr)
import System.Environment
import System.Random

import Synth
import Playback

-- |Plays a fading note with given waveform.
fadingNote :: Int -> (Time -> Signal) -> Double -> Signal
fadingNote n wave fadeSpeed = amp (fade fadeSpeed) (wave (midiNoteToFreq n))

-- |Plays a note with a nice timbre. Mixes slowly fading square wave with rapidly fading sine.
niceNote :: Int -> Signal
niceNote n = mix voice1 voice2
  where
    voice1 = volume 0.3 $ fadingNote n square 1.9
    voice2 = volume 0.7 $ fadingNote n sine 4.0

-- |Mixes given notes into a single chord signal.
warmSynth :: Double -> Frequency -> Signal
warmSynth delta freq' =
  mixMany
    [ volume 0.5 (mix (sine freq) (sine (freq * (1 + delta))))
    , volume 0.4 (mix (sine (2 * freq)) (sine (2 * freq * (1 + delta))))
    , volume 0.05 (mix (square freq) (square (freq * (1 + delta))))
    ]
  where
    freq = freq' * 0.5

-- * Instruments!
-- |Calculates an oscillation frequency for a MIDI note number, in an equally tempered scale.
midiNoteToFreq :: Int -> Frequency
midiNoteToFreq n = f0 * (aF ** (fromIntegral n - midiA4))
  where
    aF = 2 ** (1.0 / 12.0)
    f0 = 440.0 -- A-4 in an ETS is 440 Hz.
    midiA4 = 69 -- A-4 in MIDI is 69.

noteToMidiNote :: Int -> Octave -> Int
noteToMidiNote t o = t + 12 * o

type Instrument = Int -> Signal

signalToInstrument :: (Frequency -> Signal) -> Instrument
signalToInstrument = (. midiNoteToFreq)

type Octave = Int

c, cis, d, dis, e, f, fis, g, gis, a, ais, b :: Octave
                                             -> Time
                                             -> Instrument
                                             -> Music
c o t = Play (t / 8) $ noteToMidiNote 0 o

cis o t = Play (t / 8) $ noteToMidiNote 1 o

d o t = Play (t / 8) $ noteToMidiNote 2 o

dis o t = Play (t / 8) $ noteToMidiNote 3 o

e o t = Play (t / 8) $ noteToMidiNote 4 o

f o t = Play (t / 8) $ noteToMidiNote 5 o

fis o t = Play (t / 8) $ noteToMidiNote 6 o

g o t = Play (t / 8) $ noteToMidiNote 7 o

gis o t = Play (t / 8) $ noteToMidiNote 8 o

a o t = Play (t / 8) $ noteToMidiNote 9 o

ais o t = Play (t / 8) $ noteToMidiNote 10 o

b o t = Play (t / 8) $ noteToMidiNote 11 o

bd :: Instrument
bd n t = clip (-1) 1 (sine (midiNoteToFreq n * fade 10 t')) t'
  where
    t' = t * 8

clp :: Instrument
clp _ = square 8

hht :: Instrument
hht _ = clip (-0.3) 0.3 (amp (fade 4) noise)

hht2 :: Instrument
hht2 _ = clip (-0.3) 0.3 (amp (fade 100) noise)

sinc :: Instrument
sinc =
  signalToInstrument
    (\freq t ->
       let x = freq * 2 * t
       in sin (pi * x) / x)

wmm :: Instrument
wmm n = clip (-1) 1 (amp (unfade 15) (sine (midiNoteToFreq n)))

wrrrt :: Instrument
wrrrt _ = volume 0.4 (mix (square 9.01) (square 9))

brrst :: Instrument
brrst _ = volume 0.3 (square 4 . mix (const 2) (sine 3))

guuop :: Instrument
guuop n = sine (fromIntegral n) . mix (const 10) (sine 5)

---------------------------------------------------------------------------
data Music
  = Rest !Time
  | Play !Time
         !Int
         !Instrument
  | Vol !Amplitude
        !Music
  | Music :|: Music
  | Music :>: Music

infixl 2 :|:

infixr 3 :>:

transposeMusic :: Int -> Music -> Music
transposeMusic n m =
  case m of
    mL :|: mR -> transposeMusic n mL :|: transposeMusic n mR
    m1 :>: m2 -> transposeMusic n m1 :>: transposeMusic n m2
    Vol v m' -> Vol v (transposeMusic n m')
    Play t note i -> Play t (note + n) i
    Rest t -> Rest t

renderMusic :: Music -> Signal
renderMusic score t =
  case score of
    mL :|: mR -> mix (renderMusic mL) (renderMusic mR) t
    m1 :>: m2 ->
      if t < musicDuration m1
        then renderMusic m1 t
        else renderMusic m2 (t - musicDuration m1)
    Play t' n i ->
      if t < t'
        then i n t
        else silence t
    Vol v m -> volume v (renderMusic m) t
    Rest _ -> silence t

musicDuration :: Music -> Time
musicDuration m =
  case m of
    mL :|: mR -> (max `on` musicDuration) mL mR
    m1 :>: m2 -> ((+) `on` musicDuration) m1 m2
    Vol _ m' -> musicDuration m'
    Play t _n _i -> t
    Rest t -> t

rendr :: Music -> (Time, Signal)
rendr m = (musicDuration m, renderMusic m)

tempo :: Time -> Music -> Music
tempo t m =
  case m of
    mL :|: mR -> tempo t mL :|: tempo t mR
    m1 :>: m2 -> tempo t m1 :>: tempo t m2
    Vol v m' -> Vol v (tempo t m')
    Play t' n i -> Play (t' / t) n i
    Rest t' -> Rest (t' / t)

-- * Music fun
times :: Int -> Music -> Music
times 1 m = m
times n m = m :>: times (n - 1) m

bassDrum :: Music
bassDrum = a 4 1 bd :>: Rest (3 / 8)

hihats :: Music
hihats = Rest (2 / 8) :>: a 4 1 hht :>: Rest (1 / 8)

simpleBeat :: Music
simpleBeat = bassDrum :|: hihats

cliqs :: Music
cliqs =
  a 4 0.5 sinc :>:
  Rest (1 / 16) :>:
  a 5 0.5 sinc :>:
  Rest (1 / 16) :>:
  d 4 0.5 sinc :>: Rest (1 / 16) :>: e 5 0.5 sinc :>: Rest (1 / 16)

majorChord :: Music -> Music
majorChord m = m :|: transposeMusic 4 m :|: transposeMusic 7 m

minorChord :: Music -> Music
minorChord m = m :|: transposeMusic 3 m :|: transposeMusic 7 m

distributeOver :: Int -> Int -> Music -> Music -> Music
distributeOver beats bars beatM barM =
  distributed (replicate beats beatM) (replicate (bars - beats) barM)

distributed :: [Music] -> [Music] -> Music
distributed beats bars = foldr1 (:>:) segment
  where
    segment = eucl (map pure beats) (map pure bars)

-- | Euclidean algorithm generates all traditional rhythms given a number of
-- rests and a co-prime number of hits.
eucl :: [[a]] -> [[a]] -> [a]
eucl xs ys
  | null ys = head xs
  | otherwise =
    let xs' = zipWith (++) xs ys
        ys'FromXs = drop (length ys) xs
        ys'FromYs = drop (length xs) ys
        ys' =
          if null ys'FromXs
            then ys'FromYs
            else ys'FromXs
    in eucl xs' ys'
