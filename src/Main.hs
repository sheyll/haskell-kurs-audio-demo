import           Data.Binary          (encode)
import qualified Data.ByteString.Lazy as BS (concat, putStr, ByteString)
import           Data.Function
import           Data.Int             (Int16)
import           Data.List            (unfoldr)
import           System.Random

type Amplitude = Double
type Time = Double
type Frequency = Double
type Signal = Time -> Amplitude


-- |A sound from outer space.
silence :: Signal
silence = const 0

-- |Oscillate in a form of a sine wave at 'freq' Hz.
sine :: Frequency -> Signal
sine freq t = sin $ freq * (2.0 * pi) * t

-- |Square wave at 'freq' Hz.
square :: Frequency -> Signal
square freq t = if odd i then 1.0 else -1.0
    where (i, _) = properFraction (t * freq)
          i :: Integer

-- |Multiplies the signal by a fixed value.
volume :: Amplitude -> Signal -> Signal
volume x s t = s t * x

-- |Mixes two signals together by adding amplitudes.
mix :: Signal -> Signal -> Signal
mix x y t = x t + y t

-- |Mixes several signals together by adding amplitudes.
mixMany :: [Signal] -> Signal
mixMany signals = foldr mix silence signals

-- |Limits the signal's amplitude to not leave specified range.
clip :: Amplitude -> Amplitude -> Signal -> Signal
clip low high s = max low . min high . s

-- |Controls the amplitude of one signal by value of another signal.
amp :: Signal -> Signal -> Signal
amp x y t = x t * y t

-- |Emits a control signal for an exponential fade out.
fade :: Time -> Signal
fade speed = exp . (* speed) . (* (-1.0))

-- |Emits a control signal for an exponential fade out.
unfade :: Time -> Signal
unfade speed = (1-) . fade speed

-- |Plays a fading note with given waveform.
fadingNote :: Int -> (Time -> Signal) -> Double -> Signal
fadingNote n wave fadeSpeed = amp (fade fadeSpeed) (wave (midiNoteToFreq n))

-- |Plays a note with a nice timbre. Mixes slowly fading square wave with rapidly fading sine.
niceNote :: Int -> Signal
niceNote n = mix voice1 voice2
    where
        voice1 = volume 0.3 $ fadingNote n square 1.9
        voice2 = volume 0.7 $ fadingNote n sine 4.0

-- |Delays a signal by a given time.
delay :: Time -> Signal -> Signal
delay delayTime s t =
    if t >= delayTime
        then s (t - delayTime)
        else 0.0

-- |Mixes given notes into a single chord signal.

warmSynth :: Double -> Frequency -> Signal
warmSynth delta f' =
    mixMany
        [ volume 0.5 (mix (sine f) (sine (f * (1 + delta))))
        , volume 0.4 (mix (sine (2 * f)) (sine (2 * f * (1 + delta))))
        , volume 0.05 (mix (square f) (square (f * (1 + delta))))]
  where
    f = f' * 0.5

noise :: Signal
noise t = randomRs (-1,1) (mkStdGen 0) !! (round (t * 44100))

-- * Instruments!

-- |Calculates an oscillation frequency for a MIDI note number, in an equally tempered scale.
midiNoteToFreq :: (Floating a) => Int -> a
midiNoteToFreq n =
    f0 * (aF ** (fromIntegral n - midiA4))
    where
        aF = 2 ** (1.0 / 12.0)
        f0 = 440.0 -- A-4 in an ETS is 440 Hz.
        midiA4 = 69 -- A-4 in MIDI is 69.

noteToMidiNote :: Int -> Octave -> Int
noteToMidiNote t o = t +12 * o

type Instrument = Int -> Signal

signalToInstrument :: (Frequency -> Signal) -> Instrument
signalToInstrument = (.midiNoteToFreq)

type Octave = Int

c, cis, d, dis, e, f, fis, g, gis, a, ais, b :: Octave -> Time -> Instrument -> Music
c o t = Play (t/8) $ noteToMidiNote 0 o
cis o t = Play (t/8) $ noteToMidiNote 1 o
d o t = Play (t/8) $ noteToMidiNote 2 o
dis o t = Play (t/8) $ noteToMidiNote 3 o
e o t = Play (t/8) $ noteToMidiNote 4 o
f o t = Play (t/8) $ noteToMidiNote 5 o
fis o t = Play (t/8) $ noteToMidiNote 6 o
g o t = Play (t/8) $ noteToMidiNote 7 o
gis o t = Play (t/8) $ noteToMidiNote 8 o
a o t = Play (t/8) $ noteToMidiNote 9 o
ais o t = Play (t/8) $ noteToMidiNote 10 o
b o t = Play (t/8) $ noteToMidiNote 11 o


bd :: Instrument
bd n t = clip (-1) 1 (sine (midiNoteToFreq n * (fade 10 t'))) t'
  where
    t' = t * 8

clp :: Instrument
clp _ =  square 8

hht :: Instrument
hht _ =  clip (-0.3) 0.3 (amp (fade 4) noise)

hht2 :: Instrument
hht2 _ =  clip (-0.3) 0.3 (amp (fade 100) noise)

sinc :: Instrument
sinc =
    signalToInstrument
        (\freq t ->
              let x = freq * 2 * t
              in sin (pi * x) / x)

wmm :: Instrument
wmm n =  clip (-1) 1 (amp (unfade 15) (sine (midiNoteToFreq n)))

wrrrt :: Instrument
wrrrt _ = volume 0.4 (mix (square 9.01) (square 9))

brrst :: Instrument
brrst _ = volume 0.7 ((square 4) . (mix (const 2) (sine 3)))

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
    | (:|:) Music
            Music
    | (:>:) Music
            Music

infixl 2 :|:
infixr 3 :>:

transposeMusic :: Int -> Music -> Music
transposeMusic n m =
    case m of
        mL :|: mR -> transposeMusic n mL :|: transposeMusic n mR
        m1 :>: m2 -> transposeMusic n m1 :>: transposeMusic n m2
        Vol v m' -> Vol v (transposeMusic n m')
        Play t note i -> Play t (note+n) i
        Rest t -> Rest t

renderMusic :: Music -> Signal
renderMusic score t =
    case score of
        mL :|: mR ->
            mix (renderMusic mL) (renderMusic mR) t
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
        a 4 0.5 sinc :>: Rest (1 / 16)
    :>: a 5 0.5 sinc :>: Rest (1 / 16)
    :>: d 4 0.5 sinc :>: Rest (1 / 16)
    :>: e 5 0.5 sinc :>: Rest (1 / 16)

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


-- * Example Artwork


theMusic :: [(Time, Signal)]
theMusic = -- rendr theSong : techno
    techno

techno :: [(Time, Signal)]
techno =
     rendr intro :
     cycle [rendr beats0, rendr beats1, rendr beats2, rendr beats3]
  where
    intro =
        8 `times` crackNoiseLite :>: 6 `times` crackNoise :>: 6 `times`
        (crackNoiseLite :|: crackNoiseHeavy) :>:
        2 `times` (percAED crackNoiseHeavy)
    crackNoiseLite = Rest (1 / 8) :>: a 4 3 clp
    crackNoise = crackNoiseLite :|: Rest (1 / 8) :>: a 4 1 wrrrt
    crackNoiseHeavy =
        crackNoise :|: Rest (1 / 8) :>: a 4 2 brrst :>: Rest (1 / 8)
    percAED :: Music -> Music
    percAED fm =
        4 `times` (fm :|: am) :>: 4 `times` (fm :|: em) :>: 2 `times`
        ((fm :|: xm) :>: 3 `times` (fm :|: dm))
      where
        am = Rest (1 / 8) :>: a 3 2 wmm :>: Rest (1 / 8)
        em = Rest (1 / 8) :>: e 4 2 wmm :>: Rest (1 / 8)
        xm = Rest (1 / 8) :>: Vol 1.25 (d 4 3 wmm)
        dm =
            Rest (2 / 8) :>: d 3 1 wmm :|: Rest (1 / 8) :>: d 3 2 wmm :>:
            Rest (1 / 8)
    percAED2 :: Music -> Music
    percAED2 fm =
        16 `times` fm :|:
        (2 `times`
         distributed (replicate 5 $ beat (a 4)) (replicate 3 (Rest (1 / 8)))) :>:
        (2 `times`
         distributed (replicate 5 $ beat (f 5)) (replicate 3 (Rest (1 / 8)))) :>:
        2 `times`
        (distributed (replicate 5 $ beat (d 4)) (replicate 3 (Rest (1 / 8))) :>:
         distributed
             [beat (d 4), beat (d 4), beat (d 4), beat (d 4), beat (d 5)]
             (replicate 3 (Rest (1 / 8))))
      where
        beat note = Vol 0.8 (note 1 (signalToInstrument (warmSynth 0.5)))
    beats0 =
        percAED (bassDrum :|: crackNoiseLite) :>:
        percAED bassDrum :>:
        (16 `times` distributeOver 3 8 (a 4 1 brrst) (Rest (1 / 8)) :|:
         percAED bassDrum :>:
         (percAED cliqs :|: Rest (4 * 4 / 8) :>: 12 `times`
          (bassDrum :|: crackNoise)))
    beats1 =
        percAED2 (bassDrum :|: crackNoiseLite) :>:
        (8 `times` distributeOver 3 8 (a 4 1 hht2) (Rest (1 / 8)) :|:
         percAED2 bassDrum)
    beats2 =
        percAED2 simpleBeat :>:
        (16 `times` distributeOver 3 8 (a 4 1 hht2) (Rest (1 / 8)) :|:
         percAED2 simpleBeat :>:
         percAED2 (simpleBeat :|: Vol 0.75 crackNoise))
    beats3 =
        let blip =
                distributeOver 5 16 (a 1 1 guuop :|: a 4 1 hht2) (Rest (1 / 8))
        in 8 `times` blip :|: percAED simpleBeat :>:
           percAED (simpleBeat :|: Vol 0.25 crackNoiseHeavy :|: Vol 0.5 cliqs)

theSong :: Music
theSong = 2 `times` alleMeineEntchen
  where
    alleMeineEntchenBass =
        Vol 0.3 $
        majorChord (c 4 8 bass) :>: majorChord (g 4 4 bass) :>:
        majorChord (c 4 4 bass) :>:
        majorChord (g 4 4 bass) :>:
        majorChord (c 4 4 bass) :>:
        majorChord (f 4 4 bass) :>:
        minorChord (e 4 4 bass) :>:
        majorChord (g 4 4 bass) :>:
        majorChord (c 4 4 bass) :>:
        Rest 0.5
    alleMeineEntchen =
        alleMeineEntchenBass :|: (c 6 1 voice) :>: (d 6 1 voice) :>:
        (e 6 1 voice) :>:
        (f 6 1 voice) :>:
        (g 6 4 voice) :>:
        (a 6 1 voice) :>:
        (a 6 1 voice) :>:
        (a 6 1 voice) :>:
        (a 6 1 voice) :>:
        (g 6 4 voice) :>:
        (a 6 1 voice) :>:
        (a 6 1 voice) :>:
        (a 6 1 voice) :>:
        (a 6 1 voice) :>:
        (g 6 4 voice) :>:
        (f 6 1 voice) :>:
        (f 6 1 voice) :>:
        (f 6 1 voice) :>:
        (f 6 1 voice) :>:
        (e 6 2 voice) :>:
        (e 6 2 voice) :>:
        (g 6 1 voice) :>:
        (g 6 1 voice) :>:
        (g 6 1 voice) :>:
        (g 6 1 voice) :>:
        (c 6 4 voice) :>:
        Rest 0.5
    voice =
        signalToInstrument
            (\f ->
                  mix
                      (volume 0.4 (amp (fade 0.25) (warmSynth 3.0e-3 f)))
                      (delay
                           0.2
                           (volume 0.2 (amp (fade 0.5) (warmSynth 3.0e-5 f)))))
    bass =
        signalToInstrument
            (\f ->
                  volume 0.4 (amp (fade 0.15) (mix (sine (f + 0.2)) (sine f))))

-- * Rendering

-- |Samples the signal over a specified time range with given sample rate.
renderBuffers :: [(Time, Signal)] -> [BS.ByteString]
renderBuffers = unfoldr renderNext
  where
    sampleRate = 44100
    renderNext [] = Nothing
    renderNext ((buffDur, s):parts) = Just (buf, parts)
      where
        buf = BS.concat $ map encode (render (0, buffDur) sampleRate s)


-- |Samples the signal over a specified time range with given sample rate.
render :: (Time, Time) -> Int -> Signal -> [Int16]
render (startT,endT) sampleRate s =
        [int16signal (sample * samplePeriod) | sample <- [0 .. totalSamples]]
  where
    int16signal = (toInt16 . clipped) -- a function of Time -> Int16
    toInt16 x = (truncate (minSig + ((x + 1.0) / 2.0 * (maxSig - minSig))))
    minSig = fromIntegral (minBound :: Int16)
    maxSig = fromIntegral (maxBound :: Int16)
    clipped = (clip (-1.0) 1.0 s) -- the same signal clipped to stay within [-1; 1]
    totalSamples = (endT - startT) / samplePeriod -- total number of samples to render
    samplePeriod = 1.0 / (fromIntegral sampleRate) -- time interval between two sample points
main :: IO ()
main = do
    let buffers = renderBuffers theMusic
    mapM_ BS.putStr buffers
