
import Data.Int (Int16 (..)) -- we're going to use Int16 as output signal format
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (concat, putStr)


type Amplitude = Double
type Time = Double
type Signal = Time -> Amplitude


-- |A sound from outer space.
silence :: Signal
silence = const 0

-- |Oscillate in a form of a sine wave at 'freq' Hz.
sine :: Time -> Signal
sine freq t = sin $ freq * (2.0 * pi) * t

-- |Square wave at 'freq' Hz.
square :: Time -> Signal
square freq t = if odd i then 1.0 else -1.0
    where (i, _) = properFraction (t * freq)

-- |Multiplies the signal by a fixed value.
volume :: Amplitude -> Signal -> Signal
volume x s t = s t * x

-- |Mixes two signals together by adding amplitudes.
mix :: Signal -> Signal -> Signal
mix x y t = x t + y t

-- |Mixes several signals together by adding amplitudes.
mixMany :: [Signal] -> Signal
mixMany signals = foldr mix silence signals


-- |Calculates an oscillation frequency for a MIDI note number, in an equally tempered scale.
midiNoteToFreq :: (Floating a) => Int -> a
midiNoteToFreq n =
    f0 * (a ** (fromIntegral n - midiA4))
    where
        a = 2 ** (1.0 / 12.0)
        f0 = 440.0 -- A-4 in an ETS is 440 Hz.
        midiA4 = 69 -- A-4 in MIDI is 69.

-- |Mixes given notes into a single chord signal.
chord1 :: [Int] -> Signal
chord1 notes = mixMany $ map (volume 0.2 . sine . midiNoteToFreq) notes


-- |Limits the signal's amplitude to not leave specified range.
clip :: Amplitude -> Amplitude -> Signal -> Signal
clip low high s = max low . min high . s

-- |Samples the signal over a specified time range with given sample rate.
render :: Time -> Time -> Int -> Signal -> [Int16]
render startT endT sampleRate s =
    [ int16signal (sample * samplePeriod) | sample <- [0..totalSamples] ]
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
    BS.putStr $ playChord [58, 63, 67, 72, 77]
    where
        playChord notes =
            let rendered = render 0.0 3.5 44100 $ chord2 notes
            in BS.concat $ map encode rendered

---------------------------------------------------------------------------

-- |Controls the amplitude of one signal by value of another signal.
amp :: Signal -> Signal -> Signal
amp x y t = x t * y t

-- |Emits a control signal for an exponential fade out.
fade :: Time -> Signal
fade speed = exp . (* speed) . (* (-1.0))

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
delay delayTime s t = if t >= delayTime then s (t - delayTime) else 0.0


chord2 notes = mixMany noteWaves
    where
        noteWaves = zipWith noteInArpeggio [0..] notes
        noteInArpeggio idx n = delay (fromIntegral idx * 0.05) $ niceNote n
