module Janik where

import Music
import Playback
import Synth

import Synth
import Playback


theRenderedSong :: [(Time, Signal)]
theRenderedSong = [rendr theSong]

theSong :: Music
theSong =
 bpm 120
   (Vol 0.2 staccatoStimme                                     :>:
    --(Vol 0.2 appStimme :|: Vol 0.6 tiefeStimme)            :>:
    --(Vol 0.2 appStimme :|: Vol 0.6 tiefeStimme :|: beats1) :>:
    (Vol 0.2 appStimme :|: Vol 0.6 tiefeStimme :|: beats2 :|: Vol 0.2 staccatoStimme)
    )

  where
    staccatoStimme = progression (staccato geige) (staccato geige)
    triangleStimme = progression playTriangle (const . Rest . fromIntegral)
      where
        playTriangle d n = schlag (inOktave 7 n) triangle :>: Rest (fromIntegral d - 1)

    tiefeStimme = Vol 0.75 (progression playCello playCello)
      where
        playCello d n = Play (fromIntegral d) (inOktave 4 n) cello


    appStimme = progression moll dur
      where
        moll d n = tempo (1/fromIntegral d) (mollApp (inOktave 6 n) triangle)
        dur d n = tempo (1/fromIntegral d) (durApp (inOktave 6 n) triangle)

    beats1 = times 4 (einTaktKickDrum :|: einTaktHighHats)
    beats2 =
     times 2
      ((einTaktKickDrum :|: einTaktHighHats) :>:
        (einTaktKickDrum :|: einTaktHighHats :|: einTaktTrapHighHat))

    einTaktTrapHighHat = Vol 0.7 (trapHighHat 8 :>: Rest 2 :>: trapHighHat 16)

    einTaktKickDrum = times 4 (schlag 50 bd)

    einTaktHighHats = times 2 (Rest 1 :>: schlag 50 hht2)


    progression :: (Int -> Int -> Music) -> (Int -> Int -> Music) -> Music
    progression playerMinor playerMajor  =
      playerMinor 4 tonF :>:
      playerMajor 4 tonC :>:
      playerMajor 4 tonG :>:
      playerMajor 4 tonA

staccato i d ton = times d (times 2 (Play (1 / 2) (inOktave 7 ton)  i))

schlag n i = Play (1 / 8) n i :>: Rest (7 / 8)

trapHighHat n =
  tempo n (round n `times` schlag 50 hht2)

inOktave o n = n + 12*o

durApp n = app n (n + 4) (n + 7)
mollApp n = app n (n + 3) (n + 7)

app n1 n2 n3 i =
        Play (3/5) n1 i :>: Rest (2/5)
   :|:  Rest (1/5)      :>: Play (3/5) n2 i :>: Rest (1/5)
   :|:                       Rest (2/5)     :>: Play (3/5) n3 i

cello =
  signalToInstrument
    (\freq -> volume 0.8 (amp (fade 0.25) (warmSynth 3.0e-3 freq)))

geige = signalToInstrument x
     where
         x freq = adsrFmSynth 0.3 freq  -- fmSynth freq (freq / 2) pi

voice =
  signalToInstrument
    (\freq ->
       mix
         (volume 0.4 (amp (fade 0.25) (warmSynth 3.0e-3 freq)))
         (delay 0.2 (volume 0.2 (amp (fade 0.5) (warmSynth 3.0e-5 freq)))))

triangle = signalToInstrument (triangleSynth 20)

triangleSynth g f =
    mix
       (amp (fade (1 * g))
            (\t -> 0.9 * (sine f t) + 0.05 * (square (2 * f) t)))
       (amp (fade (0.1 * g))
            (volume 0.5 (sine f)))

bass =
  signalToInstrument
    (\freq -> volume 0.4 (amp (fade 0.15) (mix (sine (freq + 0.2)) (sine freq))))


-- * Example Sounds
