module Main where

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
import Music

-- * Example Sounds

tatue = modulate  (square 2) (sine . (+ 660) . (* 220))

wooooormmhhh = modulate  (sine 40) (sine . (+ 400))

pling = amp (fade 15) (sine 1200)

detuned = modulate  (fade 10) (sine . (+ 400) . (*200))


sineSineSineSineSineExample1 = sineSineSineSineSine 140 200 22 0.3 0.05

sineSineSineSineSineExample2 = sineSineSineSineSine 80 100 75 13 100

sineSineSineSineSine fc1 fc2 fm fmm fmmm t =
  sine (fc1 + sineAmped (sineAmped 1 fmm t) fm (sineAmped fc2 fmmm t)) t
  where
    sineAmped amplitude frequency =
      volume amplitude (sine frequency)

-- * Example Artwork

techno :: [(Time, Signal)]
techno =
  rendr intro : cycle [rendr beats0, rendr beats1, rendr beats2, rendr beats3]
  where
    intro =
      8 `times` crackNoiseLite :>:
      6 `times` crackNoise :>:
      6 `times` (crackNoiseLite :|: crackNoiseHeavy) :>:
      2 `times` percAED crackNoiseHeavy
    crackNoiseLite = Vol 0.3 (Rest (1 / 8) :>: a 4 3 clp)
    crackNoise =
      Vol 0.5 (crackNoiseLite :|: Vol 0.5 (Rest (1 / 8) :>: a 4 1 wrrrt))
    crackNoiseHeavy =
      Vol
        0.5
        (crackNoise :|: Rest (1 / 8) :>: Vol 0.5 (a 4 2 brrst) :>: Rest (1 / 8))
    percAED :: Music -> Music
    percAED fm =
      4 `times` (fm :|: am) :>:
      4 `times` (fm :|: em) :>:
      2 `times` ((fm :|: xm) :>: 3 `times` (fm :|: dm))
      where
        am = Rest (1 / 8) :>: a 3 2 wmm :>: Rest (1 / 8)
        em = Rest (1 / 8) :>: e 4 2 wmm :>: Rest (1 / 8)
        xm = Rest (1 / 8) :>: Vol 1.25 (d 4 3 wmm)
        dm =
          Rest (2 / 8) :>: d 3 1 wmm :|:
          Rest (1 / 8) :>: d 3 2 wmm :>: Rest (1 / 8)
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
       (percAED (Vol 0.5 cliqs) :|:
        Rest (4 * 4 / 8) :>: 12 `times` (bassDrum :|: crackNoise)))
    beats1 =
      percAED2 (bassDrum :|: crackNoiseLite) :>:
      (8 `times` distributeOver 3 8 (a 4 1 hht2) (Rest (1 / 8)) :|:
       percAED2 bassDrum)
    beats2 =
      percAED2 simpleBeat :>:
      (16 `times` distributeOver 3 8 (a 4 1 hht2) (Rest (1 / 8)) :|:
       percAED2 simpleBeat :>: percAED2 (simpleBeat :|: Vol 0.75 crackNoise))
    beats3 =
      let blip = distributeOver 5 16 (a 1 1 guuop :|: a 4 1 hht2) (Rest (1 / 8))
      in 8 `times` blip :|:
         percAED simpleBeat :>:
         percAED (simpleBeat :|: Vol 0.25 crackNoiseHeavy :|: Vol 0.5 cliqs)

technoNoNoise :: [(Time, Signal)]
technoNoNoise = cycle [rendr beats0, rendr beats1, rendr beats2]
  where
    percAED :: Music -> Music
    percAED fm =
      4 `times` (fm :|: am) :>:
      4 `times` (fm :|: em) :>:
      2 `times` ((fm :|: xm) :>: 3 `times` (fm :|: dm))
      where
        am = Rest (1 / 8) :>: a 3 2 wmm :>: Rest (1 / 8)
        em = Rest (1 / 8) :>: e 4 2 wmm :>: Rest (1 / 8)
        xm = Rest (1 / 8) :>: Vol 1.25 (d 4 3 wmm)
        dm =
          Rest (2 / 8) :>: d 3 1 wmm :|:
          Rest (1 / 8) :>: d 3 2 wmm :>: Rest (1 / 8)
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
      percAED bassDrum :>:
      percAED bassDrum :>:
      (16 `times` distributeOver 3 8 (a 4 1 brrst) (Rest (1 / 8)) :|:
       percAED bassDrum :>:
       (percAED (Vol 0.5 cliqs) :|: Rest (4 * 4 / 8) :>: 12 `times` bassDrum))
    beats1 =
      percAED2 bassDrum :>:
      (8 `times` distributeOver 3 8 (a 4 1 hht2) (Rest (1 / 8)) :|:
       percAED2 bassDrum)
    beats2 =
      let blip = distributeOver 5 16 (a 1 1 guuop :|: a 4 1 hht2) (Rest (1 / 8))
      in 8 `times` blip :|:
         percAED simpleBeat :>: percAED (simpleBeat :|: Vol 0.5 cliqs)


theRenderedSong :: [(Time, Signal)]
theRenderedSong = [rendr theSong]

theSong :: Music
theSong = bpm 90 (times 5 beats)
          -- tempo 0.5
          --   (melody :>:
          --    (melody :|: beats))
  where
    tiefeStimme = progression id id 3
    beats = einTaktKickDrum :|: einTaktHighHats
      where
        einTaktKickDrum = times 4 (schlag bd)
        einTaktHighHats = times 2 (Rest 1 :>: schlag hht2)
        schlag i = Play (1/8) 50 i :>: Rest (7/8)


    progression playerMinor playerMajor o i =
      playerMinor (a o 1 i) :>:
      playerMajor (f o 1 i) :>:
      playerMajor (c o 1 i) :>:
      playerMajor (g o 1 i)


    voice =
      signalToInstrument
        (\freq ->
           mix
             (volume 0.4 (amp (fade 0.25) (warmSynth 3.0e-3 freq)))
             (delay 0.2 (volume 0.2 (amp (fade 0.5) (warmSynth 3.0e-5 freq)))))
    testTon = signalToInstrument sine
    bass =
      signalToInstrument
        (\freq -> volume 0.4 (amp (fade 0.15) (mix (sine (freq + 0.2)) (sine freq))))

















main :: IO ()
main = play theRenderedSong
