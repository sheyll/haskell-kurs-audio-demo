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

tatue = play [(2, modulate  (square 2) (sine . (+ 660) . (* 220)))]

wooooormmhhh = play [(15, modulate  (sine 40) (sine . (+ 400) ))]

pling = play [(0.5, amp (fade 15) (sine 1200))]

detuned = play [(1, modulate  (fade 10) (sine . (+ 400) . (*200)))]


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
theSong = 2 `times` alleMeineEntchen
  where
    alleMeineEntchenBass =
      Vol 0.3 $
      majorChord (c 4 8 bass) :>:
      majorChord (g 4 4 bass) :>:
      majorChord (c 4 4 bass) :>:
      majorChord (g 4 4 bass) :>:
      majorChord (c 4 4 bass) :>:
      majorChord (f 4 4 bass) :>:
      minorChord (e 4 4 bass) :>:
      majorChord (g 4 4 bass) :>: majorChord (c 4 4 bass) :>: Rest 0.5
    alleMeineEntchen =
      alleMeineEntchenBass :|:
      c 6 1 voice :>:
      d 6 1 voice :>:
      e 6 1 voice :>:
      f 6 1 voice :>:
      g 6 4 voice :>:
      a 6 1 voice :>:
      a 6 1 voice :>:
      a 6 1 voice :>:
      a 6 1 voice :>:
      g 6 4 voice :>:
      a 6 1 voice :>:
      a 6 1 voice :>:
      a 6 1 voice :>:
      a 6 1 voice :>:
      g 6 4 voice :>:
      f 6 1 voice :>:
      f 6 1 voice :>:
      f 6 1 voice :>:
      f 6 1 voice :>:
      e 6 2 voice :>:
      e 6 2 voice :>:
      g 6 1 voice :>:
      g 6 1 voice :>:
      g 6 1 voice :>: g 6 1 voice :>: c 6 4 voice :>: Rest 0.5
    voice =
      signalToInstrument
        (\freq ->
           mix
             (volume 0.4 (amp (fade 0.25) (warmSynth 3.0e-3 freq)))
             (delay 0.2 (volume 0.2 (amp (fade 0.5) (warmSynth 3.0e-5 freq)))))
    bass =
      signalToInstrument
        (\freq -> volume 0.4 (amp (fade 0.15) (mix (sine (freq + 0.2)) (sine freq))))

main :: IO ()
main = play technoNoNoise -- techo -- theRenderedSong
