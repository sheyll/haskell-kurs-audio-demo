module Janik where


import Synth
import Playback
import Music

-- * Example Sounds

tatue = play [(2, modulate  (square 2) (sine . (+ 660) . (* 220)))]

wooooormmhhh = play [(15, modulate  (sine 40) (sine . (+ 400) ))]

pling = play [(0.5, amp (fade 15) (sine 1200))]

detuned = play [(1, modulate  (fade 10) (sine . (+ 400) . (*200)))]


