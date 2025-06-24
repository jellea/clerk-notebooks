^{:nextjournal.clerk/visibility {:code :hide}}
(ns fitting-nested-tuplets
  "A fitting solution"
  (:require [nextjournal.clerk :as clerk]))

;; # Fitting Algo: Nested Tuplets
;; So, let's take a look at solving sequencer problems in a different way: nested tuplets.

;; Say we have a sequence of 16 steps
(def sequence-a [1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0])

;; If we were to change the division from 16 to 15, we could simply change the sequence by dropping one

(-> sequence-a drop-last vec)

;; But what if we don't actually want to change the sequence and we distribute the tempo within the sequence instead?
;; So we keep `sequence-a` as is and add another vector which is just meant for timing. So if both division and sequence length are 16, we got 16 whole steps.

(let [timing-a [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]]
  {:sequence-steps (count timing-a)
   :divisions (reduce + timing-a)})

;; In another example where we would have 15 division and 16 sequencer steps we would have two half steps, but still a total of 16 steps.
(let [timing-a-alt [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1/2 1/2]]
  {:sequence-steps (count timing-a-alt)
   :divisions (reduce + timing-a-alt)})

;; Now the big question is where we lay out these half steps? How do we distribute these tempo changes?
