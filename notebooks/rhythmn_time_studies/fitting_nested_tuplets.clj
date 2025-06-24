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
   :groove-length (reduce + timing-a)})

;; In another example where we would have 15 division and 16 sequencer steps we would have two half steps, but still a total of 16 steps.
(let [timing [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1/2 1/2]]
  {:sequence-steps (count timing)
   :groove-length (reduce + timing)})

;; Now the big question is where we lay out these half steps? How do we distribute these tempo changes?

;; ## Strategy one: Group by duration - Progressive increase

;; Perhaps more intersting would be to split up the sequence into 4 groups and make them speed up:

;; 3 + 3 + 4 + 5

(defn find-progressive-groups [groove-length-16th seq-length]
  ;; hard coded for now

  ;;[Ta Ki Ta] (3) | [Ta Ki Ta] (3) | [Ta Ka Di Mi] (4) | [Ta Ka Di Mi Na] (5)

  [3  3  4  5])

(defn group-progressive-inc [seq-length groove-length-16th]
  (let [groups (find-progressive-groups groove-length-16th seq-length)
        make-group #(repeat % (/ % (count groups)))
        timing (map make-group groups)]
    (flatten timing)))

(let [timing (group-progressive-inc 16 15)]
  {:sequence-steps (count timing)
   :groove-length (reduce + 0.0 timing)
   :timing timing})

;; ## Strategy two: Fibbonaci - Increase

;; [Ta Ki Ta] (3) | [Ta Ka Di Mi Na] (5) | [Ta Ka Di Mi Ta Ki Ta Ta] (8)

;; 3 + 5 + 8

(let [timing [3/16 3/16 3/16  5/16 5/16 5/16 5/16 5/16
              1/2 1/2 1/2 1/2  1/2 1/2 1/2 1/2]
      timing* (map (partial * 1) timing)]
  {:sequence-steps (count timing*)
   :groove-length (reduce + 0.0 timing*)})

(defn find-groups [time steps]
  [3 5 8])

(defn fibonacci-timing [seq-length groove-length-16th]
  (let [fib-groups (find-groups groove-length-16th seq-length)
        group-length (/ groove-length-16th (count fib-groups))
        make-group #(repeat % (/ group-length %))
        timing (map make-group fib-groups)]
    (flatten timing)))

(let [timing (fibonacci-timing 16 15)]
  {:sequence-steps (count timing)
   :timing timing
   :groove-length (reduce + 0.0 timing)})