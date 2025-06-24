;; ## Tempo network/modulation exploration

;; *Want to combine techno at 140BPM and drum and bass at 180BPM in one track? Here's how to with some math!* 🤯

;; Tempo networks are a way to jump between tempi by keeping a musical ratio as a relationship. The pivot is a common durational unit (pick one: reference tempo, pulse, division or duration).

;; Reference paper: *Benadon, 2004 "Towards a Theory of Tempo Modulation"*

;; Let's start with some utilities, you can ignore the next code block for now:

(defn truncate-decimals
  "Truncate decimals so we can match irrational numbers easily"
  [num decimals]
  (->> num (with-precision decimals) (float)))

;; To calculate a tuplet pulse (16th note interval in ms) we provide the relationship of a tempo in BPM, a division in 16ths and a duration in 16ths.

(defn get-tuplet-pulse-16th-ms
  "Calculates the pulse/interval in ms for a tuplet"
  [tempo-bpm division-16th duration-16th]
  (-> (/ 60000 tempo-bpm) ; bpm to quarter notes in ms
      (/ 4)               ; 16ths in ms  
      (* duration-16th) ; multiply by the duration to get the ms length of a cycle
      (/ division-16th)))

;; divide by division to get the tuplet pulse in ms

;; Some examples, output is the pulse interval in millisecond, feel free to play with the numbers to get a feel for it. You can re-run the code in the block by hitting *cmd+enter*

(float (get-tuplet-pulse-16th-ms 140 16 16))

(float (get-tuplet-pulse-16th-ms 140 11 12))

;; To pivot across the matrix we calculate all the note values for all divisions and durations, so we can easily step up and down. 

(def note-value->div-dur
  "Generate a note-value matrix, for all divs and durs"
  (->> (for [div (range 32 0 -1)
             dur (range 32 0 -4)
             :let [note-value (-> (/ dur div) (truncate-decimals 4))]]
         {note-value [div dur]})
       (into (sorted-map-by <))))


;; This function allows you to find the next tuplet pair by increasing and decreasing the tempi. You choose to pivot either over the common tempo or the shared pulse. Pivotting over division and duration is still a work in progress.


(defn index-of [coll item]
  (count (take-while (partial not= item) coll)))

(defn next-tempi [next-fn {:keys [tempo-bpm div dur pulse lock] :as opts}]
  (let [current-note-value (-> (/ dur div) (truncate-decimals 4))
        next-idx (-> (keys note-value->div-dur) (index-of current-note-value) next-fn)
        [new-div new-dur] (nth (vals note-value->div-dur) next-idx)
        new-tempo (* (/ tempo-bpm (* dur new-div)) (* new-dur div))
        new-pulse (get-tuplet-pulse-16th-ms tempo-bpm new-div new-dur)]
    (condp = lock
      #_#_:division (assoc opts :tempo-bpm new-tempo :dur new-dur :pulse new-pulse)
      #_#_:duration (assoc opts :tempo-bpm new-tempo :div new-div :pulse new-pulse)
      :pulse (assoc opts :tempo-bpm new-tempo :div new-div :dur new-dur)
      :tempo (assoc opts :pulse new-pulse :div new-div :dur new-dur))))


;; If we now increase the tempi and pivot over the *pulse* by locking it, we get our next tempo and new division and duration which match the same pulse.

(next-tempi inc {:tempo-bpm 140
                 :div 4
                 :dur 12
                 :pulse (get-tuplet-pulse-16th-ms 140 4 12)
                 :lock :pulse})


;; The same example, but with the tempo locked:

(next-tempi inc {:tempo-bpm 140
                 :div 4
                 :dur 12
                 :pulse (get-tuplet-pulse-16th-ms 140 4 12)
                 :lock :tempo})

;; To make it easier to play with here's a small interface around the previous algorithm. Imagine increase and decrease as a knob allowing you to accelerate and decelerate. Let's start with defining the starting state and then we draw the UI.

;; (defcell !example {:tempo-bpm 140
;;                    :div 4
;;                    :dur 12
;;                    :pulse (get-tuplet-pulse-16th-ms 140 4 12)
;;                    :lock :pulse})

;; (cell (html [:div
;;              [:div {:style {:background-color "#ddd" :padding 10}}
;;               [:h3 [:i "Tempo "] (str (:tempo-bpm @!example) "bpm")]
;;               [:h3 [:i "Div:Dur "] (str (:div @!example) ":" (/ (:dur @!example) 4) " (pulses:beats)")]
;;               [:h3 [:i "Pulse "] (str (-> (:pulse @!example) (truncate-decimals 7)) "ms")]
;;               [:h3 [:i "Note value "] (-> (/ (:dur @!example) 4 (:div @!example)) (truncate-decimals 4))]]
;;              [:div {:style {:border "black solid 2px" :padding 10}}
;;               [:label "Lock"]
;;               [:select {:on-change #(swap! !example assoc :lock (-> % .-target .-value keyword))}
;;                [:option {:value "tempo" :selected (= (:lock @!example) :tempo)} "Tempo"]
;;                [:option {:value "pulse" :selected (= (:lock @!example) :pulse)} "Pulse"]
;;                [:option {:value "division" :disabled true :selected (= (:lock @!example) :division)} "Division (TODO)"]
;;                [:option {:value "duration" :disabled true :selected (= (:lock @!example) :duration)} "Duration (TODO)"]]
;;               [:br]
;;               [:button {:on-click #(swap! !example (partial next-tempi inc))} "+ Increase"]
;;               [:button {:on-click #(swap! !example (partial next-tempi dec))} "- Decrease"]]]))
