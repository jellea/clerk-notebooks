;; # 'Euclidian' rhythmn exploration

;; So, [euclidian rhythms](http://cgm.cs.mcgill.ca/\~godfried/publications/banff.pdf) have been hype for years now, but after using them for some time the equal spacing doesn't feel so musical for electronic music. Also, having to rely on recursion feels odd. So we explored a more musical variation on euclidian rhythms.

;; Go ahead and evaluate whole document with `cmd+option+enter`

;; On the right side you'll see the results of the code blocks.

;; We start by defining a function to visualise the sequence:

(defn paint-pattern [pattern]
  pattern
  #_(for [x pattern]
      (colorize (if (pos? x) "blue" "yellow")
                (rectangle 20 20))))

;; Test 1!...2...3!...4...5

(paint-pattern [true false true false false])

;; Good, now we define our little spacing algorithm:

(defn space-evenly [fill dur]
  (let [round-down-division (Math/floor (/ dur fill))
        empty-sequence (range fill)
        group-sizes (mapv (fn [group-num]
                            (+ round-down-division
                               (if (<= (inc group-num) (mod dur fill)) 1 0)))
                          empty-sequence)
        groups (mapv (fn [group-size]
                       (cons 1 (repeat (dec group-size) 0)))
                     group-sizes)]
    (flatten groups)))


;; Let's test it....

(space-evenly 5 17)

(count (space-evenly 5 17))

;; And now lets visualise it!

(paint-pattern (space-evenly 5 17))

(paint-pattern (space-evenly 6 17))

(paint-pattern (space-evenly 8 17))

(paint-pattern (space-evenly 9 17))

(paint-pattern (space-evenly 3 17))

;; Damn almost, for a more musical result we'd like the most common groups first. Let's sort them:

(defn space-evenly-and-sorted [fill dur]
  (let [round-down-division (Math/floor (/ dur fill))
        empty-sequence (range fill)
        group-sizes (mapv (fn [group-num]
                            (+ round-down-division
                               (if (<= (inc group-num) (mod dur fill)) 1 0)))
                          empty-sequence)
        frequencies-group-sizes (frequencies group-sizes)
        sorted-group-sizes (-> (sort-by frequencies-group-sizes group-sizes)
                               reverse)
        groups (mapv (fn [group-size]
                       (cons 1 (repeat (dec group-size) 0))) sorted-group-sizes)]
    (flatten groups)))

(paint-pattern (space-evenly-and-sorted 3 17))

(paint-pattern (space-evenly-and-sorted 9 17))

























;; Way better! Now lets rotate some sequences!

(defn rotate-seq [s n]
  (lazy-cat (drop n s)
            (take n s)))

(->> (space-evenly-and-sorted 3 17) paint-pattern)

(-> (space-evenly-and-sorted 3 17) (rotate-seq 6) paint-pattern)

























;; Now how can we make these rotations more friendly? Instead of having to rotate 6 we can just rotate 1 and have it snap to the next trigger?

(defn space-evenly-and-sorted-and-rotate [fill dur rotate]
  (let [round-down-division (Math/floor (/ dur fill))
        group-sizes (->> (range fill)
                         (mapv (fn [group-num]
                                 (+ round-down-division
                                    (if (<= (inc group-num) (mod dur fill)) 1 0)))))
        frequencies-group-sizes (frequencies group-sizes)
        sorted-group-sizes (-> (sort-by frequencies-group-sizes group-sizes) reverse)
        rotated-group-sizes (-> sorted-group-sizes (rotate-seq rotate))
        groups (mapv (fn [group-size]
                       (cons 1 (repeat (dec group-size) 0))) rotated-group-sizes)]
    (flatten groups)))

























;; By rotating the group sizes we can easily rotate by groups!

(->> (space-evenly-and-sorted-and-rotate 3 17 0) paint-pattern)





















;; Now let's make it interactive!

;; (defcell rotation 0)

;; (defcell fills 3)

;; (defcell duration 16)

;; (cell (html [:div
;;              [:h2 (str "Fills: " @fills)]
;;              [:input {:type "range"
;;                       :min 1 :max @duration
;;                       :value @fills
;;                       :on-input #(reset! fills (-> % (.-currentTarget) (.-value) int))}]
;;              [:h2 (str "Duration: " @duration)]
;;              [:input {:type "range"
;;                       :min @fills :max 64 :value @duration

;;                       :on-input #(reset! duration (-> % (.-currentTarget) (.-value) int))}]
;;              [:h2 (str "Rotation: " @rotation)]
;;              [:input {:type "range"
;;                       :value @rotation
;;                       :min 0 :step 1 :max (dec @fills)
;;                       :on-input #(reset! rotation (-> % (.-currentTarget) (.-value) int))}]]))

;; (cell (->> (space-evenly-and-sorted-and-rotate @fills @duration @rotation) paint-pattern))

















;; TODO Still breaks at:

(->> (space-evenly-and-sorted-and-rotate 13 19 0) paint-pattern)















;; Make an exception for group-sizes of one at the beginning?
