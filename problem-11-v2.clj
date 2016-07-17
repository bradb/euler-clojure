(defn parse-matrix-from-file [filename]
  (let [split clojure.string/split
        str-values (->> (split (slurp filename) #"\n")
                        (map #(split % #"\s+")))]
    (loop [[this-row & remaining] str-values
           result []]
      (if (empty? this-row)
        result
        (recur remaining (conj result (apply vector (map #(Integer. %) this-row))))))))


(defn problem-11-v2 []
  (let [p11-matrix (parse-matrix-from-file "data/p11.txt")]
      (loop [x 0
             y 0
             totals []]
        (if (> x 19)
          (apply max totals)
          (if (> y 19)
            (recur (inc x) 0 totals)
            (let [matrix-slice (fn [x-range y-range]
                                 (->> (map list x-range y-range)
                                      (map #(get-in p11-matrix %))
                                      (remove nil?)))
                  sets-of-4 (filter #(= (count %) 4) [(matrix-slice (repeat x) (range y (+ y 4)))
                                                      (matrix-slice (repeat y) (range x (+ x 4)))
                                                      (matrix-slice (range x (+ x 4)) (range y (+ y 4)))
                                                      (matrix-slice (range x (- x 4) -1) (range y (+ y 4)))])]
              (recur x (inc y) (concat totals (map #(apply *' %) sets-of-4)))))))))
