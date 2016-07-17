(defn parse-matrix-from-file [filename]
  (let [split clojure.string/split
        str-values (->> (split (slurp "data/p11.txt") #"\n")
                        (map #(split % #"\s+")))]
    (loop [remaining str-values
           result []]
      (if (empty? remaining)
        result
        (recur (rest remaining) (conj result (map #(Integer/parseInt %) (first remaining))))))))

(defn multiply-groups-of-n [values n]
  (loop [first-n (take n values)
         remaining (rest values)
         totals []]
    (if (= n (count first-n))
      (recur (take n remaining) (rest remaining) (conj totals (apply *' first-n)))
      totals)))

(defn multiply-rows-in-groups-of-n [rows n]
  (map #(multiply-groups-of-n % n) rows))

(defn multiply-columns-in-groups-of-n [rows n]
  (multiply-rows-in-groups-of-n (apply map list rows) n))

(defn vertically-align-diagonal-groups [rows]
  (let [shifted-rows (loop [[first-row & remaining] rows
                            offset 0
                            result []]
                       (if (empty? first-row)
                         result
                         (recur remaining (inc offset) (conj result (drop offset first-row)))))
        shifted-row-length (apply min (map count shifted-rows))]
    (map #(take shifted-row-length %) shifted-rows)))

(defn multiply-diag-left-to-right-groups-of-n [rows n]
  (loop [row-slice (take n rows)
         remaining (rest rows)
         result []]
    (if (= (count row-slice) n)
      (let [valigned-diag-groups (vertically-align-diagonal-groups row-slice)]
        (recur (take n remaining)
               (rest remaining)
               (conj result (multiply-columns-in-groups-of-n valigned-diag-groups n))))
      result)))

(defn multiply-diag-right-to-left-groups-of-n [rows n]
  (multiply-diag-left-to-right-groups-of-n (map reverse rows) n))

(defn solve-problem-11 []
  (let [mtrx (parse-matrix-from-file "doc/p11.txt")
        row-totals (multiply-rows-in-groups-of-n mtrx 4)
        col-totals (multiply-columns-in-groups-of-n mtrx 4)
        diag-ltr-totals (multiply-diag-left-to-right-groups-of-n mtrx 4)
        diag-rtl-totals (multiply-diag-right-to-left-groups-of-n mtrx 4)]
    (apply max (flatten [row-totals col-totals diag-ltr-totals diag-rtl-totals]))))
