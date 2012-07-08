(defn solve-euler-problem-9
  []
  (loop [b (- (/ 1000 2) 1)
         a 1]
    (if (> b 1)
      (let [c (- 1000 (+ a b))]
        (cond
          (= (+ (Math/pow a 2) (Math/pow b 2)) (Math/pow c 2))
            (* a b c)
          (and (> (- b a) 1)
               (> (- 1000 (+ b (inc a))) b))
            (recur b (inc a))
          true
            (recur (dec b) 1))))))

(println "Solution to Euler problem #9:" (solve-euler-problem-9))
