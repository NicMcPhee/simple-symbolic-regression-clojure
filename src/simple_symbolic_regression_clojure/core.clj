(ns simple-symbolic-regression-clojure.core
  (:use [clojure.math.numeric-tower])
  (:use [simple-symbolic-regression-clojure.interpreter]
        [simple-symbolic-regression-clojure.gp])
  )

;;; Main

; We eventually want to make this more flexible instead of having Sine
; wired in as the target, but this will do for the class.

(def sine-rubrics
  (repeatedly 100
    #(let [x (rand (* 2 Math/PI))]
      (->Rubric {:x x} (Math/sin x)))))


(def random-sine-guess (fn [] (random-individual token-generator 100)))


(def initial-sine-population
  (score-population (random-population 500 random-sine-guess) sine-rubrics))


(defn print-generation-report
  [population]
;;   (println (str "Min error: "
;;                 (let [winner (apply min-key get-score population)]
;;                   [(get-score winner) (:script winner)]))))
  (println (:script (first population)))
  (println (map get-score (take 10 population))))


(defn -main
  "Run the system against the Sine rubrics"
  [& args]
  (dorun
   (map print-generation-report
        (take 1000 (future-history initial-sine-population 0.05 sine-rubrics))))
  ; The shutdown-agents call is necessary to make sure that things like "lein run"
  ; terminate when they're done. Otherwise they can hang around waiting for other
  ; threads to finish.
  (shutdown-agents))
