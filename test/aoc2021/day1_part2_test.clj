(ns aoc2021.day1-part2-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc2021.common :as c]))
(def input
  (c/parse-input "resources/input-day1.txt"))

(defn is-increased [before current]
  (if (< before current)
    1
    0
    ))

(defn string->int [input]
  (Integer/parseInt input))

(defn convert-to-ints [measures]
  (map string->int measures))

(defn calculate-distance [measures]
  (let [measure-1st (first measures)
        measure-2nd (second measures)
        measure-3rd (nth measures 2)]
    (+ measure-1st measure-2nd measure-3rd)))

(defn count-increases-recur [measures distance-before, increase-count]
  (if (= (count measures) 2)
    increase-count
    (let [distance-current (calculate-distance measures)
          increased (is-increased distance-before distance-current)]
      (count-increases-recur (rest measures) distance-current (+ increased increase-count)))
    ))

(defn count-increases [measures]
  (let [int-measures (convert-to-ints measures)
        measures-rest (rest int-measures)
        distance-before (calculate-distance int-measures)]
    (-> measures-rest
        (count-increases-recur distance-before 0)
        ))
  )

(deftest day1-task2
  (testing "reads input"
    (is (= 2000 (count input))))
  (testing "sum"
    (is (= (calculate-distance [1 2 3])) 6))
  (testing "detect increased distance"
    (is (= (count-increases ["199" "200" "208" "210" "200" "207" "240" "269" "260" "263"])
           5)))
  (testing "solution"
    (is (= (count-increases input) 1597)))
  )