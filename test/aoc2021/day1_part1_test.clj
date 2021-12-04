(ns aoc2021.day1-part1-test
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

(defn count-increases-recur [measures measure-before, increase-count]
  (if (= (count measures) 0)
    increase-count
    (let [measure-current (first measures)
          increased (is-increased measure-before measure-current)]
      (count-increases-recur (rest measures) measure-current (+ increased increase-count)))
    ))

(defn count-increases [measures]
  (let [int-measures (convert-to-ints measures)
        measures-rest (rest int-measures)
        measures-first (first int-measures)]
    (-> measures-rest
        (count-increases-recur measures-first 0)
        ))
  )

(deftest day1-task1
  (testing "reads input"
    (is (= 2000 (count input))))
  (testing "detect increased distance"
    (is (= 7 (count-increases ["199"
                               "200"
                               "208"
                               "210"
                               "200"
                               "207"
                               "240"
                               "269"
                               "260"
                               "263"]))))
  (testing "solution"
    (is (= (count-increases input) 1553)))
  )