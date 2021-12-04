(ns aoc2021.day1-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc2021.common :as c]))
(def input
  (c/parse-input "resources/input-day1.txt"))

(deftest day1-task1
  (testing "reads input"
    (is (= (str/split-lines "hello") nil))))