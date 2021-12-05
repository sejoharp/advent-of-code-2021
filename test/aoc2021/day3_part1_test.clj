(ns aoc2021.day3-part1-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc2021.common :as common]))
(def input
  (common/parse-input "resources/input-day3.txt"))

; thanks to https://stackoverflow.com/questions/29233648/how-to-transpose-a-nested-vector-in-clojure
(defn nth-column [matrix n]
  (for [row matrix] (Character/digit (nth row n) 10)))

; thanks to https://stackoverflow.com/questions/29233648/how-to-transpose-a-nested-vector-in-clojure
(defn transpose [matrix]
  (for [column (range (count (first matrix)))]
    (nth-column matrix column)))

(defn most-common-bit [bits]
  (let [length (count bits)
        mayority (Math/round (double (/ length 2)))]
    (if (<= mayority (count (filter #(= % 1) bits)))
      1
      0
      )
    ))

(defn least-common-bit [bits]
  (let [length (count bits)
        mayority (Math/round (double (/ length 2)))]
    (if (> mayority (count (filter #(= % 1) bits)))
      1
      0
      )
    ))

(defn bin->int [binary-array]
  (->> binary-array
       str/join
       (str "2r")
       read-string)
  )
(defn least-common-binary [input]
  (map #(least-common-bit %) input))

(defn most-common-binary [input]
  (map #(most-common-bit %) input))

(deftest day3-task1
  (testing "reads input"
    (is (= (count input) 1000)))
  (testing "turning array from rows to columns"
    (let [example ["00100"
                   "11110"
                   "10110"
                   "10111"
                   "10101"
                   "01111"
                   "00111"
                   "11100"
                   "10000"
                   "11001"
                   "00010"
                   "01010"]
          expected [[0 1 1 1 1 0 0 1 1 1 0 0]
                    [0 1 0 0 0 1 0 1 0 1 0 1]
                    [1 1 1 1 1 1 1 1 0 0 0 0]
                    [0 1 1 1 0 1 1 0 0 0 1 1]
                    [0 0 0 1 1 1 1 0 0 1 0 0]]]
      (is (= (transpose example) expected))
      ))
  (testing "find most common bit"
    (is (= (most-common-bit [0 1 1 1 1 0 0 1 1 1 0 0]) 1))
    (is (= (most-common-bit [0 1 0 0 0 1 0 1 0 1 0 1]) 0))
    (is (= (least-common-bit [0 1 1 1 1 0 0 1 1 1 0 0]) 0))
    (is (= (least-common-bit [0 1 0 0 0 1 0 1 0 1 0 1]) 1))
    )
  (testing "common-binary"
    (let [input [[0 1 1 1 1 0 0 1 1 1 0 0]
                 [0 1 0 0 0 1 0 1 0 1 0 1]
                 [1 1 1 1 1 1 1 1 0 0 0 0]
                 [0 1 1 1 0 1 1 0 0 0 1 1]
                 [0 0 0 1 1 1 1 0 0 1 0 0]]]
      (is (= (most-common-binary input) [1 0 1 1 0]))
      (is (= (least-common-binary input) [0 1 0 0 1]))
      ))
  (testing "binary array to int"
    (is (= (bin->int [1 0 1 1 0]) 22))
    (is (= (bin->int [0 1 0 0 1]) 9))

    )

  (testing "solution"
    (let [diagnostic-bits (transpose input)
          gamma-rate-binary (most-common-binary diagnostic-bits)
          epsilon-rate-binary (least-common-binary diagnostic-bits)
          gamma-rate (bin->int gamma-rate-binary)
          epsilon-rate (bin->int epsilon-rate-binary)
          power-consumption (* gamma-rate epsilon-rate)]
      (is (= power-consumption 0))
      )
    )
  )
