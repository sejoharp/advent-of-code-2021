(ns aoc2021.day3-part2-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc2021.common :as common]))
(def input
  (common/parse-input "resources/input-day3.txt"))

(defn char->int [char]
  (Character/digit char 10))

; thanks to https://stackoverflow.com/questions/29233648/how-to-transpose-a-nested-vector-in-clojure
(defn nth-column [matrix n]
  (for [row matrix] (char->int (nth row n))))

; thanks to https://stackoverflow.com/questions/29233648/how-to-transpose-a-nested-vector-in-clojure
(defn transpose [matrix]
  (for [column (range (count (first matrix)))]
    (nth-column matrix column)))

(defn most-common-bit [bits]
  (let [ones (count (filter #(= % 1) bits))
        zeroes (count (filter #(= % 0) bits))]
    (if (>= ones zeroes)
      1
      0
      )
    ))

(defn least-common-bit [bits]
  (let [ones (count (filter #(= % 1) bits))
        zeroes (count (filter #(= % 0) bits))]
    (if (<= zeroes ones)
      0
      1
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


(defn co2-scrubber-rating-filter [input]
  (let [diagnostic-bits (transpose input)
        co2-scrubber-rating-binary (least-common-binary diagnostic-bits)]
    co2-scrubber-rating-binary
    ))


(defn find-co2-scrubber-rating [measures complete-pattern]
  (if (= (count measures) 1)
    (first measures)
    (let [pattern (first complete-pattern)
          pattern-rest (rest complete-pattern)
          position (- (count (first measures)) (count complete-pattern))
          filtered (filter #(= (char->int (nth % position)) pattern) measures)]
      (find-co2-scrubber-rating filtered pattern-rest)
      )))

(defn oxigin-generator-rating-filter [input]
  (let [diagnostic-bits (transpose input)
        oxigin-generator-rating-binary (most-common-binary diagnostic-bits)]
    oxigin-generator-rating-binary
    ))

(defn find-oxigin-generator-rating [measures complete-pattern]
  (if (= (count measures) 1)
    (first measures)
    (let [pattern (first complete-pattern)
          pattern-rest (rest complete-pattern)
          position (- (count (first measures)) (count complete-pattern))
          filtered (filter #(= (char->int (nth % position)) pattern) measures)]
      (find-oxigin-generator-rating filtered pattern-rest)
      )))

(defn string-array->decimal [string-array]
  (bin->int (map #(char->int %) string-array))
  )

(deftest day3-task2
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
    (is (= (most-common-bit [0 0 1 1]) 1))
    (is (= (least-common-bit [0 1 1 1 1 0 0 1 1 1 0 0]) 0))
    (is (= (least-common-bit [0 1 0 0 0 1 0 1 0 1 0 1]) 1))
    (is (= (least-common-bit [0 0 1 1]) 0))
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
  (testing "determine oxigin generator rating"
    (let [input ["00100"
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
                 "01010"]]
      (is (= (find-oxigin-generator-rating input (oxigin-generator-rating-filter input))) "10111")
      (is (= (string-array->decimal "10111") 23)))
    )
  (testing "determine co2 scrubber rating"
    (let [input ["00100"
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
                 "01010"]]
      (is (= (find-co2-scrubber-rating input (co2-scrubber-rating-filter input))) "01010")
      (is (= (string-array->decimal "01010") 10)))
    )
  (testing "solution"
    (let [co2-filter (co2-scrubber-rating-filter input)
          co2-binary (find-co2-scrubber-rating input co2-filter)
          co2-rate (string-array->decimal co2-binary)
          oxigin-filter (oxigin-generator-rating-filter input)
          oxigin-binary (find-oxigin-generator-rating input oxigin-filter)
          oxigin-rate (string-array->decimal oxigin-binary)

          life-support-rating (* co2-rate oxigin-rate)]
      (is (= life-support-rating 1454493))
      )
    )
  )
