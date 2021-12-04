(ns aoc2021.day2-part1-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc2021.common :as common]))
(def input
  (common/parse-input "resources/input-day2.txt"))


(defn parse-movement [move]
  (let [separated (str/split move `#" ")
        amount (common/string->int (second separated))
        command (symbol (first separated))]
    [command amount]))

(defn forward [amount position]
  (let [horizontal (first position)
        depth (second position)
        new-horizontal (+ horizontal amount)]
    [new-horizontal depth]))

(defn up [amount position]
  (let [horizontal (first position)
        depth (second position)
        new-depth (- depth amount)
        new-depth-absolute (if (neg-int? new-depth) 0 new-depth)]
    [horizontal new-depth-absolute])
  )
(defn down [amount position]
  (let [horizontal (first position)
        depth (second position)
        new-depth (+ depth amount)]
    [horizontal new-depth]))

(comment
  ((resolve (symbol "print")) 1 2 3))

(defn move [movement-string position]
  (let [movement (parse-movement movement-string)
        command (first movement)
        amount (second movement)]
    ((resolve command) amount position))
  )

(deftest day2-task1
  (testing "reads input"
    (is (= (count input) 1000)))
  (testing "parse movement"
    (is (= (parse-movement "forward 6") [(symbol "forward") 6])))
  (testing "movements"
    (is (= (forward 1 [0 0]) [1 0]))
    (is (= (up 1 [0 2]) [0 1]))
    (is (= (up 1 [0 0]) [0 0]))
    (is (= (down 1 [0 0]) [0 1])))
  (testing "execute a move"
    (is (= (move "forward 1" [0 0]) [1 0]))
    ;(is (= (move "blub 1" [0 0]) [0 0]))
    )
  (testing "example movement"
    (let [example ["forward 5"
                   "down 5"
                   "forward 8"
                   "up 3"
                   "down 8"
                   "forward 2"]])
    ())
  (testing "solution"
    (is (= 0 0)))
  )
