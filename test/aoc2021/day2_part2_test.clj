(ns aoc2021.day2-part2-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc2021.common :as common]))
(def input
  (common/parse-input "resources/input-day2.txt"))


(defn parse-movement [movement]
  (let [separated (str/split movement #" ")
        amount (common/string->int (second separated))
        command (first separated)]
    [command amount]))

(defn forward [amount position]
  (let [horizontal (first position)
        depth (second position)
        aim (nth position 2)
        new-horizontal (+ horizontal amount)
        new-depth (+ depth (* aim amount))]
    [new-horizontal new-depth aim]))

(defn up [amount position]
  (let [horizontal (first position)
        depth (second position)
        aim (nth position 2)
        new-aim (- aim amount)
        new-aim-absolute (if (neg-int? new-aim) 0 new-aim)]
    [horizontal depth new-aim-absolute])
  )

(defn down [amount position]
  (let [horizontal (first position)
        depth (second position)
        aim (nth position 2)
        new-aim (+ aim amount)
        new-aim-absolute (if (neg-int? new-aim) 0 new-aim)]
    [horizontal depth new-aim-absolute]))

(defn move [movement position]
  (let [command (first movement)
        amount (second movement)]
    (case command
      "forward" (forward amount position)
      "down" (down amount position)
      "up" (up amount position)
      position)
    )
  )

(defn travel [movements position]
  (if (= (count movements) 0)
    position
    (let [parsed-movement (parse-movement (first movements))
          new-position (move parsed-movement position)]
      (travel (rest movements) new-position)
      ))
  )
(deftest day2-task2
  (testing "reads input"
    (is (= (count input) 1000)))
  (testing "parse movement"
    (is (= (parse-movement "forward 6") ["forward" 6])))
  (testing "movements"
    (is (= (forward 8 [5 0 5]) [13 40 5]))
    (is (= (forward 8 [5 1 5]) [13 41 5]))
    (is (= (up 1 [0 0 2]) [0 0 1]))
    (is (= (up 1 [0 0 0]) [0 0 0]))
    (is (= (down 1 [0 0 0]) [0 0 1])))
  (testing "execute a move"
    (is (= (move ["forward" 1] [0 0 0]) [1 0 0]))
    (is (= (move "blub 1" [1 1 1]) [1 1 1]))
    )
  (testing "example movement"
    (let [example ["forward 5"
                   "down 5"
                   "forward 8"
                   "up 3"
                   "down 8"
                   "forward 2"]]
      (is (= (travel example [0 0 0]) [15 60 10]))
      (is (=
            (* (first (travel example [0 0 0])) (second (travel example [0 0 0])))
            900)))
    )
  (testing "solution"
    (let [end-position (travel input [0 0 0])]
      (is (= end-position [2007 668080 747]))
      (is (= (* (first end-position) (second end-position)) 1340836560))
      )
    )
  )
