(ns day1-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn hello
  []
  (println "Hello"
       ))

(deftest day1-task1
  (testing "reads input"
    (is (= (hello) nil))))