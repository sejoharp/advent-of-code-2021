(ns aoc2021.common
  (:require
    [clojure.string :as str]))

(defn parse-input
  [file]
  (->> file
       slurp
       str/split-lines
       )
  )

(defn string->int [input]
  (Integer/parseInt input))

(defn convert-to-ints [measures]
  (map string->int measures))
