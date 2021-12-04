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