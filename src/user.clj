(ns user
  (:require [nextjournal.clerk :as clerk]))

(defn run [opts]
  (clerk/serve! {:watch-paths ["notebooks"]
                 :browse? true}))