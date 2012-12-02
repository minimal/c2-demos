(ns hello-bars.core
  (:use-macros [c2.util :only [bind!]])
  (:use [c2.core :only [unify unify!]])
  (:require [c2.scale :as scale]
            [c2.event :as event]))

(def mydata (atom [["A" 1], ["B" 7], ["C" 4], ["D" 10]]))
(bind! "#bars"
  (let [width 500 bar-height 20
        data @mydata
        s (scale/linear :domain [0 (apply max (vals data))]
                        :range [0 width])]

    [:div
     (unify data (fn [[label val]]
                    [:div.abar {:style {:height bar-height
                                        :width (s val)
                                        :background-color "gray"}
                                :title val}
                     [:span {:style {:color "white"}} label]]))]))

(bind! "#button"
  [:button "Randomise"])

(defn myfilter [data]
  (into {} (filter #(even? (% 1)) data)))

(defn rand-all-vals [data]
  (into [] (for [[key val] data] [key (rand)])))

;; (defn sort-map [data]
(defn sort-by-val [d] (sort-by #(% 1) d))

(defn do-cycle [data]
  "cycle a seq once"
  (let [len (count data)]
    (take-last len (take (inc len) (cycle data)))))

(event/on-raw "#button" :click
  (fn [_ & more] (swap! mydata
                       rand-all-vals)))

(event/on-raw "#shift" :click
  (fn [_ & more] (swap! mydata
                       do-cycle)))
