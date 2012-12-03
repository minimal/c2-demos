(ns hello-bars.core
  (:use-macros [c2.util :only [bind!]])
  (:use [c2.core :only [unify unify!]])
  (:require [c2.scale :as scale]
            [c2.event :as event]
            [c2.maths :as maths]))

;; horizontal bars with divs
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


;; vertical bar

(def vdata (atom (map maths/cos (range 0 (* 4 maths/Pi)
                                       (* 4 (/ maths/Pi 500))))))

(bind! "#vbars"
       (let [x-s (range 500)
             y-s @vdata
             data (map-indexed vector y-s)
             s (scale/linear :domain [(- 0 (apply max y-s)) (apply max y-s)]
                             :range [350 100])]
         [:svg
          {:class "myfoo"}
          [:line {:x1 0 :x2 500 :y1 350 :y2 350}]
          [:g.chart
           (unify data (fn [[x y]]
                         [:line {:x1 x :x2 x :y1 350 :y2 (s y) :title (s y)}]))]]))

;; util functions
(defn myfilter [data]
  (into {} (filter #(even? (% 1)) data)))

(defn rand-all-vals [data]
  (into [] (for [[key val] data] [key (rand)])))

(defn sort-by-val [d] (sort-by #(% 1) d))

(defn rotate
  "cycle a seq n steps"
  ([data] (rotate data 1))
  ([data n]
     (let [len (count data)]
       (take-last len (take (+ n len) (cycle data))))))

;; events
(event/on-raw "#button" :click
  (fn [_ & more] (swap! mydata
                       rand-all-vals)))

(event/on-raw "#shift" :click
  (fn [_ & more] (swap! mydata
                       rotate)))

(event/on-raw "#shiftv" :click
  (fn [_ & more] (swap! vdata
                       rotate 20)))

