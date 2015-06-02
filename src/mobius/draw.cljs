(ns mobius.draw
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
            [cljs.core.match :refer-macros [match]]))

(enable-console-print!)

(def radius 2)

(defn point [context [x y]]
  (.beginPath context)
  (.arc context x y radius 0 (* 2 Math/PI) false)
  (.stroke context)
  (.fill context)
  (.closePath context))

(defn line [context p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (.beginPath context)
    (.moveTo context x1 y1)
    (.lineTo context x2 y2)
    (.stroke context)
    (.closePath context)))

(defn triangle [context p1 p2 p3]
  (let [[x1 y1] p1
        [x2 y2] p2
        [x3 y3] p3]
    (.beginPath context)
    (.moveTo context x1 y1)
    (.lineTo context x2 y2)
    (.lineTo context x3 y3)
    (.fill context)
    (.closePath context)))

(defn rect [context [x1 y1] [x2 y2]]
  (.fillRect context x1 y1 x2 y2))

(defn circle [context center radius]
  (.beginPath context)
  (.arc context (center 0) (center 1) radius 0 (* 2 Math/PI) false)
  (.stroke context)
  (.fill context)
  (.closePath context))

(defn style [context s]
  (doseq [[k v] s]
    (case k
      :fill (set! (. context -fillStyle) v)
      :stroke (set! (. context -strokeStyle) v)
      :lineDash (set! (. context -setLineDash) v)
      :lineWidth (set! (. context -lineWidth) v))))

(defn render
  [data context]
  (match data
         [:rect p1 p2]
         (rect context p1 p2)
         [:point p]
         (point context p)
         [:line p1 p2]
         (line context p1 p2)
         [:triangle p1 p2 p3]
         (triangle context p1 p2 p3)
         [:circle center radius]
         (circle context center radius)
         [:style s]
         (style context s)))

(defn drawing-loop [id]
  (let [draw-chan (chan)
        canvas (.getElementById js/document id)
        context (.getContext canvas "2d")]
    (go (loop []
          (let [draw-msg (<! draw-chan)]
            (render draw-msg context)
            (recur))))
    draw-chan))
