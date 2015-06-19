(ns mobius.draw
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
            [cljs.core.match :refer-macros [match]]
            [mobius.events :as e]))

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
  ;;(.fill context)
  (.closePath context))

(defn style [context s]
  (doseq [[k v] s]
    (case k
      :fill (set! (. context -fillStyle) v)
      :stroke (set! (. context -strokeStyle) v)
      :lineDash (set! (. context -setLineDash) v)
      :lineWidth (set! (. context -lineWidth) v))))

;; user space -> screen mapping stuff
(defn plus [p1 p2]
  (mapv + p1 p2))

(defn scal-mult [t p]
  (mapv #(* t %) p))

(defn translation
  "returns function that translates vectors by given vector"
  ([w]
   (fn [v] (plus w v)))
  ([w n]
   (fn [v] (plus (scal-mult n  w) v))))

(defn scale
  ([s] (scale s s))
  ([sx sy] (fn [[x y]] [(* sx x) (* sy y)])))

(def round-pt (fn [p] (mapv Math.round p)))

(defn user->screen
  [config]
  (let [[xi xf] (:domain config)
        [yi yf] (:range config)
        [xres yres] (:resolution config)
        sx (/ xres (- xf xi))
        sy (/ yres (- yi yf))
        scale (scale sx sy)
        translate (translation [(- xi) (- yf)])]
    (fn [p]
      (if (number? p)
        (* sx p)
        ((comp round-pt scale translate) p)))))

(defn transform-fn
  "returns function that transforms points in user space
  to points in screen space"
  ([config]
   (let [m (user->screen config)]
     (map m))))

(defn render
  [data context t-fn]
  (match data
         [:rect p1 p2]
         (rect context (t-fn p1) (t-fn p2))
         [:point p]
         (point context (t-fn p))
         [:line p1 p2]
         (line context (t-fn p1) (t-fn p2))
         [:triangle p1 p2 p3]
         (triangle context (t-fn p1) (t-fn p2) (t-fn p3))
         [:circle C]
         (let [{:keys [center radius]} C]
           (circle context (t-fn center) (t-fn radius)))
         [:style s]
         (style context s)))

(defn drawing-loop
  ([id config]
   (let [draw-chan (chan)
         canvas (.getElementById js/document id)
         context (.getContext canvas "2d")
         t-fn (user->screen config)]
     (go (loop [t-fn t-fn]
           (let [draw-msg (<! draw-chan)]
             (render draw-msg context t-fn)
             (recur t-fn))))
     draw-chan))
  ([id config events]
   (let [draw-chan (chan)
         canvas (.getElementById js/document id)
         context (.getContext canvas "2d")
         t-fn (user->screen config)
         ;; need inverse function to map screen coords back to user coords
         event-chan (e/mouse-chan canvas :mouse-move :move)]
     (go (loop [t-fn t-fn]
           (let [draw-msg (<! draw-chan)]
             (render draw-msg context t-fn)
             (recur t-fn))))
     [draw-chan event-chan])))
