(ns mobius.draw
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
            [cljs.core.match :refer-macros [match]]
            [mobius.events :as e]
            [mobius.vector :as v]
            [mobius.geometry :as geom]))

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
(def round-pt (fn [p] (mapv Math.round p)))

(defn user->screen
  [config]
  (let [[xi xf] (:domain config)
        [yi yf] (:range config)
        [xres yres] (:resolution config)
        sx (/ xres (- xf xi))
        sy (/ yres (- yi yf))
        scale (v/scale sx sy)
        translate (v/translation [(- xi) (- yf)])]
    (fn [p]
      (if (number? p)
        (* sx p)
        ((comp round-pt scale translate) p)))))

(defn screen->user
  [config]
  (let [[xi xf] (:domain config)
        [yi yf] (:range config)
        [xres yres] (:resolution config)
        sx (/ xres (- xf xi))
        sy (/ yres (- yi yf))
        scale-inverse (v/scale (/ sx) (/ sy))
        translate-inverse (v/translation [xi yf])]
    (comp translate-inverse scale-inverse)))

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
  ([id config events?]
   (let [draw-chan (chan)
         canvas (.getElementById js/document id)
         context (.getContext canvas "2d")
         t-fn (user->screen config)
         f-inv (map (screen->user config))
         event-chan (e/mouse-events canvas f-inv)]
     (go (loop [t-fn t-fn]
           (let [draw-msg (<! draw-chan)]
             (render draw-msg context t-fn)
             (recur t-fn))))
     [draw-chan event-chan])))

;; animation functions
(defn animate
  "perform an animation of given data with given delay"
  [animation-data-key delay draw-chan-1 draw-chan-2 animation-chan]
  ;; set animation-mode to true
  ;; send data to draw-channels
  ;; when done, set animation-mode to false
  )

(defn render-data
  "render data to drawing channels"
  [data-key draw-chan-1 draw-chan-2])

(comment
  ;; to animate
  (animate :axis 800 c-1 c-2 a-chan)
  ;; to render
  (render-data :axis c-1 c-2)
  )

;; generic drawing utils
(def clear
  [[:style {:fill "grey"}]
   [:rect [-4 4] [4 -4]]])

(defn clear-screen [draw-chan]
  (go (doseq [d clear]
        (>! draw-chan d))))

(defn concentric-circles
  "generate sequence of circles
  with given center and radii in range"
  [center radii]
  (let [c (fn [r] [:circle {:center center :radius r}])]
    (for [r radii]
      (c r))))

(def colors ["red" "orange" "yellow" "green" "blue" "indigo" "violet"])

(defn draw-axis
  "send a sequence of real-axis imaginary-axis and unit circle"
  [draw-chan-1 draw-chan-2 trans delay]
  (let [axis geom/axis]
    (go
      (doseq [[c color] (map vector axis ["yellow" "blue" "cyan" "green" "magenta" "red"])]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-1 c)
        (>! draw-chan-2 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-2 (trans c))))))

(defn draw-concentric-circles
  "send a sequence of circles to the drawing channel"
  [draw-chan-1 draw-chan-2 trans delay]
  (let [circles (concentric-circles [0 0] (sort [1 1.5 (/ 2 3) 2 0.50 4 0.25]))]
    (go
      (doseq [[c color] (map vector circles colors)]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-1 c)
        (>! draw-chan-2 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-2 (trans c))))))

(defn draw-radial-lines
  "send a sequence of radial lines to the drawing channel"
  [draw-chan-1 draw-chan-2 trans delay]
  (let [lines (geom/radial-lines 12)]
    (go
      (doseq [[l c] (map vector lines (cycle colors))]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l))))))

(defn draw-horizontal-lines
  "send a sequence of horizontal lines to the drawing channel"
  [draw-chan-1 draw-chan-2 trans delay]
  (let [lines (geom/horizontal-lines 0.50)]
    (go
      (doseq [[l c] (map vector lines (repeat "blue"))]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l))))))

(defn draw-vertical-lines
  "send a sequence of vertical lines to the drawing channel"
  [draw-chan-1 draw-chan-2 trans delay]
  (let [lines (geom/vertical-lines 0.50)]
    (go
      (doseq [[l c] (map vector lines (repeat "orange"))]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l))))))
