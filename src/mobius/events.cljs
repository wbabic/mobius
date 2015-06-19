(ns mobius.events
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [put! chan]]
            [goog.dom :as gdom]
            [goog.events :as events]))

;; mouse event utils

(def event-map
  {:mouse-down goog.events.EventType.MOUSEDOWN
   :mouse-move goog.events.EventType.MOUSEMOVE
   :mouse-up goog.events.EventType.MOUSEUP
   :click goog.events.EventType.CLICK
   :dblclick goog.events.EventType.DBLCLICK
   :key-down goog.events.EventType.KEYDOWN})

(defn events->chan
  ([el event-type] (events->chan el event-type (chan)))
  ([el event-type c]
     (events/listen el (event-type event-map)
                    (fn [e] (put! c e)))
     c))

(defn mouse-chan [element event-type key t-fn]
  (let [f (map (fn [e] (let [px (.-offsetX e)
                             py (.-offsetY e)]
                         [px py])))
        g (map (fn [p] (vector key p)))]
    (events->chan element event-type
                  (chan 1 (comp f t-fn g)))))

(defn keys-chan []
  (events->chan js/window :key-down
                (chan 1 (comp (map #(.-keyCode %))
                              (filter #{37 38 39 40})
                              (map {37 :previous
                                    38 :up
                                    39 :next
                                    40 :down})))))

(comment
  ;; test events
  (in-ns 'mobius.events)
  (let [c (keys-chan)]
    (go
      (let [m (<! c)]
        (prn m))))

  (let [canvas (.getElementById js/document "mobius-canvas-1")
        ch (mouse-chan canvas :mouse-down :click)]
    (go
      (prn (<! ch))))
  )
