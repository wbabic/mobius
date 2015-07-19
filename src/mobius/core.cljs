(ns mobius.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [chan <! >! timeout]]
            [cljs.core.match :refer-macros [match]]
            [mobius.draw :as draw]
            [mobius.render.canvas :as render]
            [complex.transform :as t]))

(enable-console-print!)

(def canvas-1-config
  {:resolution [500 500]
   :domain [-4 4]
   :range [-4 4]})

(def canvas-2-config
  {:resolution [500 500]
   :domain [-4 4]
   :range [-4 4]})

(defonce app-state
  (atom
   {:index 0
    :transforms t/transforms
    :render-list #{}
    }))

(defn current-transform
  "return current transform from dereferences state"
  [state]
  (let [i (:index state)]
    (get (:transforms state) i)))

(defn transform-fn
  "return current transform from dereferenced state"
  [state]
  (let [t (current-transform state)]
    (:transform t)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (println "reloading mobius.core ...")
  )

(def toggle-fn
  (fn [value] (if (true? value) false true)))

(defn update-mouse-mode [e owner control-chan]
  (let [target (. e -target)
        name (. target -name)
        key (keyword (. target -value))]
    (om/update-state!
     owner [:mouse-mode key] toggle-fn)
    (let [new-mouse-mode (om/get-state owner [:mouse-mode])]
      (go
        (if (and (false? (:rectangular new-mouse-mode))
                 (false? (:polar new-mouse-mode)))
          (>! control-chan :end)
          (>! control-chan :start))))))

(defn mouse-mode-input [owner value control-chan]
  (dom/input #js {:type "checkbox"
                  :name "mouse-mode"
                  :value value
                  :onChange
                  #(update-mouse-mode % owner control-chan)}))

(defn mouse-mode [owner control-chan]
  (dom/div #js {:className "mouse-mode"}
           (dom/dl nil
                   (dom/h3 nil "Mouse Mode")
                   (dom/dt nil "Polar")
                   (dom/dd nil (mouse-mode-input owner "polar"
                                                 control-chan))
                   (dom/dt nil "Rect")
                   (dom/dd nil (mouse-mode-input owner "rectangular"
                                                 control-chan)))))

(defn update-transform-index [e app-state index]
  (om/update! app-state [:index] index))

(defn select-transform [index value checked app-state]
  (if checked
    (dom/input #js {:type "radio"
                    :value value
                    :checked "checked"})
    (dom/input #js {:type "radio"
                    :value value
                    :onChange
                    #(update-transform-index % app-state index)})))

(defn transform-item [t index current-index app-state]
  (let [name (:name t)
        text (:text t)]
    (list
     (dom/dt nil name)
     (dom/dd #js {:className "transform"}
             text
             (select-transform index name (= index current-index) app-state)))))

(defn transform-items
  "List available transforms and a means to select current transform"
  [app-state]
  (let [transforms (:transforms @app-state)
        current-index (:index @app-state)]
    (apply dom/dl nil
           (flatten (for [[t i] (mapv vector transforms (range))]
                      (transform-item t i current-index app-state))))))

(defn next-step
  "return new state for given event"
  [event state]
  (prn event)
  (match event
         [:click p] (conj state p)
         [:move p] state))

(defn handle-event
  [owner app-state event-chan]
  (go-loop []
    (let [event (<! event-chan)]
      (match event
             [:move mouse-point]
             (do
               (om/set-state! owner :mouse-point mouse-point))
             [:click mouse-point]
             (do
               (prn mouse-point)
               (om/set-state! owner :mouse-point mouse-point)))
      (recur))))

(defn toggle [in]
  (let [out (chan)
        ctr (chan)]
    (go-loop [on false]
      (recur
       (alt!
         in ([x] (when on (>! out x)) on)
         ctr ([x] x))))
    {:chan out
     :ctr ctr}))

(defn mobius-config
  "input form for mobius"
  [app-state owner]
  (reify
    om/IInitState
    (init-state [_]
      {:mouse-mode {:polar false :rectangular false}
       :mouse-point nil})
    om/IWillMount
    (will-mount [_]
      (let [event-chan (om/get-shared owner :event-chan-1)
            control-chan (om/get-shared owner :control-chan)
            c (toggle event-chan)
            in (handle-event owner app-state (:chan c))
            ctr (:ctr c)]
        (go (loop []
              (let [control-type (<! control-chan)]
                (condp = control-type
                  :start (>! ctr true)
                  :end   (>! ctr false))
                (recur))))))

    om/IRenderState
    (render-state [_ state]
      (let [mouse-mode-state (:mouse-mode state)
            mouse-point (:mouse-point state)
            control-chan (om/get-shared owner :control-chan)
            draw-chan-1 (om/get-shared owner :draw-chan-1)
            draw-chan-2 (om/get-shared owner :draw-chan-2)]
        (when (and
               (or (true? (:rectangular mouse-mode-state))
                   (true? (:polar mouse-mode-state)))
               (not (nil? mouse-point)))
          ;; draw local state
          (draw/render-local app-state state
                             draw-chan-1
                             draw-chan-2
                             #(t/mult (transform-fn @app-state) %)))
        (dom/div nil
                 (dom/div #js {:className "select-transform"}
                          (dom/h3 nil "Select Transform")
                          (transform-items app-state))
                 (mouse-mode owner control-chan))))))

(defn el [id] (js/document.getElementById id))

(om/root
 mobius-config
 app-state
 {:target (el "mobius-config")
  :shared (let [[draw-chan-1 event-chan-1]
                (render/drawing-loop "mobius-canvas-1"
                                   canvas-1-config
                                   true)
                draw-chan-2 (render/drawing-loop "mobius-canvas-2" canvas-2-config)]
            {:draw-chan-1 draw-chan-1
             :event-chan-1 event-chan-1
             :draw-chan-2 draw-chan-2
             :control-chan (chan)})})

(comment
  (in-ns 'mobius.core)
  ;; user-screen mapping
  (let [m (render/user->screen canvas-1-config)
        tf (map m)
        data [[0 0] [1 0] [0 1] 1 [-10 10] [10 -10]]]
    (sequence tf data))
  ;;=> ([250 250] [313 250] [250 188] 62.5 [-375 -375] [875 875])

  ;; screen->user mapping
  (let [m-inv (render/screen->user canvas-1-config)
        t-fn (map m-inv)
        data [[250 250] [313 250] [250 188] [-375 -375] [875 875]]]
    (sequence t-fn data))
  ;;=> ([0 0] [1.008 0] [0 0.992] [-10 10] [10 -10])

  (let [m (render/user->screen canvas-1-config)
        tf (map m)
        data [[12500 12500] [0.125 0.125] [0 0] [-12499.875 -12499.875]]]
    (sequence tf data))
  ;;=> ([781500 -781000] [258 242] [250 250] [-780992 781492])
  )
