(ns mobius.turtle
  (:require
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [complex.number :as n]
   [complex.turtle :as turtle]
   [complex.turtle.render :as render])
  (:require-macros
   [devcards.core :as dc :refer [defcard defcard-doc defcard-om]]))

(defcard turtle-definition
  "
## Mobius Turtle

### what happens when a good turtle meets a Mobius transformation

here is our turtle:"
  turtle/standard-turtle)

(defcard turtle-rendered-data
  "
## Render Turtle

### as data

a sequence of graphics data primitives:"
  (render/render-turtle turtle/standard-turtle))

(defcard turtle-rendered-canvas
  "
## Render Turtle

### to canvas")

(comment
  (in-ns 'mobius.turtle)
  )
