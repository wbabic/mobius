(ns mobius.geometry.schemas
  "schemas for geometric objects"
  (:require [schema.core :as s :include-macros true]))

(def Point [s/Num])
(def Circle "a schema for a circle" {:center Point :radius s/Num})

(def circle [(s/one (s/eq :circle) "tag") (s/one Circle "data")])

(comment
  (s/check circle [:circle {:center [0 0] :radius 1}])
  )



;; circle point as records
;; see triangulator.datatypes
;; defrecord
;; extend-type IRender to render to a canvas context
;; can call render on the record directly

;; alternately, this code uses a vector
;; [:tag-name data]
;; and core.match

;; schema records
