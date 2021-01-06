(ns quippy.validation
  (:require [clojure.spec.alpha :as s]))

(s/def ::player-list (s/coll-of string?))

(s/valid? ::player-list '("a" "bbb" 3))
