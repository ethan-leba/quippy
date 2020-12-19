(ns quippy.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[quippy started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[quippy has shut down successfully]=-"))
   :middleware identity})
