(ns quippy.env
  (:require
    [selmer.parser :as parser]
    [clojure.tools.logging :as log]
    [quippy.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[quippy started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[quippy has shut down successfully]=-"))
   :middleware wrap-dev})
