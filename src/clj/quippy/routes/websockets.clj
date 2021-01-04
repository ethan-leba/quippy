(ns quippy.routes.websockets
  (:require
   [clojure.tools.logging :as log]
   [quippy.game.core :as game]
   [immutant.web.async :as async]))

(defonce channels (atom #{}))

(defonce game-state (atom game/default-game-state))

(defn connect! "Establishes a connection"
  [channel]
  (log/info "channel open")
  (swap! channels conj channel))

(defn disconnect! [channel {:keys [code reason]}]
  (log/info "close code:" code "reason:" reason)
  (swap! channels #(remove #{channel} %)))

(defn process-event!
  "Handles the IO of event processing"
  [event-fn]
  (let [[new-state responses timer] (event-fn @game-state)]
    (compare-and-set! game-state new-state)
    (doseq [[channel response] responses]
      (async/send! channel response))
    (when-let [[action wait-time] timer]
      (future
        (Thread/sleep (* wait-time 1000))
        (process-event! action)))))

(defn process-user-event!
  "Handles the IO of event processing"
  [channel msg]
  (process-event! #(game/process-user-event % msg)))

(def websocket-callbacks
  "WebSocket callback functions"
  {:on-open connect!
   :on-close disconnect!
   :on-message process-user-event!})

(defn ws-handler [request]
  (async/as-channel request websocket-callbacks))

(def websocket-routes
 [["/ws" ws-handler]])
