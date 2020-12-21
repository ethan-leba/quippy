(ns quippy.routes.websockets
  (:require
   [clojure.tools.logging :as log]
   [quippy.game.core :as game]
   [immutant.web.async :as async]))

(defonce channels (atom {}))

(defonce game-state (atom game/default-game-state))

(defn connect! "Establishes a connection"
  [channel type]
  (log/info "channel open")
  (swap! channels assoc channel {:type type :lobby nil}))

(defn disconnect! [channel {:keys [code reason]}]
  (log/info "close code:" code "reason:" reason)
  (swap! channels #(dissoc channel %)))

(defn process-user-event!
  "Handles the IO of event processing"
  [channel msg]
  (let [[new-state responses] (game/process-user-event @game-state msg)]
    (compare-and-set! game-state new-state)
    (doseq [[channel response] responses]
      (async/send! channel response))))

(defn websocket-callbacks
  "WebSocket callback functions"
  [type]
  {:on-open #(connect! % type)
   :on-close disconnect!
   :on-message process-user-event!})

(defn ws-handler [request type]
  (async/as-channel request #(websocket-callbacks type)))

(def websocket-routes
  [["/ws-user" #(ws-handler % :user)]
   ["/ws-display" #(ws-handler % :display)]])
