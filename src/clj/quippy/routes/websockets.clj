(ns quippy.routes.websockets
  (:require
   [clojure.tools.logging :as log]
   [immutant.web.async :as async]))

(defonce channels (atom {}))

(defn connect! "Establishes a connection"
  [channel type]
  (log/info "channel open")
  (swap! channels assoc channel {:type type :lobby nil}))

(defn disconnect! [channel {:keys [code reason]}]
  (log/info "close code:" code "reason:" reason)
  (swap! channels #(dissoc channel %)))

(defn process-user-event! [channel msg]
  (doseq [channel (keys @channels)]
    (async/send! channel msg)))

(defn handle-action! [channel msg])

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
