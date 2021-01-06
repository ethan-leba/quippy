(ns quippy.routes.websockets
  (:import java.io.ByteArrayInputStream
           java.io.ByteArrayOutputStream)
  (:require
   [clojure.tools.logging :as log]
   [quippy.game.core :as game]
   [immutant.web.async :as async]
   [clojure.data.json :as json]
   [cognitect.transit :as t]))

(defn transit-write [out]
  (let [byte-stream (ByteArrayOutputStream.)]
    (t/write (t/writer byte-stream :json) out)
    (.toString byte-stream)))

(defn transit-read [in]
  (let [byte-stream (ByteArrayInputStream. (.getBytes in))]
    (t/read (t/reader byte-stream :json))))

(defonce channels (atom #{}))

(defonce game-state (atom game/default-game-state))

(defn connect! "Establishes a connection"
  [channel]
  (log/info "channel open")
  (swap! channels conj channel))

(defn process-event!
  "Handles the IO of event processing"
  [event-fn]
  (let [[new-state responses timer] (event-fn @game-state)]
    (compare-and-set! game-state @game-state new-state)
    (doseq [[channel response] responses]
      (async/send! channel (transit-write response)))
    (when-let [[action wait-time] timer]
      (future
        (Thread/sleep (* wait-time 1000))
        (process-event! action)))))

(defn disconnect! [channel {:keys [code reason]}]
  (log/info "close code:" code "reason:" reason)
  (swap! channels #(remove #{channel} %))
  (when (get (:players @game-state) channel)
    (process-event! game/reset-game)))

(defn process-user-event!
  "Handles the IO of event processing"
  [channel msg]
  (let [parsed-msg (transit-read msg)]
    (log/info "received event from " channel ": " parsed-msg)
    (process-event! #(game/process-user-event % parsed-msg channel))))

(def websocket-callbacks
  "WebSocket callback functions"
  {:on-open connect!
   :on-close disconnect!
   :on-message process-user-event!})

(defn ws-handler [request]
  (async/as-channel request websocket-callbacks))

(def websocket-routes
 [["/ws" ws-handler]])
