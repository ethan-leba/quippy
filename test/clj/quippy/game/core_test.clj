(ns quippy.game.core-test
  (:require
   [clojure.test :refer :all]
   [quippy.game.core :refer :all]
   [clojure.string :as str]))


(defn run-event [[game-state response-list] {:keys [event player]}]
  (let [[next-game-state responses] (process-user-event game-state event player)]
    [next-game-state (conj response-list responses)]))

(defn rotate-list
  "Deterministic replacement for `shuffle`"
  [take-n]
  (let [rotator (atom 0)]
    (fn [elems]
      (let [[skip _] (swap-vals! rotator #(+ % take-n))]
        (->> (cycle elems)
             (drop skip)
             (take (count elems)))))))

(defn simulate-events
  "Simulates a series of events getting sent to the server.
  Runs `setup-events` first, and then `events`, but only returns the
  output of `events`."
  [setup-events events]
  (with-redefs [shuffle (rotate-list 3)]
    (->> events
         (concat (flatten setup-events))
         (reduce run-event [default-game-state []])
         second
         (take-last (count events)))))

(defn repeat-map [keys val]
  (into {} (for [key keys] [key val])))

(def players (into (sorted-map) (map (juxt (comp keyword str) str) "abcdefg")))

(def lobby-join-events
  (for [[id username] players]
    {:event {:action :lobby-join
             :username username}
     :player id}))

(def game-start-events
  '({:event {:action :game-start}
     :player :a}))

(def prompt-submit-events
  (for [[id username] players]
    {:event {:action :submit-prompt
             :prompt (str "prompt-" username)}
     :player id}))

(defn make-quip-submit-event [assigned-prompts]
  (for [[id {:keys [prompts]}] assigned-prompts
        prompt prompts]
    {:event {:action :submit-quip
             :prompt prompt
             :quip (str "quip-" (get players id))}
     :player id}))

(def quip-submit-events
  (-> (simulate-events [lobby-join-events game-start-events] prompt-submit-events)
    last
    make-quip-submit-event))

(deftest test-starting
  (testing "invalid action"
    (is (= '({})
           (simulate-events
            '()
            '({:event {:action nil}
               :player nil})))))
  (testing "joining the lobby"
    (is (= (for [i (range (count players))
                 :let [in-lobby (take (inc i) (into [] players))]] ;; Ordering issues?
             (repeat-map (map first in-lobby)
                         {:players (map second in-lobby)}))
           (simulate-events [] lobby-join-events))))
  (testing "starting the game"
    (is (= [(repeat-map (map first players) {:state :prompt})]
           (simulate-events [lobby-join-events] game-start-events)))))

(deftest test-prompts
  (testing "entering prompts"
    (let [prompt-responses (simulate-events [lobby-join-events game-start-events] prompt-submit-events)
          ack-responses (drop-last 1 prompt-responses)
          completed-response (last prompt-responses)]
      (is (= (repeat (-> players count dec) {})
             ack-responses))
      (is ((complement empty?) completed-response))
      (doseq [[player response] completed-response]
        (is (= (:state response) :quip))
        (is (not-any? #(str/includes? % (get players player))
                      (:prompts response)))))))
(deftest test-quip
  (testing "entering quips"
    (let [quip-responses (simulate-events [lobby-join-events game-start-events prompt-submit-events] quip-submit-events)
          ack-responses (drop-last 1 quip-responses)
          completed-response (last quip-responses)]
      (is (= (repeat (-> players count (* 3) dec) {})
             ack-responses))
      (is ((complement empty?) completed-response))
      (doseq [[player response] completed-response]
        (is (= (:state response) :vote))
        (is (= (-> response
                   :quips
                   count) 3))
        (is (= (:can-vote response)
               (not-any? #(str/includes? % (get players player))
                         (:quips response))))
        (is (str/includes? (:prompt response) "prompt"))))))
