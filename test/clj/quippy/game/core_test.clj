(ns quippy.game.core-test
  (:require
   [clojure.test :refer :all]
   [quippy.game.core :refer :all]
   [clojure.string :as str]))




(defn run-event [[game-state response-list] event-fn]
  (let [[next-game-state responses [timer-event _]] (event-fn game-state)
        game-response [next-game-state (conj response-list responses)]]
    (if timer-event
      (run-event game-response timer-event)
      game-response)))


(defn run-user-event [game-response {:keys [event player]}]
  (run-event game-response #(process-user-event % event player)))

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
    (let [setup-state (->> setup-events
                           flatten
                           (reduce run-user-event [default-game-state []])
                           first)]
      (->> events
           (reduce run-user-event [setup-state []])
           second))))

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

(def prompt->player
  (->> prompt-submit-events
       (map (fn [{:keys [event player]
                  {:keys [prompt]} :event}] [prompt player]))
       (into {})))

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

(def quip->player
  (->> quip-submit-events
       (map (fn [{:keys [event player]
                  {:keys [prompt quip]} :event}] [[prompt quip] player]))
       (into {})))

(defn make-vote-submit-event [quip-responses]
  (for [[[id {:keys [quips can-vote]}] vote-idx] (zipmap quip-responses (range))
        :when can-vote]
    {:event {:action :submit-vote
             :vote-for (as-> vote-idx vote
                         (mod vote (count quips))
                         (nth quips vote))}
     :player id}))

(def vote-submit-event
  (-> (simulate-events [lobby-join-events game-start-events prompt-submit-events] quip-submit-events)
      last
      make-vote-submit-event))

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
                         {:event :state-transition
                          :state :lobby
                          :players (map second in-lobby)}))
           (simulate-events [] lobby-join-events))))
  (testing "starting the game"
    (is (= [(repeat-map (map first players) {:event :state-transition
                                             :state :prompt})]
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
        (is (= (:event response) :state-transition))
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
        (is (= (:event response) :state-transition))
        (is (= (:state response) :vote))
        (is (= (-> response
                   :quips
                   count) 3))
        (is (= (:can-vote response)
               (not-any? #(str/includes? % (get players player))
                         (:quips response))))
        (is (str/includes? (:prompt response) "prompt"))))))

(deftest test-vote
  (testing "voting 1 round"
    (let [vote-responses (simulate-events [lobby-join-events
                                           game-start-events
                                           prompt-submit-events
                                           quip-submit-events]
                                          vote-submit-event)
          ack-responses (drop-last 2 vote-responses)
          completed-response (nth vote-responses (-> vote-responses count (- 2)))
          vote-response (last vote-responses)
          first-vote-prompt (-> (simulate-events [lobby-join-events game-start-events prompt-submit-events] quip-submit-events)
                                last
                                vals
                                first
                                :prompt)
          expected-votes (->> vote-submit-event
                              (map #(get-in % [:event :vote-for]))
                              (group-by identity)
                              (map (fn [[k v]] [k {:votes (count v)
                                                   :quipper (->> [first-vote-prompt (first v)]
                                                                 (get quip->player)
                                                                 (get players))}]))
                              (into {}))]
      (is (= (repeat (-> players count (- 4)) {})
             ack-responses))
      (is ((complement empty?) completed-response))
      (doseq [[player response] completed-response]
        (is (= (:event response) :state-transition))
        (is (= (:state response) :results))
        (is (= (:quips response) expected-votes))
        (is (= (:prompter response)
               (get players (get prompt->player (:prompt response)))))
        (is (str/includes? (:prompt response) "prompt")))
      ;; FIXME: Copypasta
      (doseq [[player response] vote-response]
        (is (= (:event response) :state-transition))
        (is (= (:state response) :vote))
        (is (= (-> response
                   :quips
                   count) 3))
        (is (= (:can-vote response)
               (not-any? #(str/includes? % (get players player))
                         (:quips response))))
        (is (str/includes? (:prompt response) "prompt"))))))

;; TODO: Test final result tally
