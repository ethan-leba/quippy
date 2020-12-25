(ns quippy.game.core-test
  (:require
   [clojure.test :refer :all]
   [quippy.game.core :refer :all]))

(defn run-event [[game-state response-list] {:keys [event player]}]
  (let [[next-game-state responses] (process-user-event game-state event player)]
    [next-game-state (conj response-list responses)]))

(defn simulate-events [initial-state & events]
  (reduce run-event [initial-state []] events))

(defn repeat-map [keys val]
  (into {} (for [key keys] [key val])))

(def players (into (sorted-map) (map (juxt (comp keyword str) str) "abcdefg")))

(deftest test-process-user-event
  (testing "invalid action"
    (let [[_ response-list]
          (simulate-events
           default-game-state
           {:event {:action nil}
            :player nil})]
      (is (= '({}) response-list))))
  (let [[lobby-state lobby-responses]
        (apply
         simulate-events
         default-game-state
         (for [[id username] players]
           {:event {:action :lobby-join
                    :username username}
            :player id}))
        [prompt-state start-responses]
        (simulate-events
         lobby-state
         {:event {:action :game-start}
          :player :a})
        [_ prompt-responses]
        (apply
         simulate-events
         lobby-state
         (for [[id username] players]
           {:event {:action :prompt-submit
                    :prompts (map #(str "prompt-" username "-" %) (range 2))}
            :player :a}))]
    (testing "joining the lobby"
      (is (= (for [i (range (count players))
                   :let [in-lobby (take (inc i) (into [] players))]] ;; Ordering issues? 
               (repeat-map (map first in-lobby)
                           {:players (map second in-lobby)}))
             lobby-responses)))
    (testing "starting the game"
      (is (= [(repeat-map (map first players) {:state :prompt})]
             start-responses)))
    (testing "entering prompts"
      (let [ack-responses (drop-last 1 prompt-responses)
            completed-response (take-last 1 prompt-responses)]
        (is (= (repeat (-> players count dec) {})
               ack-responses))
        (doseq [[player response] completed-response]
          (is (= (:state response) :quip))
          (is (seq? (:prompts response)))
          (is (not-any? #(contains? (get player players) %)
                        (:prompts response))))))))

