;;; ------------------------------------------------------------------------------------
;;; 
;;; functions to generate a pack of 52 playing cards
;;;
;;; suffle-pack - this is the main function to use. It returns a shuffled deck of cards
;;; 
(ns algorithms.puzzles.poker.cards)

(defn make-card [val suit]
  {:value val :suit suit})

(def suits-list [:heart :diamond :club :spade] )
(def card-value-list [:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A])


(defn make-pack []
  (vec
   (for
    [suit suits-list
     val card-value-list]
     (make-card val suit)))
    )

;;; ------------------------ Remove a specific item from a vector
;;; There is no native function to do this in clojure
;;; See https://stackoverflow.com/questions/1394991/clojure-remove-item-from-vector-at-a-specified-location

;; return a list of the index pos to extract from
(defn index-exclude
  "Take all indices execpted ex" 
  [r ex]
  (filter #(not (ex %)) (range r)))

(defn dissoc-idx [v & ds]
  ;; NOTE: using mapv to return a vector rather than a collection
  ;; The vector v is being used as a function here
  (mapv v (index-exclude (count v) (into #{} ds))))

;; The following is how you can remove multiple items from a vector
;; (dissoc-idx [1 2 3] 1 2)
;; (map [1 2 3] [0])
;; (index-exclude 5 (into #{} [1 2])) 
;; vectors can be used as functions 
;; ([1 2 3] 0)
;;; ------------------------ END

(defn suffle-pack
  ;; If pack is not supplied create one and then shuffle
  ([] (suffle-pack (make-pack)))
  ;; Given a pack shuffle it
  ([pack] 
  ;; We will shuffle by taking random cards from the original unsuffled pack
  ;; and adding then to a new pack
  (loop [from-pack pack
         to-pack []]
    (if (= from-pack [])
      ;; Finished - return the shuffled pack
      to-pack
      ;; Take a random card from the from-pack and add to the to-pack...
      (let [random-pos (rand-int (count from-pack))
            card-to-move (get from-pack random-pos)
            from-pack-minus-card (dissoc-idx from-pack random-pos)]
        (recur from-pack-minus-card (conj to-pack card-to-move)))))
  ))

  ;; Given a pack! atom returns the next card from top of pack
  ;; Updates the pack! removing the card returned
  (defn deal-card! [pack!]
    (let [card (peek @pack!)
          updated-pack (if card (pop @pack!) nil) ;; if card is nil pack is empty
          _ (reset! pack! updated-pack)]
          card))

(comment

  (make-pack)
  (suffle-pack)

  ;; You can shuffle multiple times but I'm don't think there is any advantage of this??
  (suffle-pack (suffle-pack))
  (count (suffle-pack))

  (def the-pack (atom (suffle-pack)))

  (for [_ (range 50)] ;; deal this number of cards
    (deal-card! the-pack))

  (deal-card! the-pack)

  (count @the-pack)
  (pop @the-pack)
  (peek @the-pack)
  (peek [])
  (get @the-pack 51))

