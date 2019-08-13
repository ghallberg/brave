(ns pegboard.core
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn tri*
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  [cand]
  (= (last (take-while #(<= % cand) tri)) cand))

(defn row-tri
  [row-no]
  (nth tri (- row-no 1)))

(defn row-num
  [pos]
  (inc (count (take-while #(< % pos) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
              board
              [[pos destination] [destination pos]])
    board))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if (<= destination (row-tri (row-num pos)))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [neighbor (+ (row-num pos) pos)
        destination (+ (row-num neighbor) neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [neighbor (+ 1 (row-num pos) pos)
        destination (+ 1 (row-num neighbor) neighbor)]
    (connect board max-pos pos neighbor destination)))


(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (if (<= pos max-pos)
    (let [pegged-board (assoc-in board [pos :pegged] true)] 
      (reduce (fn [new-board fun] (fun new-board max-pos pos))
              pegged-board
              [connect-right connect-down-right connect-down-left]))))

(defn new-board
  "Creates a new board with the given number of rows"
  [num-rows]
  (let [max-pos (row-tri num-rows)]
    (reduce (fn [new-board pos] (add-pos new-board max-pos pos))
            {:rows num-rows}
            (range 1 (inc max-pos)))))

(defn pegged?
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn add-peg
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  [board source destination]
  (add-peg (remove-peg board source) destination))

(defn valid-moves
  "Return a map of all valid moves for pos, where key is the destination and value is the jumped pos"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (pegged? board jumped)
                       (not (pegged? board destination))))
                (get-in board [pos :connections]))))

(defn valid-move?
  [board source destination]
  ((valid-moves board source) destination))

(defn make-move
  [board source destination]
  (when-let [jumped (valid-move? board source destination)]
    (remove-peg (move-peg board source destination) jumped)))

(defn can-move?
  [board]
  (some
    #(and (pegged? board %)
          (not (empty? (valid-moves board %))))
    (range 1 (inc (row-tri (:rows board))))))
