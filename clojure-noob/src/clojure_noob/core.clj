(ns clojure-noob.core
  (:gen-class))

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "left-forearm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])


(defn matching-part
  "Returns the matching body part to the one given"
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts1
  "Expects a seq of parts that have :name and :size"
  [asym-parts]
  (loop [remaining-parts asym-parts
         sym-parts []]
    (if (empty? remaining-parts)
      sym-parts
      (let [[part & remaining] remaining-parts]
         (recur remaining
                (into sym-parts
                      (set [part (matching-part part)])))))))

(defn symmetrize-body-parts2 [asym-parts]
  (vec (set (into asym-parts
                  (map matching-part asym-parts)))))

(defn part-reducer [sym-parts part]
  (into sym-parts (set [part (matching-part part)])))

(defn symmetrize-body-parts3 [asym-parts]
  (reduce part-reducer [] asym-parts))

(defn symmetrize-body-parts [asym-parts]
  (reduce (fn [sym-parts part]
            (into sym-parts (set [part (matching-part part)])))
          []
          asym-parts))


(defn swole [body {:keys [name size]}]
  (concat body (take size (repeat name))))

(defn hit
  [asym-body-parts]
  (let [body (symmetrize-body-parts asym-hobbit-body-parts)
        swole-body (reduce (fn [swoler-body {:keys [size] :as part}]
                             (concat swoler-body (take size (repeat part))))
                             []
                             body)]
    (first (shuffle swole-body))))


(defn hit2
  [asym-body-parts]
  (let [body (symmetrize-body-parts asym-hobbit-body-parts)
        body-size (reduce + (map :size body))
        target (rand body-size)]
    (reduce (fn [remaining-target part]
              (let [part-size (:size part)]
                (if (<= remaining-target part-size)
                  (reduced part)
                  (- remaining-target part-size))))
            target
            body)))

(defn hit3
  [asym-body-parts]
  (let [body (symmetrize-body-parts asym-hobbit-body-parts)
        body-size (reduce + (map :size body))
        target (rand body-size)]
    (loop [[part & remaining] body
            accumulated-size (:size part)]
           (if (> accumulated-size target)
             part
             (recur remaining (+ accumulated-size (:size (first remaining))))))))




(defn handle-group [[group-name group-contents]]
  (let [num-hits (count group-contents)
        size (:size (first group-contents))
        hits-per-size (/ size num-hits)]
    [group-name num-hits size hits-per-size]))


(defn test-hits
  [hit-fn]
  (let [hits (take 100000 (repeatedly #(hit-fn asym-hobbit-body-parts)))
        groups (group-by :name hits)
        group-results (map handle-group groups)]
    (println (map float (map last group-results)))))








(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot!"))
