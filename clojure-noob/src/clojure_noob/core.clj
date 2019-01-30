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

(def part-positions ["right-"
                     "top-"
                     "bottom-"
                     "back-"
                     "front-"])

(defn positioned-part
  "Returns `multiplier` matching body parts to the one given"
  [position part]
  {:name (clojure.string/replace (:name part) #"^left-" position)
   :size (:size part)})

(defn multiply-part [multiplier part]
  (let [positions (take multiplier part-positions)]
    (map (fn [position] (positioned-part position part)) positions)))

(defn multiply-body-parts [multiplier asym-parts]
  (reduce (fn [sym-parts part]
            (into sym-parts (set (conj (multiply-part multiplier part)))))
          []
          asym-parts))


(defn symmetrize-body-parts [asym-parts]
  (multiply-body-parts 2 asym-parts))


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

(comment "EXERCISES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

(defn mapset
  [fun coll]
  (set (map fun coll)))


(defn dec-maker [no]
  (fn [no2] (- no2 no)))




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot!"))
