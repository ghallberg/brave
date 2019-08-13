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


 (map + [1 2 3] [4 5 3] [100 100 011])


(def sum #(reduce + %))
(def avg #(/ (sum %) (count %)))

(defn stats
  [numbers]
  (map #(% numbers) [sum count avg]))


(reduce (fn [new-map [key val]]
          (if (> val 4)
            (assoc new-map key val)
            new-map))
        {}
        {:four 4 :five 5 :six 6 :seven 7 :one 1 :two 2 :three 3 :fourfour 4.4 :twotwo 2.2 :threenine 3.9})



(defn redmap [fun coll]
  (reduce (fn [out in]
            (conj out (fun in)))
          []
          coll))

(defn even-numbers
  ([] (even-numbers 0))
  ([n] (cons n (lazy-seq (even-numbers (+ n 2))))))

(defn one-comp
  [f]
  (fn [& args]
    (apply f args)))

(defn two-comp
  [f g]
  (fn [& args]
    (f (apply (one-comp g) args))))

(defn three-comp
  [f g h]
  (fn [& args]
    (f (apply (two-comp g h) args))))

(defn my-comp
  ([] identity)
  ([f] f)
  ([f & more-fs]
   (fn [& args]
     (f (apply (apply my-comp more-fs) args)))))


(defn sum
  ([vals]
   (sum vals 0))
  ([vals acc]
   (if (empty? vals)
     acc
     (recur (rest vals) (+ (first vals) acc)))))


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

(defn row-tri2
  [row-no]
  (last (take row-no tri)))
