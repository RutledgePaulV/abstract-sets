(ns io.github.rutledgepaulv.abstract-sets.utils
  (:require [clojure.test :refer :all]))


(defn lt
  "Like < but for comparables."
  ([] true)
  ([_] true)
  ([a b] (neg? (compare a b)))
  ([a b & more]
   (if (lt a b)
     (if (next more)
       (recur b (first more) (next more))
       (lt b (first more)))
     false)))

(defn lte
  "Like <= but for comparables."
  ([] true)
  ([_] true)
  ([a b] (not (pos? (compare a b))))
  ([a b & more]
   (if (lte a b)
     (if (next more)
       (recur b (first more) (next more))
       (lte b (first more)))
     false)))

(defn gt
  "Like > but for comparables."
  ([] true)
  ([_] true)
  ([a b] (pos? (compare a b)))
  ([a b & more]
   (if (gt a b)
     (if (next more)
       (recur b (first more) (next more))
       (gt b (first more)))
     false)))

(defn gte
  "Like >= but for comparables."
  ([] true)
  ([_] true)
  ([a b] (not (neg? (compare a b))))
  ([a b & more]
   (if (gte a b)
     (if (next more)
       (recur b (first more) (next more))
       (gte b (first more)))
     false)))

(defn least-by
  "Returns the smallest element according to some fn of the element"
  [f coll]
  (letfn [(inner-least
            ([] nil)
            ([a] a)
            ([[fa a] [fb b]] (if (lt fa fb) a b)))]
    (reduce inner-least (map (juxt identity f) coll))))

(defn greatest-by
  "Returns the largest element according to some fn of the element"
  [f coll]
  (letfn [(inner-greatest
            ([] nil)
            ([a] a)
            ([[fa a] [fb b]] (if (gt fa fb) a b)))]
    (reduce inner-greatest (map (juxt identity f) coll))))

(defn least [coll]
  (least-by identity coll))

(defn greatest [coll]
  (greatest-by identity coll))
