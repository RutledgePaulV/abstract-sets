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
            ([[fa a]] a)
            ([[fa a] [fb b]] (if (lt fa fb) a b)))]
    (if (= 1 (bounded-count 2 coll))
      (first coll)
      (reduce inner-least (map (juxt f identity) coll)))))

(defn greatest-by
  "Returns the largest element according to some fn of the element"
  [f coll]
  (letfn [(inner-greatest
            ([] nil)
            ([[fa a]] a)
            ([[fa a] [fb b]] (if (gt fa fb) a b)))]
    (if (= 1 (bounded-count 2 coll))
      (first coll)
      (reduce inner-greatest (map (juxt f identity) coll)))))

(defn least [coll]
  (least-by identity coll))

(defn greatest [coll]
  (greatest-by identity coll))

(defn scroll-until-max [coll max]
  (drop-while (fn [x] (lt x max)) coll))

(defn scroll-until-min [coll min]
  (drop-while (fn [x] (lt min x)) coll))

(defn take-upto
  "Returns a lazy sequence of successive items from coll up to and including
  the first item for which `(pred item)` returns true."
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result x]
        (let [result (rf result x)]
          (if (pred x)
            (ensure-reduced result)
            result))))))
  ([pred coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [x (first s)]
         (cons x (if-not (pred x) (take-upto pred (rest s)))))))))


(defn taking-until-any-empty [coll]
  (->> coll
       (take-upto
         (fn [[seqs]]
           (reduce (fn [nf x]
                     (if (clojure.core/empty? x)
                       (reduced true)
                       nf))
                   false seqs)))
       (mapcat second)))

(defn taking-until-all-empty [coll]
  (->> coll
       (take-upto (fn [[seqs]] (every? empty? seqs)))
       (mapcat second)))

(defn taking-until-first-empty [coll]
  (->> coll
       (take-upto (fn [[seqs]] (empty? (first seqs))))
       (mapcat second)))
