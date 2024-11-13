(ns io.github.rutledgepaulv.abstract-sets.sets
  (:refer-clojure :exclude [conj disj empty empty?])
  (:require [io.github.rutledgepaulv.abstract-sets.protocols :as protos])
  (:import (clojure.lang IReduceInit)
 (java.util List Set)))


(extend-protocol protos/AbstractSet
  Set
  (contains? [s x]
    (.contains s x))
  (reducible [s]
    s)
  (max-cardinality [s]
    (count s))
  (min-cardinality [s]
    (count s))
  List
  (contains? [s x]
    (<= 0 (.indexOf s x)))
  (reducible [s]
    (eduction (distinct) s))
  (max-cardinality [s]
    (count s))
  (min-cardinality [s]
    (min 1 (count s))))

(defn empty
  "Create an empty abstract set."
  []
  (reify protos/AbstractSet
    (contains? [_ _] false)
    (max-cardinality [_] 0)
    (min-cardinality [_] 0)
    (reducible [_] (reify IReduceInit (reduce [this f init] init)))))

(defn intersection
  "Given two abstract sets, returns a new abstract set representing the intersection."
  [& sets]
  (let [os (sort-by protos/max-cardinality sets)]
    (reify

      protos/AbstractSet
      (contains? [_ x]
        (reduce (fn [_ s] (if (protos/contains? s x) true (reduced false))) false os))

      (max-cardinality [_]
        (if (clojure.core/empty? sets)
          0
          (reduce (fn [min-max s] (min (protos/max-cardinality s) min-max)) Long/MAX_VALUE os)))

      (min-cardinality [_]
        0)

      (reducible [_]
        (if (clojure.core/empty? os)
          (reify IReduceInit (reduce [this f init] init))
          (eduction
            (distinct)
            (reduce
              (fn [agg s]
                (eduction (filter (partial protos/contains? s)) agg))
              (protos/reducible (first os))
              (rest os))))))))

(defn difference
  "Given two abstract sets, returns a new abstract set representing the difference."
  [a & more]
  (reify

    protos/AbstractSet
    (contains? [_ x]
      (and (protos/contains? a x)
           (reduce (fn [_ s] (if (protos/contains? s x) (reduced false) true)) true more)))

    (max-cardinality [_]
      (protos/max-cardinality a))

    (min-cardinality [_]
      (if (clojure.core/empty? more)
        (protos/min-cardinality a)
        (let [min-a (protos/min-cardinality a)]
          (max 0 (- min-a (reduce + 0 (map protos/max-cardinality more)))))))

    (reducible [_]
      (eduction
        (distinct)
        (reduce
          (fn [agg s]
            (eduction (remove (partial protos/contains? s)) agg))
          (protos/reducible a)
          more)))))

(defn union
  "Given zero or more abstract sets, returns a new abstract set representing the union."
  [& sets]
  (reify

    protos/AbstractSet
    (contains? [_ x]
      (reduce (fn [nf s] (if (protos/contains? s x) (reduced true) nf)) false sets))

    (max-cardinality [_]
      (reduce + 0 (map protos/max-cardinality sets)))

    (min-cardinality [_]
      (reduce (fn [max-min s] (max max-min (protos/min-cardinality s))) 0 sets))

    (reducible [_]
      (eduction cat (distinct) (map protos/reducible sets)))))

(defn symmetric-difference
  "Given two abstract sets, return a new set containing only the disjoint elements."
  [a b]
  (reify

    protos/AbstractSet
    (contains? [_ x]
      (if (protos/contains? a x)
        (not (protos/contains? b x))
        (protos/contains? b x)))

    (max-cardinality [_]
      (+ (protos/max-cardinality a) (protos/max-cardinality b)))

    (min-cardinality [_]
      0)

    (reducible [_]
      (eduction cat [(eduction (remove (partial protos/contains? b)) (protos/reducible a))
                     (eduction (remove (partial protos/contains? a)) (protos/reducible b))]))))


(defn empty?
  "Is the abstract set empty?"
  [s]
  (and (zero? (protos/min-cardinality s))
       (reduce (constantly (reduced false)) true (protos/reducible s))))

(defn intersects?
  "Do all the sets share at least one element?"
  [& sets]
  (if (clojure.core/empty? sets)
    false
    (let [[head & rest] (sort-by protos/max-cardinality sets)]
      (reduce
        (fn [nf x]
          (if (reduce (fn [_ s] (if (protos/contains? s x) true (reduced false))) nf rest)
            (reduced true)
            nf))
        false
        (protos/reducible head)))))


(defn cartesian-product
  "Returns two abstract sets, return a new abstract set representing the cartesian product."
  [a b]
  (reify

    protos/AbstractSet
    (contains? [_ x]
      (and (sequential? x)
           (= 2 (count x))
           (let [[xa xb] x]
             (and (protos/contains? a xa)
                  (protos/contains? b xb)))))

    (max-cardinality [_]
      (* (protos/max-cardinality a) (protos/max-cardinality b)))

    (min-cardinality [_]
      (* (protos/min-cardinality a) (protos/min-cardinality b)))

    (reducible [_]
      (reify IReduceInit
        (reduce [this f init]
          (reduce
            (fn [agg xa]
              (reduce
                (fn [agg xb]
                  (f agg [xa xb]))
                agg
                (protos/reducible b)))
            init
            (protos/reducible a)))))))

(defn subset?
  "Is s1 a subset of s2?"
  [s1 s2]
  (reduce (fn [result x] (if (protos/contains? s2 x) result (reduced false))) true s1))

(defn superset?
  "Is s1 a subset of s2?"
  [s1 s2]
  (subset? s2 s1))

(defn conj
  "Conjoins one or more values to an abstract set."
  [s & xs]
  (let [elements (set xs)]
    (reify protos/AbstractSet
      (contains? [_ x]
        (or (clojure.core/contains? elements x) (protos/contains? s x)))
      (max-cardinality [_]
        (+ (protos/max-cardinality s) (count elements)))
      (min-cardinality [_]
        (protos/min-cardinality s))
      (reducible [_]
        (eduction cat (distinct) [elements (protos/reducible s)])))))

(defn disj
  "Disjoins one or more values from an abstract set."
  [s & xs]
  (let [elements (set xs)]
    (reify protos/AbstractSet
      (contains? [_ x]
        (and (not (clojure.core/contains? elements x))
             (protos/contains? s x)))
      (max-cardinality [_]
        (protos/max-cardinality s))
      (min-cardinality [_]
        (min 0 (- (protos/min-cardinality s) (count elements))))
      (reducible [_]
        (eduction (remove elements) (protos/reducible s))))))

(defn cardinality
  "Returns the actual cardinality of the set by counting the elements."
  [s]
  (let [min (protos/min-cardinality s)
        max (protos/max-cardinality s)]
    (if (= min max) max (reduce (fn [i _] (inc i)) 0 (protos/reducible s)))))

(defn realize
  "Creates a concrete set from an abstract set. Primarily for testing."
  [s]
  (into #{} (protos/reducible s)))
