(ns io.github.rutledgepaulv.abstract-sets.sets
  (:require [io.github.rutledgepaulv.abstract-sets.protocols :as protos])
  (:import (clojure.lang IReduceInit)
           (java.util List Set))
  (:refer-clojure :exclude [conj disj empty empty?]))


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

(defn intersection
  "Given two abstract sets, returns a new abstract set representing the intersection."
  [a b]
  (reify

    protos/AbstractSet
    (contains? [_ x]
      (and (protos/contains? a x) (protos/contains? b x)))

    (max-cardinality [_]
      (min (protos/max-cardinality a) (protos/max-cardinality b)))

    (min-cardinality [_]
      0)

    (reducible [_]
      (let [[bigger smaller] (if (< (protos/max-cardinality a) (protos/max-cardinality b)) [b a] [a b])]
        (eduction (filter (partial protos/contains? bigger)) (protos/reducible smaller))))))

(defn difference
  "Given two abstract sets, returns a new abstract set representing the difference."
  [a b]
  (reify

    protos/AbstractSet
    (contains? [_ x]
      (and (protos/contains? a x) (not (protos/contains? b x))))

    (max-cardinality [_]
      (protos/max-cardinality a))

    (min-cardinality [_]
      0)

    (reducible [_]
      (eduction (remove (partial protos/contains? b)) (protos/reducible a)))))

(defn union
  "Given two abstract sets, returns a new abstract set representing the union."
  [a b]
  (reify

    protos/AbstractSet
    (contains? [_ x]
      (or (protos/contains? a x) (protos/contains? b x)))

    (max-cardinality [_]
      (+ (protos/max-cardinality a) (protos/max-cardinality b)))

    (min-cardinality [_]
      (max (protos/min-cardinality a) (protos/max-cardinality b)))

    (reducible [_]
      (eduction cat (distinct) [(protos/reducible a) (protos/reducible b)]))))

(defn symmetric-difference
  "Given two abstract sets, return a new set containing only the disjoint elements."
  [a b]
  (reify

    protos/AbstractSet
    (contains? [_ x]
      (case [(protos/contains? a x) (protos/contains? b x)]
        ([true false] [false true]) true
        false))

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
  "Do a and b share at least one element?"
  [a b]
  (let [[bigger smaller] (if (< (protos/max-cardinality a) (protos/max-cardinality b)) [b a] [a b])]
    (reduce (fn [nf x] (if (protos/contains? bigger x) (reduced true) nf)) false (protos/reducible smaller))))


(defn cartesian-product
  "Returns two abstract sets, return a new abstract set representing the cartesian product."
  [a b]
  (reify

    protos/AbstractSet
    (contains? [_ x]
      (and (vector? x)
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

(defn empty
  "Create an empty abstract set."
  []
  (reify protos/AbstractSet
    (contains? [_ _] false)
    (max-cardinality [_] 0)
    (min-cardinality [_] 0)
    (reducible [_] (reify IReduceInit (reduce [this f init] init)))))

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
  ([] (empty))
  ([s & xs]
   (let [elements (set xs)]
     (reify protos/AbstractSet
       (contains? [_ x]
         (or (clojure.core/contains? elements x) (protos/contains? s x)))
       (max-cardinality [_]
         (+ (protos/max-cardinality s) (count elements)))
       (min-cardinality [_]
         (protos/min-cardinality s))
       (reducible [_]
         (eduction cat [elements (protos/reducible s)]))))))

(defn disj
  "Disjoins one or more values from an abstract set."
  ([] (empty))
  ([s & xs]
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
         (eduction (remove elements) (protos/reducible s)))))))

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
