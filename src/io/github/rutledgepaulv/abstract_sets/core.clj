(ns io.github.rutledgepaulv.abstract-sets.core
  (:require [io.github.rutledgepaulv.abstract-sets.protocols :as protos])
  (:import (java.util List Set)))


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
    (contains? [s x]
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
    (contains? [s x]
      (and (protos/contains? a x) (not (protos/contains? b x))))

    (max-cardinality [s]
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
    (contains? [s x]
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
