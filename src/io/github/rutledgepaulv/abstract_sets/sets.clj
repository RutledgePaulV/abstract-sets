(ns io.github.rutledgepaulv.abstract-sets.sets
  "Set algebra functions implemented on abstract sets and abstract sorted sets.
   Implementations leverage cardinality and sortedness to achieve efficient
   incremental APIs on the returned sets that defer accessing values until
   necessary."
  (:refer-clojure :exclude [conj disj empty empty?])
  (:require [io.github.rutledgepaulv.abstract-sets.protocols :as protos]
            [io.github.rutledgepaulv.abstract-sets.utils :as utils])
  (:import (clojure.lang PersistentTreeSet Reversible Seqable)))

(extend-protocol protos/AbstractSortedSet
  PersistentTreeSet
  (contains? [s x]
    (clojure.core/contains? s x))
  (max-cardinality [s]
    (count s))
  (min-cardinality [s]
    (count s))
  (starting [s anchor]
    (into (clojure.core/empty s) (.seqFrom s anchor true)))
  (stopping [s anchor]
    (into (clojure.core/empty s) (take (fn [x] (utils/lte x anchor))) s))
  (seq* [s]
    (seq s))
  (rseq* [s]
    (rseq s)))

(def empty-set
  (reify
    protos/AbstractSortedSet
    (contains? [_ _] false)
    (max-cardinality [_] 0)
    (min-cardinality [_] 0)
    (seq* [_] (seq ()))
    (starting [_ _] empty-set)
    (stopping [_ _] empty-set)
    (rseq* [_] (rseq ()))))

(defn intersection
  "Given two abstract sets, returns a new abstract set representing the intersection."
  [& sets]
  (let [os (sort-by protos/max-cardinality sets)]
    (reify

      protos/AbstractSortedSet
      (contains? [_ x]
        (reduce (fn [_ s] (if (protos/contains? s x) true (reduced false))) false os))

      (max-cardinality [_]
        (if (clojure.core/empty? sets)
          0
          (reduce (fn [min-max s] (min (protos/max-cardinality s) min-max)) Long/MAX_VALUE os)))

      (min-cardinality [_]
        0)

      (starting [this from]
        (apply intersection (mapv #(protos/starting % from) os)))

      (stopping [this from]
        (apply intersection (mapv #(protos/stopping % from) os)))

      (seq* [this]
        (->> [(mapv protos/seq* os) #{}]
             (iterate (fn [[seqs previous-values]]
                        ))
             (mapcat second)))

      (rseq* [this]
        (->> [(mapv protos/rseq* os) #{}]
             (iterate (fn [[seqs previous-values]]
                        ))
             (mapcat second))))))

(defn difference
  "Given two abstract sets, returns a new abstract set representing the difference."
  [a & more]
  (reify

    protos/AbstractSortedSet
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

    protos/AbstractSortedSet
    (starting [this anchor]
      (apply difference (protos/starting a) (map #(protos/starting % anchor) more)))

    (stopping [this anchor]
      (apply difference (protos/stopping a) (map #(protos/stopping % anchor) more)))

    (seq* [this]
      (->> [(into [(protos/seq* this)] (map protos/seq*) more) #{}]
           (iterate (fn [[seqs previous-values]]
                      ))
           (mapcat second)))

    (rseq* [this]
      (->> [(into [(protos/rseq* this)] (map protos/rseq*) more) #{}]
           (iterate (fn [[seqs previous-values]]
                      ))
           (mapcat second)))

    ))

(defn union
  "Given zero or more abstract sets, returns a new abstract set representing the union."
  [& sets]
  (reify

    protos/AbstractSortedSet
    (contains? [_ x]
      (reduce (fn [nf s] (if (protos/contains? s x) (reduced true) nf)) false sets))

    (max-cardinality [_]
      (reduce + 0 (map protos/max-cardinality sets)))

    (min-cardinality [_]
      (reduce (fn [max-min s] (max max-min (protos/min-cardinality s))) 0 sets))

    (starting [this from]
      (apply union (map #(protos/starting % from) sets)))

    (stopping [this from]
      (apply union (map #(protos/stopping % from) sets)))

    (seq* [this from]
      (->> [(mapv protos/seq* sets) #{}]
           (iterate (fn [[seqs previous-values]]
                      ))
           (mapcat second)))

    (rseq* [this from]
      (->> [(mapv protos/rseq* sets) #{}]
           (iterate (fn [[seqs previous-values]]
                      ))
           (mapcat second)))
    ))

(defn symmetric-difference
  "Given two abstract sets, return a new set containing only the disjoint elements."
  [a b]
  (reify

    protos/AbstractSortedSet
    (contains? [_ x]
      (case [(protos/contains? a x) (protos/contains? b x)]
        ([true true] [false false]) false
        ([true false] [false true]) true))

    (max-cardinality [_]
      (+ (protos/max-cardinality a) (protos/max-cardinality b)))

    (min-cardinality [_]
      0)

    protos/AbstractSortedSet
    (starting [this from]
      (symmetric-difference (protos/starting a from) (protos/starting b from)))

    (stopping [this from]
      (symmetric-difference (protos/stopping a from) (protos/stopping b from)))

    (seq* [this]
      )

    (rseq* [this]
      )))


(defn empty?
  "Is the abstract set empty?"
  [s]
  (and (zero? (protos/min-cardinality s))
       (reduce (constantly (reduced false)) true (protos/seq* s))))

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
        (protos/seq* head)))))

(defn cartesian-product
  "Returns two abstract sets, return a new abstract set representing the cartesian product."
  [a b]
  (reify

    protos/AbstractSortedSet
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

    (starting [this from]
      )

    (stopping [this from]
      )

    (seq* [this]
      )

    (rseq* [this]
      )))

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
    (reify protos/AbstractSortedSet
      (contains? [_ x]
        (or (clojure.core/contains? elements x) (protos/contains? s x)))
      (max-cardinality [_]
        (+ (protos/max-cardinality s) (count elements)))
      (min-cardinality [_]
        (protos/min-cardinality s))
      (starting [this anchor]
        )
      (stopping [this anchor]
        )
      (seq* [_]
        )
      (rseq* [_]
        ))))

(defn disj
  "Disjoins one or more values from an abstract set."
  [s & xs]
  (let [elements (set xs)]
    (reify protos/AbstractSortedSet
      (contains? [_ x]
        (and (not (clojure.core/contains? elements x))
             (protos/contains? s x)))
      (max-cardinality [_]
        (protos/max-cardinality s))
      (min-cardinality [_]
        (min 0 (- (protos/min-cardinality s) (count elements))))
      (starting [this anchor]
        )
      (stopping [this anchor]
        )
      (seq* [_]
        )
      (rseq* [_]
        ))))

(defn cardinality
  "Returns the actual cardinality of the set by counting the elements."
  [s]
  (let [min (protos/min-cardinality s)
        max (protos/max-cardinality s)]
    (if (= min max) max (reduce (fn [i _] (inc i)) 0 (protos/seq* s)))))


(defn realize
  "Creates a concrete set from an abstract set. Primarily for testing."
  [s]
  (into (sorted-set) (protos/seq* s)))
