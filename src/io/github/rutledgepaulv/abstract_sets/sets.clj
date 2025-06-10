(ns io.github.rutledgepaulv.abstract-sets.sets
  "Set algebra functions implemented on abstract sets and abstract sorted sets.
   Implementations leverage cardinality and sortedness to achieve efficient
   incremental APIs on the returned sets that defer accessing values until
   necessary."
  (:refer-clojure :exclude [conj disj empty empty?])
  (:require [io.github.rutledgepaulv.abstract-sets.protocols :as protos]
            [io.github.rutledgepaulv.abstract-sets.utils :as utils])
  (:import (clojure.lang PersistentTreeSet)))

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
    (into (clojure.core/empty s) (take-while (fn [x] (utils/lte x anchor))) s))
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
    (starting [this _] this)
    (stopping [this _] this)
    (rseq* [_] (rseq ()))))

(defn intersection
  "Given two abstract sets, returns a new abstract set representing the intersection."
  [& sets]
  (let [os sets]
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

      (starting [this anchor]
        (apply intersection (map #(protos/starting % anchor) os)))

      (stopping [this anchor]
        (apply intersection (map #(protos/stopping % anchor) os)))

      (seq* [this]
        (->> [(mapv protos/seq* os) #{}]
             (iterate (fn [[seqs]]
                        (let [lowest (utils/least (map first seqs))]
                          (loop [[head-seq & remaining-seqs :as sequences] seqs output-seqs [] unanimous true]
                            (cond
                              (clojure.core/empty? sequences)
                              [output-seqs (if unanimous #{lowest} #{})]
                              (= lowest (first head-seq))
                              (recur remaining-seqs (clojure.core/conj output-seqs (rest head-seq)) (and unanimous true))
                              :else
                              (recur remaining-seqs (clojure.core/conj output-seqs head-seq) false))))))
             (utils/taking-until-any-empty)))

      (rseq* [this]
        (->> [(mapv protos/rseq* os) #{}]
             (iterate (fn [[seqs]]
                        (let [highest (utils/greatest (map first seqs))]
                          (loop [[head-seq & remaining-seqs :as sequences] seqs output-seqs [] unanimous true]
                            (cond
                              (clojure.core/empty? sequences)
                              [output-seqs (if unanimous #{highest} #{})]
                              (= highest (first head-seq))
                              (recur remaining-seqs (clojure.core/conj output-seqs (rest head-seq)) (and unanimous true))
                              :else
                              (recur remaining-seqs (clojure.core/conj output-seqs head-seq) false))))))
             (utils/taking-until-any-empty))))))


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
      (apply difference (protos/starting a anchor) (map #(protos/starting % anchor) more)))

    (stopping [this anchor]
      (apply difference (protos/stopping a anchor) (map #(protos/stopping % anchor) more)))

    (seq* [this]
      (->> [(into [(protos/seq* a)] (map protos/seq*) more) ()]
           (iterate (fn [[[add & subtractions]]]
                      (if-some [v (first add)]
                        (let [scrolled (into []
                                             (comp (remove clojure.core/empty?)
                                                   (map (fn [s] (utils/scroll-until-max s v))))
                                             subtractions)]
                          (cond
                            (clojure.core/empty? scrolled)
                            [[] add]
                            (= (utils/least (map first scrolled)) v)
                            [(into [(rest add)] scrolled) ()]
                            :else
                            [(into [(rest add)] scrolled) (list v)]))
                        [() ()])))
           (utils/taking-until-first-empty)))

    (rseq* [this]
      (->> [(into [(protos/rseq* a)] (map protos/rseq*) more) #{}]
           (iterate (fn [[[add & subtractions]]]
                      (if-some [v (first add)]
                        (let [scrolled (into []
                                             (comp (remove clojure.core/empty?)
                                                   (map (fn [s] (utils/scroll-until-min s v))))
                                             subtractions)]
                          (cond
                            (clojure.core/empty? scrolled)
                            [[] add]
                            (= (utils/greatest (map first scrolled)) v)
                            [(into [(rest add)] scrolled) ()]
                            :else
                            [(into [(rest add)] scrolled) (list v)]))
                        [() ()])))
           (utils/taking-until-first-empty)))))

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

    (starting [this anchor]
      (apply union (map #(protos/starting % anchor) sets)))

    (stopping [this anchor]
      (apply union (map #(protos/stopping % anchor) sets)))

    (seq* [this]
      (->> [(map protos/seq* sets) ()]
           (iterate (fn [[seqs :as input]]
                      (let [lowest (utils/least (map first seqs))]
                        (loop [[head-seq & remaining-seqs :as sequences] seqs output-seqs []]
                          (cond
                            (clojure.core/empty? sequences)
                            [output-seqs (list lowest)]
                            (nil? (first head-seq))
                            (recur remaining-seqs output-seqs)
                            (= lowest (first head-seq))
                            (recur remaining-seqs
                                   (cond-> output-seqs
                                     (seq (rest head-seq))
                                     (clojure.core/conj (rest head-seq))))
                            :else
                            (recur remaining-seqs (clojure.core/conj output-seqs head-seq)))))))
           (utils/taking-until-all-empty)))

    (rseq* [this]
      (->> [(map protos/rseq* sets) #{}]
           (iterate (fn [[seqs]]
                      (let [highest (utils/greatest (map first seqs))]
                        (loop [[head-seq & remaining-seqs :as sequences] seqs output-seqs []]
                          (cond
                            (clojure.core/empty? sequences)
                            [output-seqs (list highest)]
                            (nil? (first head-seq))
                            (recur remaining-seqs output-seqs)
                            (= highest (first head-seq))
                            (recur remaining-seqs
                                   (cond-> output-seqs
                                     (seq (rest head-seq))
                                     (clojure.core/conj (rest head-seq))))
                            :else
                            (recur remaining-seqs (clojure.core/conj output-seqs head-seq)))))))
           (utils/taking-until-all-empty)))))

(defn symmetric-difference
  "Given two abstract sets, return a new set containing only the disjoint elements."
  [a b]
  (difference (union a b) (intersection a b)))

(defn empty?
  "Is the abstract set empty?"
  [s]
  (or (= 0 (protos/max-cardinality s))
      (and (zero? (protos/min-cardinality s))
           (empty? (protos/seq* s)))))

(defn intersects?
  "Do all the sets share at least one element?"
  [& sets]
  (some? (clojure.core/seq (protos/seq* (apply intersection sets)))))

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

    (starting [this [start-a start-b]]
      (throw (ex-info "Not implemented yet." {})))

    (stopping [this [stop-a stop-b]]
      (throw (ex-info "Not implemented yet." {})))

    (seq* [this]
      (for [a (protos/seq* a)
            b (protos/seq* b)]
        [a b]))

    (rseq* [this]
      (for [a (protos/rseq* a)
            b (protos/rseq* b)]
        [a b]))))

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
  (union s (into (sorted-set) xs)))

(defn disj
  "Disjoins one or more values from an abstract set."
  [s & xs]
  (difference s (into (sorted-set) xs)))

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
