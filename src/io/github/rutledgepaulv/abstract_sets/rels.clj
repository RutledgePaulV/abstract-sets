(ns io.github.rutledgepaulv.abstract-sets.rels
  (:refer-clojure :exclude [conj disj empty])
  (:require [io.github.rutledgepaulv.abstract-sets.protocols :as protos]
            [io.github.rutledgepaulv.abstract-sets.sets :as sets])
  (:import (clojure.lang IPersistentMap)))

(extend-protocol protos/AbstractRelation
  IPersistentMap
  (cols [this] (:cols this))
  (rows [this] (:rows this)))

(defn sorted-relation
  [cols rows]
  (let [columns     (into (sorted-set) cols)
        extractor   (apply juxt columns)
        sorter      (fn [a b] (compare (extractor a) (extractor b)))
        sorted-rows (into (sorted-set-by sorter) rows)]
    (reify protos/AbstractRelation
      (cols [_] columns)
      (rows [_] sorted-rows))))

(defn relation
  "Create an abstract relation from abstract column and row sets."
  [cols rows]
  (reify protos/AbstractRelation
    (cols [_] cols)
    (rows [_] rows)))

(def empty-relation
  (reify protos/AbstractRelation
    (cols [_] (sorted-set))
    (rows [_] (sorted-set))))

(defn cartesian-product [rel1 rel2]
  (let [rel1-keys (protos/cols rel1)
        rel2-keys (protos/cols rel2)
        rel1-rows (protos/rows rel1)
        rel2-rows (protos/rows rel2)]
    (reify protos/AbstractSortedSet
      (max-cardinality [_]
        (* (protos/max-cardinality rel1-rows)
           (protos/max-cardinality rel2-rows)))
      (min-cardinality [_]
        (* (protos/min-cardinality rel1-rows)
           (protos/min-cardinality rel2-rows)))
      (contains? [this x]
        (and (protos/contains? rel1-rows (select-keys x (protos/seq* rel1-keys)))
             (protos/contains? rel2-rows (select-keys x (protos/seq* rel2-keys)))))
      (starting [this x exclusive]
        (cartesian-product
          (reify protos/AbstractRelation
            (cols [_] rel1-keys)
            (rows [_] (protos/starting rel1-rows x exclusive)))
          (reify protos/AbstractRelation
            (cols [_] rel2-keys)
            (rows [_] (protos/starting rel2-rows x exclusive)))))
      (stopping [this x exclusive]
        (cartesian-product
          (reify protos/AbstractRelation
            (cols [_] rel1-keys)
            (rows [_] (protos/stopping rel1-rows x exclusive)))
          (reify protos/AbstractRelation
            (cols [_] rel2-keys)
            (rows [_] (protos/stopping rel2-rows x exclusive)))))
      (seq* [_]
        (for [a (protos/seq* rel1-rows)
              b (protos/seq* rel2-rows)]
          (merge a b)))
      (rseq* [_]
        (for [a (protos/rseq* rel1-rows)
              b (protos/rseq* rel2-rows)]
          (merge a b))))))

(defn hash-join [rel1 rel2]
  (let [rel1-keys (protos/cols rel1)
        rel2-keys (protos/cols rel2)
        rel1-rows (protos/rows rel1)
        rel2-rows (protos/rows rel2)
        join-keys (sets/intersection rel1-keys rel2-keys)]
    (reify protos/AbstractSortedSet
      (max-cardinality [_]
        (min (protos/max-cardinality (protos/rows rel1))
             (protos/max-cardinality (protos/rows rel2))))
      (min-cardinality [_]
        0)
      (contains? [this x]
        (and (reduce (fn [result k] (if (contains? x k) result (reduced false))) true (protos/seq* join-keys))
             (protos/contains? rel1 (reduce (fn [m k] (assoc m k (get x k))) {} (protos/seq* rel1-keys)))
             (protos/contains? rel2 (reduce (fn [m k] (assoc m k (get x k))) {} (protos/seq* rel2-keys)))))
      (starting [this x exclusive]
        (hash-join
          (reify protos/AbstractRelation
            (cols [_] rel1-keys)
            (rows [_] (protos/starting rel1-rows x exclusive)))
          (reify protos/AbstractRelation
            (cols [_] rel2-keys)
            (rows [_] (protos/starting rel2-rows x exclusive)))))
      (stopping [this x exclusive]
        (hash-join
          (reify protos/AbstractRelation
            (cols [_] rel1-keys)
            (rows [_] (protos/stopping rel1-rows x exclusive)))
          (reify protos/AbstractRelation
            (cols [_] rel2-keys)
            (rows [_] (protos/stopping rel2-rows x exclusive)))))
      (seq* [_]
        ; TODO, when join-keys is a prefix of both relation sort orders
        ; then we can do an iterative join instead of a hash-join and save
        ; on memory usage
        (let [[bigger smaller]
              (if (< (protos/max-cardinality rel1-rows) (protos/max-cardinality rel2-rows)) [rel2-rows rel1-rows] [rel1-rows rel2-rows])
              join-table
              (reduce
                (fn [m x]
                  (let [join-key (reduce (fn [m k] (assoc m k (get x k))) {} (protos/seq* join-keys))]
                    (assoc m join-key x)))
                {}
                (protos/seq* smaller))]
          (keep
            (fn [x]
              (let [join-key (reduce (fn [m k] (assoc m k (get x k))) {} (protos/seq* join-keys))]
                (when-some [y (get join-table join-key)]
                  (merge x y))))
            (protos/seq* bigger))))
      (rseq* [_]
        (let [rows1
              (protos/rows rel1)
              rows2
              (protos/rows rel2)
              [bigger smaller]
              (if (< (protos/max-cardinality rows1) (protos/max-cardinality rows2)) [rows2 rows1] [rows1 rows2])
              join-table
              (reduce
                (fn [m x]
                  (let [join-key (reduce (fn [m k] (assoc m k (get x k))) {} (protos/seq* join-keys))]
                    (assoc m join-key x)))
                {}
                (protos/seq* smaller))]
          (keep
            (fn [x]
              (let [join-key (reduce (fn [m k] (assoc m k (get x k))) {} (protos/seq* join-keys))]
                (when-some [y (get join-table join-key)]
                  (merge x y))))
            (protos/rseq* bigger)))))))

(defn join
  "Given two abstract relations, return a new abstract relation representing their inner join."
  [rel1 rel2]
  (reify protos/AbstractRelation
    (cols [this]
      (sets/union (protos/cols rel1) (protos/cols rel2)))
    (rows [this]
      (let [rel1-keys (protos/cols rel1)
            rel2-keys (protos/cols rel2)
            join-keys (sets/intersection rel1-keys rel2-keys)]
        (if (sets/empty? join-keys)
          (cartesian-product rel1 rel2)
          (hash-join rel1 rel2))))))

(defn realize
  "Creates a concrete relation from an abstract relation. Primarily for testing."
  [rel]
  {:cols (into #{} (protos/seq* (protos/cols rel)))
   :rows (into #{} (protos/seq* (protos/rows rel)))})


(comment

  ; hash join
  (realize
    (join
      (sorted-relation #{:a :b :c} #{{:a 1 :b 2 :c 3} {:a 1 :b 3 :c 4}})
      (sorted-relation #{:a :b :d} #{{:a 1 :b 2 :d 4} {:a 1 :b 3 :d 6}})))

  ; cartesian product
  (realize
    (join
      (sorted-relation #{:a :b :c} #{{:a 1 :b 2 :c 3} {:a 2 :b 3 :c 4}})
      (sorted-relation #{:d :e :f} #{{:d 1 :e 2 :f 3} {:d 2 :e 3 :f 4}})))

  )
