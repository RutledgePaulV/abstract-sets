(ns io.github.rutledgepaulv.abstract-sets.rels
  (:refer-clojure :exclude [conj disj empty])
  (:require [io.github.rutledgepaulv.abstract-sets.protocols :as protos]
            [io.github.rutledgepaulv.abstract-sets.sets :as sets])
  (:import (clojure.lang IPersistentMap)))

(extend-protocol protos/AbstractRelation
  IPersistentMap
  (cols [this] (:cols this))
  (rows [this] (:rows this)))

(defn relation
  "Create an abstract relation from abstract column and row sets."
  ([] (relation #{} #{}))
  ([cols rows]
   (reify protos/AbstractRelation
     (cols [_] cols)
     (rows [_] rows))))

(defn project
  "Given a relation and an abstract set of columns, return a new relation containing only those columns which intersected."
  [relation columns]
  (let [cols (sets/intersection (protos/cols relation) columns)
        rows (protos/rows relation)]
    (reify protos/AbstractRelation
      (cols [this] cols)
      (rows [this]
        (reify protos/AbstractSortedSet
          (max-cardinality [_]
            (protos/max-cardinality rows))
          (min-cardinality [_]
            (protos/min-cardinality rows))
          (contains? [this x]
            (let [[subset? subset]
                  (reduce
                    (fn [[subset? m] k]
                      (if-some [entry (find x k)]
                        [subset? (assoc m (key entry) (val entry))]
                        (reduced [false m])))
                    [true {}]
                    (protos/seq* cols))]
              (and subset?
                   (reduce
                     (fn [nf x']
                       (if (= x' subset)
                         (reduced true)
                         nf))
                     false
                     (protos/seq* this)))))
          (reducible [_]
            (eduction
              (map
                (fn [x]
                  (reduce
                    (fn [agg column]
                      (assoc agg column (get x column)))
                    {}
                    (protos/seq* cols))))
              (distinct)
              (protos/seq* rows))))))))


(defn union
  "Given two abstract relations, return a new abstract relation representing their union."
  [rel1 rel2]
  (reify protos/AbstractRelation
    (cols [this]
      (protos/cols rel1))
    (rows [this]
      (sets/union (protos/rows rel1) (protos/rows rel2)))))

(defn intersection
  "Given two abstract relations, return a new abstract relation representing their intersection."
  [rel1 rel2]
  (reify protos/AbstractRelation
    (cols [this]
      (protos/cols rel1))
    (rows [this]
      (sets/intersection (protos/rows rel1) (protos/rows rel2)))))

(defn difference
  "Given two abstract relations, return a new abstract relation representing their difference."
  [rel1 rel2]
  (reify protos/AbstractRelation
    (cols [this]
      (protos/cols rel1))
    (rows [this]
      (sets/difference (protos/rows rel1) (protos/rows rel2)))))

(defn cartesian-product
  "Given two abstract relations, return a new abstract relation representing their cartesian product."
  [rel1 rel2]
  (reify protos/AbstractRelation
    (cols [this]
      (sets/union (protos/cols rel1) (protos/cols rel2)))
    (rows [this]
      (let [set (sets/cartesian-product (protos/rows rel1) (protos/rows rel2))]
        (reify protos/AbstractSortedSet
          (max-cardinality [_]
            (protos/max-cardinality set))
          (min-cardinality [_]
            (protos/min-cardinality set))
          (contains? [_ x]
            (and (protos/contains?
                   (protos/rows rel1)
                   (reduce
                     (fn [m k] (assoc m k (get x k)))
                     {}
                     (protos/seq* (protos/cols rel1))))
                 (protos/contains?
                   (protos/rows rel2)
                   (reduce
                     (fn [m k] (assoc m k (get x k)))
                     {}
                     (protos/seq* (protos/cols rel2))))))
          (seq* [_]
            (eduction
              (map (fn [[a b]] (merge a b)))
              (distinct)
              (protos/seq* set))))))))

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
          (sets/cartesian-product (protos/rows rel1) (protos/rows rel2))
          (reify protos/AbstractSortedSet
            (max-cardinality [_]
              (if (sets/empty? join-keys)
                (* (protos/max-cardinality (protos/rows rel1))
                   (protos/max-cardinality (protos/rows rel2)))
                (min (protos/max-cardinality (protos/rows rel1))
                     (protos/max-cardinality (protos/rows rel2)))))
            (min-cardinality [_]
              0)
            (contains? [this x]
              (and (reduce (fn [result k] (if (contains? x k) result (reduced false))) true (protos/seq* join-keys))
                   (protos/contains? rel1 (reduce (fn [m k] (assoc m k (get x k))) {} (protos/seq* rel1-keys)))
                   (protos/contains? rel2 (reduce (fn [m k] (assoc m k (get x k))) {} (protos/seq* rel2-keys)))))
            (seq* [_]
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
                  (protos/seq* bigger))))))))))


(defn realize
  "Creates a concrete relation from an abstract relation. Primarily for testing."
  [rel]
  {:cols (into #{} (protos/seq* (protos/cols rel)))
   :rows (into #{} (protos/seq* (protos/rows rel)))})

(comment

  (def rel1 (relation #{:a :b :c} #{{:a 1 :b 2 :c 3} {:a 1 :b 3 :c 4}}))
  (def rel2 (relation #{:a :b :d} #{{:a 1 :b 2 :d 4} {:a 1 :b 3 :d 6}}))
  (def joined (join rel1 rel2))

  (realize
    (cartesian-product
      (relation #{:a :b} #{{:a 1 :b 2} {:a 2 :b 3}})
      (relation #{:c :d} #{{:c 1 :d 2} {:c 2 :d 3}})
      ))

  (protos/contains?
    (protos/rows
      (cartesian-product
        (relation #{:a :b} #{{:a 1 :b 2} {:a 2 :b 3}})
        (relation #{:c :d} #{{:c 1 :d 2} {:c 2 :d 3}})))
    {:a 2, :b 3, :c 2, :d 3}
    )

  )
