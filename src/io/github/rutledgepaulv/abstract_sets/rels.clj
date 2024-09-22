(ns io.github.rutledgepaulv.abstract-sets.rels
  (:require [io.github.rutledgepaulv.abstract-sets.protocols :as protos]
            [io.github.rutledgepaulv.abstract-sets.sets :as sets])
  (:refer-clojure :exclude [conj disj empty])
  (:import (clojure.lang IPersistentMap IReduceInit)))

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
  (reify protos/AbstractRelation
    (cols [this]
      (sets/intersection (protos/cols relation) columns))
    (rows [this]
      (reify protos/AbstractSet
        (max-cardinality [_]
          (protos/max-cardinality (protos/rows relation)))
        (min-cardinality [_]
          (protos/min-cardinality (protos/rows relation)))
        (contains? [this x]
          (reduce
            (fn [nf x']
              (if (= x' x)
                (reduced true)
                nf))
            false
            (protos/reducible this)))
        (reducible [_]
          (eduction
            (map
              (fn [x]
                (reduce
                  (fn [agg column]
                    (assoc agg column (get x column)))
                  {}
                  (protos/reducible (protos/cols this)))))
            (distinct)
            (protos/reducible (protos/rows relation))))))))


(defn union
  "Given two abstract relations, return a new abstract relation representing their union."
  [rel1 rel2]
  (reify protos/AbstractRelation
    (cols [this]
      (sets/union (protos/cols rel1) (protos/cols rel2)))
    (rows [this]
      (sets/union (protos/rows rel1) (protos/rows rel2)))))

(defn intersection
  "Given two abstract relations, return a new abstract relation representing their intersection."
  [rel1 rel2]
  (reify protos/AbstractRelation
    (cols [this]
      (sets/intersection (protos/cols rel1) (protos/cols rel2)))
    (rows [this]
      )))

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
        (reify protos/AbstractSet
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
                     (protos/reducible (protos/cols rel1))))
                 (protos/contains?
                   (protos/rows rel2)
                   (reduce
                     (fn [m k] (assoc m k (get x k)))
                     {}
                     (protos/reducible (protos/cols rel2))))))
          (reducible [_]
            (eduction (map (fn [[a b]] (merge a b))) (protos/reducible set))))))))

(defn join
  "Given two abstract relations, return a new abstract relation representing their inner join."
  [rel1 rel2]
  (reify protos/AbstractRelation
    (cols [this]
      (sets/intersection (protos/cols rel1) (protos/cols rel2)))
    (rows [this]
      (let [rel1-keys (protos/cols rel1)
            rel2-keys (protos/cols rel2)
            join-keys (sets/intersection rel1-keys rel2-keys)]
        (reify protos/AbstractSet
          (max-cardinality [_]
            (if (sets/empty? join-keys)
              (* (protos/max-cardinality (protos/rows rel1))
                 (protos/max-cardinality (protos/rows rel2)))
              (min (protos/max-cardinality (protos/rows rel1))
                   (protos/max-cardinality (protos/rows rel2)))))
          (min-cardinality [_]
            0)
          (contains? [this x]
            (and (reduce (fn [result k] (if (contains? x k) result (reduced false))) true (protos/reducible join-keys))
                 (protos/contains? rel1 (reduce (fn [m k] (assoc m k (get x k))) {} (protos/reducible rel1-keys)))
                 (protos/contains? rel2 (reduce (fn [m k] (assoc m k (get x k))) {} (protos/reducible rel2-keys)))))
          (reducible [_]
            (reify IReduceInit
              (reduce [this f init]
                (let [rows1
                      (protos/rows rel1)
                      rows2
                      (protos/rows rel2)
                      [bigger smaller]
                      (if (< (protos/max-cardinality rows1) (protos/max-cardinality rows2)) [rows2 rows1] [rows1 rows2])
                      join-table
                      (reduce
                        (fn [m x]
                          (let [join-key (reduce (fn [m k] (assoc m k (get x k))) {} (protos/reducible join-keys))]
                            (assoc m join-key x)))
                        {}
                        (protos/reducible smaller))]
                  (reduce
                    (fn [agg x]
                      (let [join-key (reduce (fn [m k] (assoc m k (get x k))) {} (protos/reducible join-keys))]
                        (if-some [y (get join-table join-key)]
                          (f agg (merge x y))
                          agg)))
                    init
                    (protos/reducible bigger)))))))))))


(defn realize
  "Creates a concrete relation from an abstract relation. Primarily for testing."
  [rel]
  {:cols (into #{} (protos/reducible (protos/cols rel)))
   :rows (into #{} (protos/reducible (protos/rows rel)))})

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
