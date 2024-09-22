(ns io.github.rutledgepaulv.abstract-sets.rels
  (:require [io.github.rutledgepaulv.abstract-sets.protocols :as protos])
  (:refer-clojure :exclude [conj disj empty]))



(defn project [relation columns]
  (reify protos/AbstractRelation
    (cols [this]
      )
    (rows [this])))
