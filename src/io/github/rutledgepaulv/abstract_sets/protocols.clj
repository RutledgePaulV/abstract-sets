(ns io.github.rutledgepaulv.abstract-sets.protocols
  (:refer-clojure :exclude [contains?]))

(defprotocol AbstractSortedSet
  (max-cardinality [s]
    "Maximum possible cardinality for this set.")
  (min-cardinality [s]
    "Minimum possible cardinality for this set.")
  (contains? [s x]
    "Does the set contain the element `x`?")
  (seq* [s]
    "Returns an iterator producing the elements of the set in an undefined order.")
  (rseq* [s]
    "Returns an iterator producing the elements of the set in reverse order.")
  (starting [s anchor]
    "Returns a view of the set with a lower bound of `anchor`.")
  (stopping [s anchor]
    "Returns a view of the set with an upper bound of `anchor`."))

(defprotocol AbstractRelation
  (rows [rel] "Returns an abstract set containing the items of the relation as maps.")
  (cols [rel] "Returns an abstract set containing the attributes of the relation."))
