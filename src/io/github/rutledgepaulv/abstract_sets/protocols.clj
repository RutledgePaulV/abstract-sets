(ns io.github.rutledgepaulv.abstract-sets.protocols
  (:refer-clojure :exclude [contains? compare]))

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
  (starting [s anchor exclusive]
    "Returns a view of the set with a lower bound of `anchor`.")
  (stopping [s anchor exclusive]
    "Returns a view of the set with an upper bound of `anchor`."))

(defprotocol AbstractRelation
  (rows [rel] "Returns an abstract set containing the items of the relation as maps.")
  (cols [rel] "Returns a set containing the attributes of the relation.")
  (join [rel other] "Joins one relation with another, producing a new relation.")
  (union [rel other] "Unions the rows of one relation with another that has the same columns.")
  (intersection [rel other] "Intersects the rows of one relation with another that has the same columns.")
  (difference [rel other] "Subtracts the rows of another relation from one that has the same columns.")
  (project [rel columns] "Returns a new relation containing a subset of the original columns.")
  (select [rel pred] "Filters the rows of the relation using the predicate function pred."))
