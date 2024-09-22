(ns io.github.rutledgepaulv.abstract-sets.protocols
  (:refer-clojure :exclude [contains?]))


(defprotocol AbstractSet
  (max-cardinality [s]
    "Maximum possible cardinality for this set.")
  (min-cardinality [s]
    "Minimum possible cardinality for this set.")
  (contains? [s x]
    "Does the set contain the element `x`?")
  (reducible [s]
    "Returns something implementing clojure.lang.IReduceInit that can be reduced against to iterate the elements of the set."))


(defprotocol AbstractRelation
  (rows [rel] "Returns an abstract set containing the items of the relation as maps.")
  (cols [rel] "Returns an abstract set containing the attributes of the relation."))
