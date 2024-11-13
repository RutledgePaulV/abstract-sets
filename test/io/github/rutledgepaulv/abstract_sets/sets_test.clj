(ns io.github.rutledgepaulv.abstract-sets.sets-test
  (:require [clojure.test :refer :all]
            [io.github.rutledgepaulv.abstract-sets.sets :as sets]))


(deftest subset?-test
  (is (sets/subset? #{2} #{2 3}))
  (is (sets/subset? #{} #{2 3}))
  (is (not (sets/subset? #{4} #{2 3})))
  (is (sets/subset? [2] [2 3]))
  (is (sets/subset? [] [2 3]))
  (is (not (sets/subset? [4] [2 3]))))

(deftest intersection-test
  (is (= #{} (sets/realize (sets/intersection #{2} #{3}))))
  (is (= #{2} (sets/realize (sets/intersection #{2} #{2}))))
  (is (= #{3} (sets/realize (sets/intersection #{2 3} #{3 4})))))

(deftest union-test
  (is (= #{2 3} (sets/realize (sets/union #{2} #{3}))))
  (is (= #{2} (sets/realize (sets/union #{2} #{2}))))
  (is (= #{2 3 4} (sets/realize (sets/union #{2 3} #{3 4})))))

(deftest difference-test
  (is (= #{} (sets/realize (sets/difference #{2} #{2}))))
  (is (= #{4 5} (sets/realize (sets/difference #{3 4 5} #{3}))))
  (is (= #{1} (sets/realize (sets/difference #{1} #{3 4 5})))))

(deftest cartesian-product-test
  (is (= #{} (sets/realize (sets/cartesian-product #{} #{}))))
  (is (= #{[1 1]} (sets/realize (sets/cartesian-product #{1} #{1}))))
  (is (= #{[2 3] [1 4] [1 3] [2 4]} (sets/realize (sets/cartesian-product #{1 2} #{3 4})))))

(deftest symmetric-difference-test
  (is (= #{1 3} (sets/realize (sets/symmetric-difference #{1 2} #{2 3}))))
  (is (= #{1 4 3 2} (sets/realize (sets/symmetric-difference #{1 2} #{3 4})))))

(deftest intersects?-test
  (is (sets/intersects? #{1} #{1 2}))
  (is (not (sets/intersects? #{} #{1 2})))
  (is (not (sets/intersects? #{3} #{1 2}))))

(deftest cardinality-test
  (is (= 0 (sets/cardinality #{})))
  (is (= 1 (sets/cardinality #{1}))))

(deftest empty-test
  (is (= #{} (sets/realize (sets/empty)))))

(deftest empty?-test
  (is (sets/empty? #{})))
