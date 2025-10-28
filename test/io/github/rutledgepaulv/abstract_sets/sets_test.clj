(ns io.github.rutledgepaulv.abstract-sets.sets-test
  (:require [clojure.test :refer :all]
            [io.github.rutledgepaulv.abstract-sets.sets :as sets]))


(deftest subset?-test
  (is (sets/subset? (sorted-set 2) (sorted-set 2 3)))
  (is (sets/subset? #{} (sorted-set 2 3)))
  (is (not (sets/subset? (sorted-set 4) (sorted-set 2 3)))))

(deftest intersection-test
  (is (= #{} (sets/realize (sets/intersection (sorted-set 2) (sorted-set 3)))))
  (is (= (sorted-set 2) (sets/realize (sets/intersection (sorted-set 2) (sorted-set 2)))))
  (is (= (sorted-set 3) (sets/realize (sets/intersection (sorted-set 2 3) (sorted-set 3 4))))))

(deftest union-test
  (is (= (sorted-set 2 3) (sets/realize (sets/union (sorted-set 2) (sorted-set 3)))))
  (is (= (sorted-set 2) (sets/realize (sets/union (sorted-set 2) (sorted-set 2)))))
  (is (= (sorted-set 2 3 4) (sets/realize (sets/union (sorted-set 2 3) (sorted-set 3 4))))))

(deftest difference-test
  (is (= #{} (sets/realize (sets/difference (sorted-set 2) (sorted-set 2)))))
  (is (= (sorted-set 4 5) (sets/realize (sets/difference (sorted-set 3 4 5) (sorted-set 3)))))
  (is (= (sorted-set 1) (sets/realize (sets/difference (sorted-set 1) (sorted-set 3 4 5))))))

(deftest cartesian-product-test
  (is (= #{} (sets/realize (sets/cartesian-product (sorted-set) (sorted-set)))))
  (is (= (sorted-set [1 1]) (sets/realize (sets/cartesian-product (sorted-set 1) (sorted-set 1)))))
  (is (= (sorted-set [2 3] [1 4] [1 3] [2 4]) (sets/realize (sets/cartesian-product (sorted-set 1 2) (sorted-set 3 4))))))

(deftest symmetric-difference-test
  (is (= (sorted-set 1 3) (sets/realize (sets/symmetric-difference (sorted-set 1 2) (sorted-set 2 3)))))
  (is (= (sorted-set 1 4 3 2) (sets/realize (sets/symmetric-difference (sorted-set 1 2) (sorted-set 3 4))))))

(deftest intersects?-test
  (is (sets/intersects? (sorted-set 1) (sorted-set 1 2)))
  (is (not (sets/intersects? (sorted-set) (sorted-set 1 2))))
  (is (not (sets/intersects? (sorted-set 3) (sorted-set 1 2)))))

(deftest cardinality-test
  (is (= 0 (sets/cardinality (sorted-set))))
  (is (= 1 (sets/cardinality (sorted-set 1)))))

(deftest empty-test
  (is (= #{} (sets/realize sets/empty-set))))

(deftest empty?-test
  (is (sets/empty? (sorted-set))))
