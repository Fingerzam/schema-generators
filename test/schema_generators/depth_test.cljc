(ns schema-generators.depth-test
  #?(:clj (:use clojure.test))
  (:require
   #?(:cljs [cljs.test :refer-macros [deftest is testing run-tests]])
   [schema.core :as s]
   [clojure.test.check.generators :as gen]
   [clojure.test.check :as tc]
   [schema-generators.schema-depth :refer [depth]]))

(deftest schema-depth-test
  (testing "Schema depth"
    (testing "simple schemas"
      (is (= 0 (depth s/Int)))
      (is (= 1 (depth s/Str))))
    (testing "single parameter wrapper schemas"
      (is (= 0 (depth (s/maybe s/Int))))
      (is (= 1 (depth (s/atom s/Str))))
      (is (= 0 (depth (s/named s/Bool "name")))))
    (testing "schemas with multiple parameter schemas"
      (is (= 0 (depth (s/either s/Int s/Bool))))
      (is (= 1 (depth (s/either s/Int s/Str))))
      (is (= 1 (depth (s/conditional integer? s/Int string? s/Str))))
      (is (= 1 (depth (s/cond-pre s/Int s/Str s/Bool))))
      (is (= 0 (depth (s/both s/Int s/Int)))))
    (testing "collection schema helpers"
      (testing "required keys"
        (is (= 0 (depth (s/required-key "test-key"))))
        (is (= 0 (depth :test-key))))
      (testing "optional keys"
        (is (= 0 (depth (s/optional-key :test-key)))))
      (testing "map entries"
        (is (= 0 (depth (s/map-entry :test-key s/Int))))
        (is (= 1 (depth (s/map-entry :test-key s/Str))))
        (is (= 1 (depth (s/map-entry s/Str s/Str))))
        (is (= 1 (depth (s/map-entry s/Keyword s/Int)))))
      (testing "required elements in sequences"
        (is (= 1 (depth (s/one s/Str "required element in a sequence"))))
        (is (= 0 (depth (s/one s/Int "required element in a sequence")))))
      (testing "optional elements in sequences"
        (is (= 1 (depth (s/optional s/Keyword "optional element in a sequence"))))
        (is (= 0 (depth (s/optional s/Bool "optional element in a sequence"))))))
    (testing "collection schemas"
      (testing "sets"
        (is (= 1 (depth #{s/Int})))
        (is (= 2 (depth #{s/Str})))
        (is (= 2 (depth #{#{s/Int}})))
        (is (= 3 (depth #{#{s/Str}}))))
      (testing "vectors/sequences"
        (is (= 1 (depth [s/Int])))
        (is (= 3 (depth [[s/Str]])))
        (is (= 0 (depth [(s/one s/Int "required1") (s/one s/Bool "required2")])))
        (is (= 1 (depth [(s/one s/Str "required") (s/optional s/Int "optional")])))
        (is (= 1 (depth [(s/one s/Int "required") (s/optional s/Str "optional")])))
        (is (= 1 (depth [(s/one s/Str "required") (s/optional s/Str "optional")])))
        (is (= 2 (depth [(s/one s/Str "required") (s/optional s/Int "optional") s/Str]))))
      (testing "maps"
        (is (= 0 (depth {:test-key s/Int
                         :another s/Bool
                         (s/optional-key "third key") s/Int})))
        (is (= 1 (depth {:test-key s/Str})))
        (is (= 2 (depth {:test-key s/Int
                         s/Keyword s/Int})))
        (is (= 2 (depth {:test-key [s/Str]})))
        )
      )))
