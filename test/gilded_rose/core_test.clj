(ns gilded-rose.core-test
  (:require [clojure.test :refer :all]
            [gilded-rose.core :refer :all]))

(deftest gilded-rose-tests
  (testing "Can call API"
    (is (= (update-quality inventory) (progress-one-day inventory)))))
