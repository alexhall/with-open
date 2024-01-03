(ns jarohen.with-open-test
  (:require
   [clojure.test :refer [deftest is]]
   [jarohen.with-open :as with-open]
   [matcher-combinators.test]))

(deftest test-try-finally-no-exception
  (let [*result (atom nil)]
    (is (= 3
           (with-open/try-finally
            (+ 1 2)
            :finally
            (reset! *result :done))))
    (is (= :done
           @*result))))

(deftest test-try-finally-body-exception
  (let [*result (atom nil)]
    (is (thrown? ArithmeticException
                 (with-open/try-finally
                  (/ 1 0)
                  :finally
                  (reset! *result :done))))
    (is (= :done
           @*result))))

(deftest test-try-finally-finally-exception
  (let [*result (atom nil)]
    (is (thrown? NullPointerException
                 (with-open/try-finally
                  (+ 1 2)
                  :finally
                  (do
                    (.toString nil)
                    (reset! *result :done)))))
    (is (nil? @*result))))

(deftest test-try-finally-body-and-finally-exception
  (let [*result (atom nil)]
    (is (thrown? ArithmeticException
                 (with-open/try-finally
                  (/ 1 0)
                  :finally
                  (do
                    (.toString nil)
                    (reset! *result :done)))))
    (is (nil? @*result))

    ;; confirm that the exception from the finally block is included in the thrown exception
    (when-let [ex (is (thrown? Exception
                               (with-open/try-finally
                                (/ 1 0)
                                :finally
                                (.toString nil))))]
      (is (= NullPointerException (class (first (.getSuppressed ex))))))))

;; (time (run-tests))
