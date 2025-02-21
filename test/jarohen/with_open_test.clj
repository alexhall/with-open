(ns jarohen.with-open-test
  (:require
   [clojure.test :refer [deftest is]]
   [jarohen.with-open :as with-open :refer [with-open+]]))

(deftest test-suppress-throwable
  (let [*result (atom nil)]
    (is (= 3
           (try
             (+ 1 2)
             (catch Throwable ex
               (with-open/suppress-throwable ex
                 (reset! *result :done))))))
    (is (nil? @*result)))
  
  (let [*result (atom nil)]
    (is (thrown? ArithmeticException
                 (try
                   (/ 1 0)
                   (catch Throwable ex
                     (with-open/suppress-throwable ex
                       (reset! *result :done))))))
    (is (= :done @*result)))

  (when-let [ex (is (thrown? ArithmeticException
                             (try
                               (/ 1 0)
                               (catch Throwable ex
                                 (with-open/suppress-throwable ex
                                   (.toString nil))))))]
    (is (= NullPointerException (class (first (.getSuppressed ex)))))))

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


(defrecord MockResource [value closed]
  with-open/Resource
  (-close [this]
    (reset! closed true)))

(deftest test-nil
  (try
    (with-open+ [r nil]
      (is false))
    (catch Exception e
      (is (= (ex-data e) {:resource nil}))
      (is (= (ex-message e) "Invalid resource passed to with-open+")))))

(deftest test-single-binding
  (let [closed (atom false)
        mock-resource (MockResource. "test" closed)]
    (with-open+ [r mock-resource]
      (is (= (:value r) "test"))
      (is (= @closed false)))
    (is (= (:value mock-resource) "test"))
    (is (= @closed true))))

(deftest test-multiple-bindings
  (let [counter (atom 0)]
    ;; Just checks they're run in order
    (with-open+ [r1 (MockResource. (swap! counter inc) (atom false))
                 r2 (MockResource. (swap! counter inc) (atom false))]
      (is (= (.value r1) 1))
      (is (= (.value r2) 2)))))

(deftest test-destructured-binding
  (with-open+ [{:keys [key]} (fn [cb] (cb {:key "test"}))]
    (is (= key "test"))))

(deftest test-callback
  ;; It doesn't actually close resources passed into a callback, but it pops the stack
  (let [closed (atom false)
        run-after (atom false)
        resource-cb (fn [cb]
                      (let [mock-resource (MockResource. "test" closed)]
                        (cb mock-resource)
                        (reset! run-after true)))]
    (with-open+ [r resource-cb]
      (is (= (.value r) "test"))
      (is (= @run-after false))
      (is (= @closed false)))
    (is (= @closed false))
    (is (= @run-after true))))

(deftest test-exception-in-body
  (let [mock-resource (MockResource. "test" (atom false))
        exception (try
                    (with-open+ [r mock-resource]
                      (throw (Exception. "Test Exception")))
                    (catch Exception e e))]
    (is (= (.getMessage exception) "Test Exception"))))

(deftest test-with-open+-try-finally
  (let [exception (try
                    (with-open+ [r (reify with-open/Resource
                                     (-close [_] (throw (Exception. "Finally Exception"))))]
                      (throw (Exception. "Body Exception")))
                    (catch Exception e e))]
    (is (= (.getMessage exception) "Body Exception"))
    (is (= (.getMessage (first (.getSuppressed exception))) "Finally Exception"))))

(deftest test-nested-resources
  (let [closed1 (atom false)
        closed2 (atom false)
        mock-resource1 (MockResource. "test1" closed1)
        mock-resource2 (MockResource. "test2" closed2)]
    (with-open+ [r1 mock-resource1]
      (with-open+ [r2 mock-resource2]
        (is (= (.value r1) "test1"))
        (is (= (.value r2) "test2"))
        (is (= @closed1 false))
        (is (= @closed2 false)))
      (is (= @closed1 false))
      (is (= @closed2 true)))
    (is (= @closed1 true))
    (is (= @closed2 true))))

;; (time (clojure.test/run-tests))
