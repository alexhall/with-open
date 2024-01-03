(ns jarohen.with-open
  (:import
   [java.io Closeable]
   [java.lang AutoCloseable]))

(set! *warn-on-reflection* true)

(defmacro try-finally
  "Use try/finally such that an exception in the finally clause does not
  mask a prior exception in the try block. The first arg is a form to be
  evaluated in the main body of the `try`; the second arg is an unused marker
  for readability (the suggested value is `:finally`); the third arg is a
  form to be evaluated in the finally block of the `try`.

  Example usage:
  ```
  (try-finally
    (/ 1 0)
    :finally
    (println \"Cleaning up\"))
  ```"
  [body _ finally-body]
  `(let [*e# (atom nil)]
     (try
       ~body
       (catch Throwable e#
         (reset! *e# e#))
       (finally
         (try
           ~finally-body
           (catch Throwable e2#
             ;; if an exception was thrown by the first try body and by the first finally block then avoid masking the
             ;; first exception
             (if @*e#
               ;; add the new exception to the prior exception
               (.addSuppressed ^Throwable @*e# e2#)
               (throw e2#)))
           (finally
             (when @*e#
               (throw @*e#))))))))

(defprotocol Resource
  "Represents a resource with state that should be closed when no longer needed."
  (-close [resource]))

(extend-protocol Resource
  Closeable
  (-close [this] (.close this))

  AutoCloseable
  (-close [this] (.close this)))

(defn _with-open [resource f]
  (cond
    (satisfies? Resource resource) (try-finally
                                    (f resource)
                                    :finally
                                    (-close resource))

    (fn? resource) (resource f)
    :else (throw (ex-info "Invalid resource passed to with-open+" {:resource resource}))))

(defmacro with-open+
  "Like `clojure.core/with-open` this evaluates the body in a try expression with
  the provided bindings that are cleaned up in a finally clause, but provides some
  additional features:
  - Bindings support destructuring forms and not just raw names.
  - Arbitrary types can be treated as resources by wrapping them in higher-order
    functions (or extending the Resource protocol).
  - Exceptions thrown in the finally clause will not mask previous exceptions thrown
    from the body (or inner finally clauses)."
  [bindings & body]
  (if-let [[binding expr & more-bindings] (seq bindings)]
    `(_with-open ~expr (fn [~binding]
                         (with-open+ [~@more-bindings]
                           ~@body)))

    `(do ~@body)))
