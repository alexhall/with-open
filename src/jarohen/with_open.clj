(ns jarohen.with-open
  (:import
   [java.io Closeable]
   [java.lang AutoCloseable]))

(set! *warn-on-reflection* true)

(defmacro suppress-throwable
  "Throw ex. Run the body and if it throws, then add the exception as a suppressed exception to ex."
  [e body]
  `(try
     ~body
     (catch Throwable e2#
       (.addSuppressed ^Throwable ~e e2#))
     (finally
       (throw ~e))))

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
  (if (= (count bindings) 0) `(do ~@body)
      (let [[binding expr] bindings]
        ;; It needs to allow for destructuring so we make a placeholder gensym
        `(let [value# ~expr]
           (cond
             (satisfies? Resource value#)
             ,(try-finally
                (let [~binding value#]
                  (with-open+ ~(subvec bindings 2) ~@body))
                :finally
                (-close value#))
             (fn? value#)
             ,(value# (fn [~binding]
                        (with-open+ ~(subvec bindings 2) ~@body)))
             :else (throw (ex-info "Invalid resource passed to with-open+" {:resource value#})))))))
