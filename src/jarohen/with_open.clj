(ns jarohen.with-open
  (:import
   [java.io Closeable]
   [java.lang AutoCloseable]))

(set! *warn-on-reflection* true)

(defmacro try-finally
  "Use try/finally such that an exception in the finally clause does not
  mask a prior exception in the try block."
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

(defmacro with-open+ [bindings & body]
  (if-let [[binding expr & more-bindings] (seq bindings)]
    `(_with-open ~expr (fn [~binding]
                         (with-open+ [~@more-bindings]
                           ~@body)))

    `(do ~@body)))
