(ns jarohen.with-open
  (:import
   [java.io Closeable]
   [java.lang AutoCloseable]))

(set! *warn-on-reflection* true)

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
    (satisfies? Resource resource) (try
                                     (f resource)
                                     (finally
                                       (-close resource)))

    (fn? resource) (resource f)
    :else (throw (ex-info "Invalid resource passed to with-open+" {:resource resource}))))

(defmacro with-open+ [bindings & body]
  (if-let [[binding expr & more-bindings] (seq bindings)]
    `(_with-open ~expr (fn [~binding]
                         (with-open+ [~@more-bindings]
                           ~@body)))

    `(do ~@body)))
