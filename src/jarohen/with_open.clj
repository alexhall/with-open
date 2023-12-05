(ns jarohen.with-open
  (:import [java.io Closeable]
           [java.lang AutoCloseable]))

(defn _with-open [resource f]
  (cond
    (or (instance? Closeable resource)
        (instance? AutoCloseable resource)) (with-open [_ resource]
                                              (f resource))

    (fn? resource) (resource f)
    :else (throw (ex-info "Invalid resource passed to with-open+" {:resource resource}))))

(defmacro with-open+ [bindings & body]
  (if-let [[binding expr & more-bindings] (seq bindings)]
    `(_with-open ~expr (fn [~binding]
                         (with-open+ [~@more-bindings]
                           ~@body)))

    `(do ~@body)))
