(ns unifn.core
  (:require [clojure.spec.alpha :as s]
            [clojure.stacktrace :as stacktrace]))

(defn deep-merge [a b]
  (loop [[[k v :as i] & ks] b
         acc a]
    (if (nil? i)
      acc
      (let [av (get acc k)]
        (cond
          (= v av)
          (recur ks acc)

          (and (map? v) (= :replace (::action v)))
          (recur ks (assoc acc k (dissoc v ::action)))

          (and (map? v) (map? av))
          (recur ks (assoc acc k (deep-merge av v)))

          :else (recur ks (assoc acc k v)))))))

(defmulti *fn (fn [arg] (::fn arg)))

(defmethod *fn ::identity
  [arg] {})

(defmethod *fn :default
  [arg]
  {::status :error
   ::message (str "Could not resolve " (::fn arg))})

(declare *apply)

(defn *apply-impl [{st ::status inter ::intercept f-name ::fn tracers ::tracers :as arg}]
  (if (and (not (= inter :all))
           (contains? #{:error :stop} st))
    arg
    (let [arg (dissoc arg ::intercept) ]
       (when tracers
         (let [trace-ev {::fn f-name ::phase :enter}]
           (doseq [t tracers] (*apply t {:event trace-ev :arg arg}))))

       (if-let [problems (and (s/get-spec f-name) (s/explain-data f-name arg))]
         (let [ev {::status :error
                   ::fn  f-name
                   ::message (with-out-str (s/explain f-name arg))
                   ::problems (::s/problems problems)}]
           (when tracers
             (doseq [t tracers] (*apply t {:event ev :arg arg})))
           (merge arg ev))
         (let [patch (if (::safe? arg)
                       (try (*fn arg)
                            (catch Exception e
                              {::status :error
                               ::message (with-out-str (stacktrace/print-stack-trace e))}))
                       (*fn arg))
               patch (cond (map? patch) patch
                           (nil? patch) {}
                           :else {::value patch})
               ;; ??? effects?
               ;; if response contains ::fx map we apply effects to result
               ;; you can override fx for tests by passing ::u/override-fx map 
               patch (reduce (fn [acc [k {*fn ::fn :as u}]]
                               (let [*fn (get-in arg [::override-fx *fn] *fn)]
                                 (assoc acc k (*apply (assoc u ::fn *fn) {}))))
                             (dissoc patch ::fx)  (::fx patch))

               res (deep-merge arg patch)]
           (when tracers
             (let [trace-ev (merge arg {::phase :leave})]
               (doseq [t tracers] (*apply t {:event patch :arg res}))))
           res)))))

(defn *apply [f arg]
  ;; validate f
  (cond
    (string? f)  (*apply-impl (assoc arg ::fn (keyword f)))
    (keyword? f) (*apply-impl (assoc arg ::fn f))
    (map? f)     (*apply-impl (deep-merge arg f))
    (vector? f) (loop [[f & fs] f, arg arg]
                  (cond
                    (nil? f) arg
                    :else (recur fs (*apply f arg))))
    (var? f) (*apply (var-get f) arg)
    :else (throw (Exception. (str "I don't know how to apply " (pr-str f))))))
