(ns unifn.core-test
  (:require [unifn.core :as u]
            [matcho.core :as matcho]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as s]))

(defmethod u/*fn :test/transform
  [arg]
  {:var "value"})


(defmethod u/*fn :test/response
  [arg]
  {:response  {:some "response"}})


(defmethod u/*fn :test/interceptor
  [arg]
  (when (:intercept arg)
    {:response {:interecepted true}
     ::u/status :stop}))

(defmethod u/*fn :test/catch
  [arg]
  {:catched true})

(defmethod u/*fn :test/throwing
  [arg]
  (throw (Exception. "ups")))

(s/def :test/specified
  (s/keys :req [:test/specific-key]))

(defmethod u/*fn :test/specified
  [arg]
  {:specified true})

(defmethod u/*fn ::http
  [arg]
  (println "FX:" (:uri arg))
  {:status 200 :body {:ok true}})

(defmethod u/*fn ::override-http
  [arg]
  (println "FX:" (:uri arg))
  {:status 404 :body {:ok false}})

(defmethod u/*fn ::do-efffect
  [_]
  {::u/fx {:response {::u/fn ::http
                      :uri "http://ya.ru"}}})

(defmethod u/*fn ::handle-efffect
  [{resp :response}]
  {:processed resp})

(deftest deep-merge-test
  (is
   (= (u/deep-merge
       {:a {:b {:c 1}
            :x {:y 1 :z 2}}}
        {:a {:b {::u/action :replace :d 2}
             :x {:y 2}}})
      {:a {:b {:d 2} :x {:y 2 :z 2}}}))

  )

(deftest unifn-basic-test
  (matcho/match
   (u/*apply {::u/fn :test/transform} {:some "payload"})
   {::u/fn #(= "transform" (name %))
    :var "value"
    :some "payload"})

  (matcho/match
   (u/*apply {::u/fn :test/unexisting} {:some "payload"})
   {::u/fn #(= "unexisting" (name %))
    ::u/status :error})

  (is (thrown? Exception (u/*apply {::u/fn :test/throwing} {:some "payload"})))

  (matcho/match
   (u/*apply {::u/fn :test/throwing} {:some "payload" ::u/safe? true})
   {::u/status :error
    ::u/message string?})


  (matcho/match
   (u/*apply [{::u/fn :test/transform}
              {::u/fn :test/interceptor}
              {::u/fn :test/response}]
             {:request {}})

   {:response {:some "response"} :var "value" })

  (matcho/match
   (u/*apply [{::u/fn :test/transform}
              {::u/fn :test/interceptor}
              {::u/fn :test/response}]
             {:request {} :intercept true})
   {:response {:interecepted true}})

  (matcho/match
   (u/*apply [{::u/fn :test/interceptor}
              {::u/fn :test/response}
              {::u/fn :test/catch ::u/intercept :all}
              {::u/fn :test/response}]
             {:intercept true})
   {:response {:interecepted true}
    :catched true})

  (matcho/match
   (u/*apply {::u/fn :test/specified} {})
   {::u/status :error
    ::u/message string?})

  (matcho/match
   (u/*apply {::u/fn :test/specified} {:test/specific-key 1})
   {:specified true})



  (matcho/match
   (u/*apply [::do-efffect ::handle-efffect] {})
   {:processed {:status 200 :body {:ok true}}})

  (matcho/match
   (u/*apply [::do-efffect ::handle-efffect] {::u/override-fx {::http ::override-http}})
   {:processed {:status 404 :body {:ok false}}})




  )
