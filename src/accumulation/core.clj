(ns accumulation.core
  (:require [clostache.parser :as mustache]
            [ring.util.codec :as codec]
            [clojure.core.async :as a]
            [cheshire.core :as json]
            [robert.bruce :refer [try-try-again]]
            [schema.core :as schema]
            [cemerick.url :refer [url-encode]]
            [clojure.core.match :refer [match]]
            [clj-uuid :as uuid]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.local :as l]
            [clj-time.coerce :as c]
            [clj-http.client :as client]
            [taoensso.carmine :as car :refer (wcar)]
            [compojure.core :refer :all]
            [clojure-csv.core :as csv]
            [clj-ssh.ssh :as ssh]
            [clj-ssh.cli :as cli]
            [miner.ftp :as ftp]
            [loom.graph :as g]
            [clojure.math.combinatorics :as combo]
            [clojure.walk :refer [keywordize-keys]]
            [clojure-tensorflow.ops :as tf]
            [clojure-tensorflow.layers :as layer]
            [clojure-tensorflow.optimizers :as optimize]
            [clojure-tensorflow.core :refer [run with-graph with-session]])
  (:import [java.util GregorianCalendar]))

(comment
  "A point P=(x,y)"
  "A line from A=(x1,y1) to B=(x2,y2)"
  "To determine where the point locates")

(defn right-side?
  [a b p]
  (let [[x1 y1] a
        [x2 y2] b
        [x y] p
        minuend (* (- x x1) (- y2 y1))
        substractor (* (- y y1) (- x2 x1))]
    (pos? (- minuend substractor))))

(defn on-line?
  [a b p]
  (let [[x1 y1] a
        [x2 y2] b
        [x y] p]
    (cond
      (= x1 x) (= x2 x)
      (= y1 y) (= y2 y)
      :else (= (* (- x1 x) (- y1 y))
               (* (- x x2) (- y y2))))))

;;Return str values of map; values' order are the same as keys
(defn ordered-str-values
  [map keyseq]
  (loop [ret [] keys (seq keyseq)]
    (if keys
      (let [value (. clojure.lang.RT (get map (first keys)))]
        (recur
         (if value
           (conj ret (str value))
           ret)
         (next keys)))
      (with-meta ret (meta map)))))

;; mapcat is a strong tool
;; check it out
;; we implement an flatten here

(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])

(mapcat identity [[[0 1] [1 2]] [[11 12]]])

(mapcat reverse [[[0 1] [1 2]] [[11 12]]])

(mapcat identity [[:a  :b] [:c]])

(str "salesforce"  #uuid "971914b0-24fc-11e5-90b9-a0f77e30d8d3")

(mapcat reverse [[:a  :b] [:c]])

;;local bind (let [] ())
(let [x 7 y 3 z 1] (+ x (+ y z)))

(+ 1 (* 2 3) 4)

((fn foo [x]
  (when (> x 0)
    (conj (foo (dec x)) x)
    )) 5)

(loop [x 5 result []]
  (if (> x 0)
    (recur (dec x) (conj result (+ 2 x)))
    result))
 
;; 4clojure implement nth
;; problem 21
(#(first (drop %2 %1)) [1 2 3 4 5 6] 2)

(-> :send-sms name (str "-parameters") keyword)

;; 4clojure implement count
;; problem 22
((partial reduce (fn [c x] (inc c)) 0) [1 2 3 4 5])

;;4clojure  Fibonacci Sequence

(#(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))) 50)

(#(take % (iterate (fn [[a b]] [b (+ a b)]) [1 1])) 4)

(repeat 5 (rand-int 100))

(repeatedly 5 #(rand-int 100))

(take 5 (cycle '(rand-int 100)))

;; 4clojure implement flatten recursively
;; problem 28
((fn myFlatten [x]
   (if (coll? x)
     (mapcat myFlatten x)
     [x])) [1 2 3 [4 [7 [10]] 8] 5 6])

;; 4clojure find the capital letter in a string
;; problem 29
(#(apply str (re-seq #"[A-Z]" %))
 "AxYze3%WAHT1")

(clojure.string/join "" (filter #(Character/isUpperCase %1) (seq "Hello WorlddfdAAA" )))

;; it does not work why  ? we need apply!!!
(apply str (filter #(Character/isUpperCase %1) (seq "Hello WorlddfdAAA" )))

(filter #(Character/isUpperCase %1)  "Hello WorlddfdAAA" )

(str "sdfdf" "www")

;; 4clojure 30
;; removes the consecutive duplicates from a sequence
;; compare the element with the last in acc
;; conj [] val  val is the last element in new vector
;; conj '() val val is the first element in new sequence
;; reduce( f val coll)

((fn f [s]
  (let [f2
        (fn [acc s2]
          (if (= (last acc) s2)
            acc
            (conj acc s2)))]
    (reduce f2 [] s)))
  "leeeeeeeeeerrrrrrrrrooooyyyyy wwwwhat")

;; a more simple solution
( apply str (map first
                 (partition-by identity "Leeeeeeeeeeeeeeeeeeerrrreeeeeeorrryyyy")))

;; 4clojure 31 pack consecutive duplicates
(partition-by identity "leewwwyyee")

(map (fn [x] (conj [] x)) [1 2 3])

(partition-by identity [1 1 2 3 3 1 1 5 5])

(group-by identity [1 1 2 3 3 1 1 5 5])

;; 4clojure 32 duplicate each element
((fn f [s]
   (let [f2
         (fn [acc s2]
           (conj acc s2 s2))]
     (reduce f2 [] s))) "le")

;; 4clojure 33 duplicate each element n times
((fn f [s n]
   (let [f2
         (fn [acc s2]
           (concat acc (repeat n s2)))]
     (reduce f2 [] s))) "le" 3)

;; 4clojure 34 implement range
((fn f [l  r]
   (loop [x l result []]
     (if (< x r)
       (recur (inc x) (conj result  x))
       (seq result)))) -1 3 )

;;4clojure 37
(str (re-seq #"[A-Z]+" "bA1B3Ce "))

(map last (re-seq #"(\S+):(\d+)" "RX pkts:18 err:5 drop:48"))

;;4clojure 38
;;use & for uncertain parameters

((fn f [& s]
   (reduce
    (fn [acc s2] (if (> s2 acc) s2 acc))
    (first s)
    (rest s))) 1 2 3 9 3 4 5 )

((fn [& x] (reduce (fn [y z] (if (< y z) z y)) 0 (seq x))) 1 2 3 4 5 6 4)

((fn [& x] (sequence x)) 1 2 3 4 5 6 4)

;;4clojure 39
;;mapcat
(mapcat vector [1 2] [3 4 5])

(map vector [1 2] [3 4 5])

;;map a map
;;in a map {:a 1 :b 2 :c 3}
;;the first element is :a 1  :key-value
(map #(vector (first %) (* 2 (second %)))
     {:a 1 :b 2 :c 3})
;;4clojure 40
((fn f [s1,s]
   (let [f2
         (fn [acc s2]
           (if (empty? acc)
             (conj acc s2)
             (conj acc s1 s2)))]
     (reduce f2 [] s))) "," ["one" "two" "three"])


((fn [s1,s]
   (let [f2
         (fn [acc s2]
               (conj acc s1 s2))]
     (rest (reduce f2 [] s)))) 0  [1 2 3 4 5 6 7 8])

;; another solution
((fn [x s]
   (rest (mapcat #(vector x %1) s) )) "," ["one" "two" "three"])

(mapcat  #(vector "," %) ["one" "two" "three"])

;; 4clojure 41 drop EVERY nth element
((fn f [s n]
   (loop [agg [] s* s]
     (if (empty? s*)
       agg
       (recur (concat agg (let [x (take n s*)]
                            (if (> n (count x))
                              x
                              (drop-last x))))
              (drop n s*))))) [1 2 3 4 5 6 7 8] 3)

;; another excellent solution
((fn drop-nth [lat n]
   (flatten (map #(if (= (count %) n) (drop-last %) %)
                 (partition-all n lat)))) [1 2 3 4 5 6 7 8] 3)

((fn drop-nth [lat n]
    (mapcat #(if (= (count %) n) (drop-last %) %)
                 (partition-all n lat))) [1 2 3 4 5 6 7 8] 3)

;; 4clojure 42
((fn functorial [n]
   (loop [x n result 1]
     (if (> x 0)
       (recur (dec x) (* result x))
       result))) 6)

;; 4clojure  43
;; implement the reverse of interleave
 (map seq (vals (group-by #(mod % 3) (range 9))))

((fn f [s n]
   (map seq (vals (group-by #(mod % n) s)))) (range 10) 5)

(#(apply map list (partition %2 %1)) (range 9) 3)
;;
(apply map vector [[1 2] [3 4]]) ;;  ([1 3] [2 4])

(apply map list [[1 2] [3 4]]) ;;  ((1 3) (2 4))

(map list [1 2] [3 4])

(map #(apply max %) [[1 2 3][4 5 6][7 8 9]])

;; get-in to get value by key from a  nested map
(get-in {:attribute {:properties.region
                     {:key :validation/must-be/enum,
                      :args '("'gll', 'br1', 'sg1', 'jp1', 'ie1', 'us1', 'au1'")}}}
         [:attribute :properties.region])

(get-in {:message {:key :validation/bad-params, :args nil}}
        [:message])
 
(fn [& args] inc )

;; reduce  kv with init {}
(reduce-kv #(assoc %1 %3 %2) {} {:a 1 :b 2 :c 3})

(:integration-types {:integration-types [:salesforce]})

(reduce #(let [type (keyword %2)](assoc % type {:tenantid "aaaa" :active true})) {} [:salesforce :twilio])

(update (update {:month 1 :age 26} :month inc) :age dec)

;;; use -> to update twice
(-> {:month 1 :age 26}
    (assoc :month2 3)
    (assoc :age3 4))

(def params {:id #uuid "fa10c9a0-7069-11e6-80d3-ca81484488df",
             :action-name :send-flow-sms-test,
             :sub-id #uuid "fa10a290-7069-11e6-80d3-ca81484488df",
             :interaction-id #uuid "f9faf7b0-7069-11e6-9e15-ca81484488df",
             :tenant-id #uuid "92f50f30-661c-11e6-b1b9-ca81484488df",
             :integration
             {:account-sid "ACef3653b6d46943104252b71341b5917a",
              :auth-token "7ccc86d599dd5cf00a9b90114bb950b2",
              :force-region "true",
              :region "sg1",
              :web-rtc "true"},
             :metadata
             {:caller "",
              :called-country "CA",
              :called-city "",
              :caller-zip "",
              :from-zip "",
              :from-state "NB",
              :protocol "https",
              :called-state "New Brunswick",
              :participants
              {:c-ac-3d-0c-92bcc-8b-38c-5a-29494b-150ad-6416
               {:type "customer",
                :id "+15068970000",
                :extension "+15068970000",
                :call-sid "CAc3d0c92bcc8b38c5a29494b150ad6416",
                :dialed "2016-09-01T17:31:54.781Z"}},
              :caller-state "NB",
              :to-city "",
              :api-version "2010-04-01",
              :caller-city "FREDERICTON",
              :to-country "CA",
              :host "us-east-1-dev-twilio-gateway.cxengagelabs.net",
              :called-zip "",
              :from "",
              :called "+15063002662",
              :call-status "ringing",
              :to-state "New Brunswick",
              :application-sid "AP3c89cdf40da360704b6a0d550f5bae9f",
              :to-zip "",
              :account-sid "ACef3653b6d46943104252b71341b5917a",
              :from-city "FREDERICTON",
              :tenant-id #uuid "92f50f30-661c-11e6-b1b9-ca81484488df",
              :direction "inbound",
              :call-sid "CAc3d0c92bcc8b38c5a29494b150ad6416",
              :from-country "CA",
              :caller-country "CA",
              :to "+15063002662"},
             :parameters {:from "+15063002662", :to "+15068970000", :body nil}})

(mustache/render  "Hello, {{from-city}}!" params)

(defn flatten-content [node]
  (lazy-seq
   (if (string? node)
     (list node)
     (mapcat flatten-content (:content node)))))

(flatten-content params)

;; dissoc
(apply dissoc {:a 1 :b 2 :c 3} [:c :b])

;;dissoc-in
(update-in {:a {:b {:x 3} :c 1}} [:a :b] dissoc :x)

;;assoc-in
(assoc-in {:user {:bar "baz"}}  [:user :bar] "some-id")

;;get-in
(get-in {:user {:bar "baz"}}  [:user :bar])

(assoc {} :a 1)

;;find
(find  {:user {:bar "baz"}}  :bar)

(doseq [[k v] {:bar "baz" :a "test"}] (println v))

(interleave [:a :b :c] [1 2 3])

(zipmap [:a :b :c] [1 2])

(format "integration.%s.%s" "7fb9b930-49e3-11e5-9dd5-621c6d9e2761" #uuid "a2e8f9a0-6956-11e6-ac64-ca81484488df")

(drop 2 [1 2 3 4 5])
(take 5 (iterate (partial + 2) 0))

(-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last))

(->> [1 2 3 4 5]
     (#(map inc %))
     (apply +))

(->> [1 2 3 4 5 6 7 8] (filter even?) (take 3))

(for [x (iterate #(+ 4 %) 0)
      :let [z (inc x)]
      :while (< z 40)
      ]
  z)

(for [[x y] (partition 2 (range 20))]
  (+ x y))

(for [x (iterate #(+ 4 %) 0)
      :while (< x 40)
      ]
  x)

(#(reduce + 0 %1) '(1 2 3 4 5))

(reduce #(+ %1 %2) [1 2 3 4 5])

;; cons x seq
(reduce #(cons %2 %1) [1 2 3] [4 5 6])

(->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc))

(second (reverse (list 1 2 3 4 5)))

(filter odd? #{1 2 3 4 5})

(codec/percent-decode (codec/percent-encode "whata 1334543/df234345`ssd~dfhah'"))

(def test-params {:to "whatds"
                  :from "+15060000"
                  :body { :text "what a great day  {{name}} "}
                  :message "it is test code"})

(zipmap (keys test-params)
        (map #(if (string? %)
                (codec/percent-encode %)
                %)
             (vals test-params)))

(keys test-params)

(reduce-kv #(assoc %1 %2 (= Integer (type %3))) {} test-params)

(doseq [k {:a "x" :b "y" :c "z"}] (prn k))

(def c (a/chan 1))

(a/go
  (try
    (a/<! c)
    (println "foo")
    (catch Exception e)))

(a/>!! c "hello")

(a/put! c "hello")

(a/<!! c)

(((fn [& fs] (reduce (fn [f g] #(f (apply g %&))) fs))
  rest reverse) [1 2 3 4 5])

((comp first rest reverse) [1 2 3 4 5])

(symbol? "(render hello {{interaction/customer-metadata.first-name}}")

(reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] [1 2 1 3 1 2 4])

(((fn [& fs] (reduce (fn [f g] #(f (apply g %&))) fs)) first reverse rest) [1 2 3 4])
;;reduce take the first 2 arguments  first and reverse
;;compute the result as temp and then fn (temp rest)

((comp rest reverse) [1 2 3 4 5 76])

(((fn [& fs]
    (fn [& as] (map #(apply % as) fs)))
  + max min) 2 3 5 1 6 4)
 
((set [1]) 2)
(set [])
(#{1 2} 2)
(#{1 } 2)

(vec (set [1 2 1 3 1 2 4]))

(vec (set [1 2 1 3 1 2 4]))

(let [[a b & c :as d] [1 2 3 4 5]] [a b c d])
(#(vector (take % %2) (drop % %2)) 3 '(1 2 3 4 5 6))

(#(vals (group-by type %)) [1 :a 2 :b 3 :c])
(group-by type [1 :a 2 :b 3 :c])

(let [[a b c d e]
      [0 1 2 3 4]]
  [c e])

(a/put! c "hello" (fn [ & agrs] (print "what")))

;; recursively 
((fn p [n c] (when (and (seq c) (>= (count c) n)) (cons (take n c) (p n (drop n c))))) 3 (range 9))

(#(loop [xs %2 acc []]
    (if (< (count xs) %)
      acc
      (recur (drop % xs) (conj acc (take % xs))))) 3 (range 8))

(take 3 (range 8))
(drop 3 (range 8))
(let [x 1
      y 2]
  (+ x y)
  (- x y)
  {:a "hello"})

(let [[a b & c :as d] [1 2 3 4 5]]
  (println a) ; 1
  (println b) ; 2
  (println c)  ; (3 4 5)
  d) ;[1 2 3 4 5]

(def chan1 (a/chan))

(reduce (fn [result [connection topics]]
          (or result (when (< (count topics) 40)
                       connection))) nil {:123 "123",:tony "wang"})

(assoc {:123 "123"} :tony "wang")
(count "wang")

(or :123 :tony)

(defrecord Person [fname lname add])
(defrecord Address [street city zip state])

(def stu (Person. "tony" "wang" (Address. "200" "fredericton" "E3B2M8" "NB")))

(let [{:keys [fname add]} stu]
  add)

(let [z (map->Person {:fname "tony" :lastname "wang" :add "an address"})]
  println z)

(->Person "tony" "wang" "any")

(defprotocol Poolable
  (disconnect [this])
  (subscribe [this topic qos])
  (unsubscribe [this topic])
  (handler [this handler])
  (publish [this topic message-or-payload])
  (get-events-chan [this])
  (get-publishing-client [this]))

(def get-events-chan* (memoize (fn [this] (a/chan 1024))))

(loop [iteration 0]
  (println (str "Iteration " iteration))
  (if (> iteration 3)
    (println "Goodbye!")
    (recur (inc iteration))))

(take-while neg? [-2 -1 0 1 2 3])

(take-while neg? [-2 1 -1 0 1 2 3])

((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)

(frequencies ['a 'b 'a 'a])
 
(reduce (fn [result el] (if (contains?  result el) (update result el inc) (assoc result el 1))) {} [1 1 2 3 2 1 1])

(reduce #(if (contains? % %2) (update % %2 inc) (assoc % %2 1)) {} [1 1 2 3 2 1 1])

(reduce #(assoc % %2 (inc (% %2 0))) {} [1 2 3 4 4])

(contains? {'a 1} 'a)

({1 2} 1 0)

(def my-chan (a/chan 1024))

(a/go-loop [cnt 1]
  (when-let [{:keys [topic response-channel] :as envent} (a/<! my-chan)]
    (do (println (str topic cnt) (a/close! response-channel) (Thread/sleep 1000)))
    (recur (inc cnt))))

(let [response-chan (a/chan)]
  (a/>!! my-chan {:topic "hello" :response-channel response-chan})
  (a/<!! response-chan))

(def my-response2 {:response {:status 200, :body {:status 500, :body nil}},
                   :type :toran-gateway,
                   :request
                   {:host :plivo,
                    :path "/tenants/00000000-0000-0000-0000-000000000000/interactions/00000000-0000-0000-0000-000000000000/actions/verify-credentials",
                    :method :post,
                    :body
                    {:id #uuid "96d43ff1-7f5c-11e6-98e4-ca81484488df",
                     :action-name :verify-credentials,
                     :sub-id #uuid "00000000-0000-0000-0000-000000000000",
                     :interaction-id #uuid "00000000-0000-0000-0000-000000000000",
                     :tenant-id #uuid "00000000-0000-0000-0000-000000000000",
                     :integration nil,
                     :metadata {},
                     :parameters {:auth-id "D", :auth-token "G"}},
                    :options nil}})

(def my-response {:response
                  {:status 400, :body {:status 400, :body {:message "Invalid URL"}}},
                  :type :toran-gateway,
                  :request
                  {:remote-addr "10.211.36.112",
                   :request-method :post,
                   :uri "/gateways",
                   :content-type "application/transit+msgpack",
                   :extracted-params
                   {:id #uuid "60f2dc70-7f57-11e6-98e4-ca81484488df",
                    :source :plivo,
                    :action
                    {:id #uuid "60f2dc71-7f57-11e6-98e4-ca81484488df",
                     :action-name :verify-credentials,
                     :sub-id #uuid "00000000-0000-0000-0000-000000000000",
                     :interaction-id #uuid "00000000-0000-0000-0000-000000000000",
                     :tenant-id #uuid "00000000-0000-0000-0000-000000000000",
                     :integration nil,
                     :metadata {},
                     :parameters {:auth-id "ddss", :auth-token "test"}}}}})

(:body (:response my-response))

(try
  (let [response  (:response my-response2)]
    (= (:status response) 200))
  (catch Exception _
    false))

{:response {:status 200,
            :body {:status 500,
                   :body nil}},
 :type :toran-gateway,
 :request
 {:remote-addr "10.211.36.112",
  :request-method :post,
  :uri "/gateways",
  :content-type "application/transit+msgpack",
  :extracted-params
  {:id #uuid "d1688480-7f5e-11e6-98e4-ca81484488df",
   :source :plivo,
   :action
   {:id #uuid "d1688481-7f5e-11e6-98e4-ca81484488df",
    :action-name :verify-credentials,
    :sub-id #uuid "00000000-0000-0000-0000-000000000000",
    :interaction-id #uuid "00000000-0000-0000-0000-000000000000",
    :tenant-id #uuid "00000000-0000-0000-0000-000000000000",
    :integration nil,
    :metadata {},
    :parameters {:auth-id "D", :auth-token "G"}}}}}

(when-not (or (empty? nil) (empty? "s"))
  println "hello")

(nil? nil)

(or true false)

(let [el "hello"]
  (str el "ss")
  (str el "tony")
  {200 "dd"})

(if (or (empty? "x") (empty? "x"))
  {400 "sorry"}
  (do (println "dff")
   {200 "ok"}))

   
(if-let [interaction-id (get-in {:cookies {:value2 "hello"}} [:cookies :value])]
  (str "888" )
  (str "what"))

(get-in {:cookies {:value "hello"}} [:cookies :value])

(defn start
  ([p1 p2 p3]
   (println "parameters"))
  ([p1 p2]
   (start p1 p2 nil)))

;; to interaction/customer from interaction/contact-point messag hello {{interaction/customer}}  how you doing ?
(def vip-instance
  {:ASGName nil,
   :leaseInfo "ss" 
   :dirty false,
   :overriddenStatus :unknown,
   :appName "TONY-TEST",
   :SID "771f62b0-f870-11e6-818a-87de25dddb39",
   :instanceId nil,
   :lastDirtyTimestamp 1487707264794,
   :appGroupName "UNKNOWN",
   :securePort 443,
   :statusPageUrl "http://twang:7081/Status",
   :dataCenterInfo "info"
   :hostName "twang",
   :healthCheckUrl "http://twang:7081/healthcheck",
   :port 7081,
   :healthCheckUrls #{"http://twang:7081/healthcheck"},
   :secureVipAddress nil,
   :lastUpdatedTimestamp 1487707265016,
   :secureHealthCheckUrl nil,
   :status :up,
   :VIPAddress "twilio-gateway.titan.net",
   :id "twang",
   :homePageUrl "http://twang:7081/",  
   :countryId 1,
   :IPAddr "127.0.1.1",
   :version "unknown",
   :metadata {}})

(defn my-fun
  [a]
  (println a)
  (println "word"))

(defn my-test
  [a]
  (let [node a action-response "here"]
    (cond (nil? node) (println "node is nil")
          (not (:self? node)) (println "send")
          :else (println "else"))))

(defn fun1 [my-special-chan]
  (dotimes [n 100] (a/>!! my-special-chan (str "n is" n))))

(defn get-rest-nodes []
  "dd")

(defn shutdown-mode
  [chan]
  (if-let [nodes (get-rest-nodes)]
    (do (loop []
          (when-let [val (a/<!! chan)]
            (println (str "forward value in channel to one of remaining nodes" val))
            (recur)))
        (println "channel is drained now in shutdown-mode"))
    (println "last node  shutdown immediately")))

(defn fun-test-promise-chan [test-promise-chan my-special-chan]
  (a/go-loop []
    (let [[val chan] (a/alts! [test-promise-chan my-special-chan] :priority true)]
      (if (= chan test-promise-chan)
        (do (println "shutdown")
            (shutdown-mode my-special-chan))
        (if (not (nil? val))
          (do (println (str "process message in channel value : " val))              
              (recur))
          (println "channel is drained now"))))))

(defn fun-test-promise-chan2 [test-promise-chan my-special-chan]
  (a/go-loop []    
    (let [[val port] (a/alts! [ test-promise-chan my-special-chan] :priority true)]
      (println "here is the port :" port )
      (println "here is the val :" val )
      (match [val port]
             [_ test-promise-chan]
             (do (println (str "shutdown" ))
                 (shutdown-mode my-special-chan))
             [_ my-special-chan]
             (if (not (nil? val))
               (do (println (str "process message in channel value "))              
                   (recur))
               (println "channel is drained now"))))))

(defn test-fn [val]
  (when-let [node val]
    (println val)
    (println "hello"))
  (println "here we go ")
  (println "tony"))

(defn x-integer? [x]
  (cond
    (integer? x) x
    (string? x) (try
                  (Integer/parseInt x)
                  (catch Exception e
                    :clojure.spec/invalid))
    :else :clojure.spec/invalid))

(s/def :integration/work-items boolean?)
(s/def :integration/endpoint-prefix string?)
(s/def :integration/password string?)
(s/def :integration/interaction-field-id  (s/conformer x-integer?))

(defmulti integration-work-items :integration/work-items)

(defmethod integration-work-items true [_]
  (s/keys :req [:integration/work-items :integration/endpoint-prefix :integration/username :integration/password :integration/interaction-field-id]))

(defmethod integration-work-items false [_]
  (s/keys :req [:integration/work-items :integration/endpoint-prefix :integration/username :integration/password]
             :opt [:integration/interaction-field-id]))

(s/def ::integration (s/multi-spec integration-work-items :integration/work-items))

(s/def ::work-items boolean?)
(s/def ::endpoint-prefix string?)
(s/def ::password string?)
(s/def ::interaction-field-id  (s/conformer x-integer?))

(defmulti integration-work-items2 :work-items)

(defmethod integration-work-items2 true [_]
  (s/keys :req-un [::work-items ::endpoint-prefix ::username ::password ::interaction-field-id]))

(defmethod integration-work-items2 false [_]
  (s/keys :req-un [::work-items ::endpoint-prefix ::username ::password]))

(s/def ::integration2 (s/multi-spec integration-work-items2 :work-items))

(def node-map {:nodes
               #{{:id #uuid "45b9a630-f3df-11e6-bd59-2c89d49e69b7",
                  :address "127.0.0.1",
                  :port 7080}
                 {:id #uuid "4abd4280-f3e0-11e6-a76c-2c89d49e69b7",
                  :address "127.0.0.1",
                  :port 6080}}})

(def email (json/generate-string {"bindings" {},
                                  "params" {"artifact-id"
                                            {"source" "expression",
                                             "type" "variable",
                                             "mandatory" true,
                                             "key" "artifactId"}},
                                  "label" "Send Email",
                                  "name" "send-email",
                                  "signals" [],
                                  "props" ["name" "params" "bindings" "loop" "target"],
                                  "target" "interaction/source",
                                  "inputs" [{"index" 0,
                                             "group" "params",
                                             "placeholder" nil,
                                             "label" "Artifact ID",
                                             "path" "params.artifactId",
                                             "name" "artifactId",
                                             "required" true,
                                             "type" "string",
                                             "hidden" false,
                                             "disabled" false}],
                                  "entity" "activity"}))

(def instances '({:ASGName nil,
                  :leaseInfo
                  "leaseInfo"
                  :dirty false,
                  :overriddenStatus :unknown,
                  :appName "TWILIO-GATEWAY",
                  :SID "na",
                  :instanceId nil,
                  :lastDirtyTimestamp 1487691969836,
                  :appGroupName "UNKNOWN",
                  :securePort 443,
                  :statusPageUrl "http://ed2b76f266ba:8080/Status",
                  :dataCenterInfo
                  "data"
                  :hostName "ed2b76f266ba",
                  :healthCheckUrl "http://ed2b76f266ba:8080/healthcheck",
                  :port 8080,
                  :healthCheckUrls #{"http://ed2b76f266ba:8080/healthcheck"},
                  :secureVipAddress nil,
                  :lastUpdatedTimestamp 1487691970298,
                  :secureHealthCheckUrl nil,
                  :status :up,
                  :VIPAddress "twilio-gateway.titan.net",
                  :id "ed2b76f266ba",
                  :homePageUrl "http://ed2b76f266ba:8080/",
                  :class "aa"
                  :actionType
                  "ss"
                  :countryId 1,
                  :IPAddr "172.18.0.5",
                  :version "unknown",
                  :metadata {}}
                 {:ASGName nil,
                  :leaseInfo
                  "df" 
                  :dirty false,
                  :overriddenStatus :unknown,
                  :appName "TWILIO-GATEWAY",
                  :SID "na",
                  :instanceId nil,
                  :lastDirtyTimestamp 1487691969409,
                  :appGroupName "UNKNOWN",
                  :securePort 443,
                  :statusPageUrl "http://2cfcd855fdb0:8080/Status",
                  :dataCenterInfo
                  "dd"
                  :hostName "2cfcd855fdb0",
                  :healthCheckUrl "http://2cfcd855fdb0:8080/healthcheck",
                  :port 8080,
                  :healthCheckUrls #{"http://2cfcd855fdb0:8080/healthcheck"},
                  :secureVipAddress nil,
                  :lastUpdatedTimestamp 1487691969840,
                  :secureHealthCheckUrl nil,
                  :status :up,
                  :VIPAddress "twilio-gateway.titan.net",
                  :id "2cfcd855fdb0",
                  :homePageUrl "http://2cfcd855fdb0:8080/",
                  :class "dd"
                  :actionType
                  "dd"
                  :countryId 1,
                  :IPAddr "172.18.0.4",
                  :version "unknown",
                  :metadata {}}))

(defmulti foo (fn [meal] (condp instance? meal
                           java.util.Collection ::breakfast
                           java.lang.Double ::lunch)))

(defmethod foo ::breakfast
  [meal]
  (count meal))

(defmethod foo ::lunch
  [meal]
  meal)

(comment
  "Here is the block comment "
  "I have no idea to use this")

(defrecord Parcel [start            
                   finish
                   package 
                   run-type
                   weight
                   ])

(defn costOfPath [p1 p2]
  3)

(defn routeCost [parcel cost]
  "Calculate the total route cost"
  (if (empty? parcel)
    (print "Total Journey Cost: " cost)
    (let [{:keys [start finish]} (first parcel)
          value (costOfPath start finish)]
      (routeCost (rest parcel) (+ cost value)))))

(s/def ::name #{"Bob" "Josh" "Mary" "Susan"})
(s/def ::height-inches (s/int-in 48 90))
(s/def ::person (s/keys :req-un [::name] :opt-un [::height-inches]))
(gen/sample (s/gen ::person))
(s/exercise ::person)

(def xform (comp cat (filter #(> % 10)) (take 10)))

(defn get-page [idx k]
  (println "Fetching page" idx)
  (Thread/sleep 100)
  (future
    (k (vec (range (* idx 5)
                   (* (inc idx) 5))))))

(defn pages [xf rf k]
  (let [f (xf rf)
        start (fn process-page [idx acc page]
                (let [result (f acc page)]
                  (if (reduced? result)
                    (k @result)
                    (get-page (inc idx)
                              (partial process-page (inc idx) result)))))]
    (get-page 0 (partial start 0 (f)))))

(comment
 " 1)facebook-gaway now need permissions to get access to dynamodb
   2) set properties 
   new properties
   mqtt.inbound.uri.format=/tenants/%s
   gateway.auth=dGl0YW4tZ2F0ZXdheXNAbGl2ZW9wcy5jb206YkNzVzUzbW80NVdXc3VaNQ==
   existing properties
   mqtt.brokerURI=ssl://a325jyvl83vlc7.iot.us-east-1.amazonaws.com
   1) redis
   2) (fn [& args]) (apply hash-map (rest args) ")

(def server-conn {:pool {}
                  :spec {:host "127.0.0.1"
                         :port 6379}})
(defmacro wcar* [& body] `(car/wcar server-conn ~@body))

(wcar* (car/ping))
(def records (atom {}))
(swap! records
       (fn [m]
         (assoc m :id "id1")))
(reset! records {})

(defn test-try [& args]
  (println "executing fn")
  (throw (Exception. "throwing exception")))

(defn make-widget [& {:keys [x y] :or {x 10 y 20}}]
  (str x y))

(make-widget :x 1 :y 2)

"https://us-east-1-dev-edge.cxengagelabs.net/v1/regions"
"https://us-east-1-dev-edge.cxengagelabs.net/v1/tenants"
"https://us-east-1-dev-edge.cxengagelabs.net/v1/tenants/57e2f960-3328-11e6-8dd4-c88eee4d9f61/roles"
"https://us-east-1-dev-edge.cxengagelabs.net/v1/roles"

(comment
  "a021N00000UQL5WQAX"
  ""
  "a021N00000UQL5qQAH"
  "a021N00000UQL6KQAX")

(comment
  "a031N00000cla2BQAQ"
  {:id "a031N00000clxVkQAI", :success true, :errors []})

(comment
  {:status 201}
  {
   "id" "00T1N00001y5RJqUAM",
   "id2" "00T1N00001y5RJqUAM"
   "success" true,
   "errors" []
   })

(def sf-prefix "salesforce.api.field.prefix" "LOCC_App1__")
(def sf-postfix "salesforce.api.field.postfix" "__c")

(defn- gen-keyword
  [k]
  (if (clojure.string/starts-with? (name k) sf-prefix)
    k
    (->> (clojure.string/split (name k)  #"-")
         (map #(if (or
                    (= "url" %)
                    (= "id" %))
                 (clojure.string/upper-case %)
                 (clojure.string/capitalize %)))
         (clojure.string/join "_"
          )
         (str sf-prefix)
         (#(str % sf-postfix))
         keyword)))

(defn- gen-keyword2
  [k]
  (if (clojure.string/starts-with? (name k) (sf-prefix))
    k
    (->> (name k)
         (#(if (= "Work_Segments" %)
             (str % (sf-postfix))
             (str % (sf-postfix)))) 
         (str (sf-prefix))
         keyword)))

(def vv {:Queue_Name nil
         :Resource_ID nil
         :Wrap_Up_Time ""
         :Tenant_Name nil
         :Queue_Type "Named"
         :name "ws-35c47312-4b85-11e7-a614-0a3affe207d5"
         :Resource_Talk_Time nil
         :LOCC_App1__Disposition_ID nil
         :Interaction_ID "35c47312-4b85-11e7-a614-0a3affe207d5"
         :Recording_URL ""
         :Recorded false
         :Resource_Handle_Time ""
         :Disposition_Name nil
         :Resource_Name nil
         :Work_Accepted true
         :Queue_ID "35c47313-4b85-11e7-a614-0a3affe207d5"
         :Target_Wrap_Up_Time ""
         :Tenant_ID "35c47310-4b85-11e7-a614-0a3affe207d5"
         :Transcript_URL ""
         :Status "completed"})

(def valid-gateways #{"twilio" "plivo" "monet" "salesforce" "client" "lambda" "birst" "facebook" "zendesk" "teleopti" "email" "broadworks" "messaging"})

(reduce (fn [acc gateway]
          (conj acc
                (symbol (str "(d/defprop " gateway "-api-key-sid"))
                (str gateway ".api.key.sid")
                (str gateway "-api-key-sid")
                (symbol ")")
                (symbol (str "(d/defprop " gateway "-api-key-secret"))
                (str gateway ".api.key.secret")
                (str gateway "-api-key-secret")
                (symbol ")")))
        [] valid-gateways)

(reduce (fn [acc gateway] (conj acc {:tag (keyword (str "platform-tenant-" gateway "-api-key"))
                                     :tenant-id :tag/platform-tenant
                                     :name (str gateway)
                                     :description (str "Platform api key for " gateway)
                                     :status :enabled
                                     :role-id :tag/platform-tenant-administrator-role
                                     :platform-role-id :tag/platform-gateways-role
                                     :id (str gateway "-api-key-sid")
                                     :secret (str gateway "-api-key-secret")}))
        [] valid-gateways)

(def vv  {"tenantId"  "57e2f960-3328-11e6-8dd4-c88eee4d9f61"
          "description"  "test for api"
          "createdBy"  "0c86bfb0-6a0f-11e6-b3a9-cf548af89e0d"
          "additionalRoleIds"  []
          "updated"  "2017-06-15T19:29:16Z"
          "name"  "twilio"
          "created"  "2017-06-15T19:29:16Z"
          "secret"  "0Bf1JPDMr/ps2G4mBgjvVGCac7iP4o02nt4llQIyVNg="
          "updatedBy"  "0c86bfb0-6a0f-11e6-b3a9-cf548af89e0d"
          "status"  "enabled"
          "id"  "eb5bf780-5200-11e7-8e27-d6167ceff5c7"
          "roleId"  "57e32070-3328-11e6-8dd4-c88eee4d9f61"})

(comment
  1 migrate
  2 seed
  3 feature flag in configurator
  4 set properties (sid secret platform-tenant) at service level
  5 Migrate platform-roles)

(comment
  1 front-end flow-designer flow-library config-ui tool-bar
   body search multiselect
  2 notations send-message send-sms search-email
  3 clusterd twilio  cluster facebook  clusterd listeners
  4 manipulate cassandra by using migrate existing data
  seed new data
  in fforward
  in configurator
  in toran
  in flow-manager)

(comment 
  Macro is a way to do meta-programming
  Code is data
  All clojure code is made of lists of data
  To write a macro  you probably have to use following
  [test & body]  Attention body is a list
  (list )
  quote '
  unquote splicing ~@ tilde 
  unquote ~ tilde
  Syntax quoting ` backtick  (template)
  `(let [r# 1]) gensym  generated symbol
  when evaluating macro all lists will be treated as function)

(mapv #(Integer/parseInt (str %)) "987")
(apply mapv vector [[:a :b :c]
                    [:d :e :f]
                    [:g :h :i]])

(s/def ::queue-interactions (s/and int? pos? #(< % 3600)))
(s/def ::agent-talk-time (s/and int? pos? #(< % 3600)))
(s/def ::work-accepted (s/and int? pos? #(< % 3600)))
(s/def ::average-handle-time (s/and int? pos? #(< % 1800)))
(s/def ::queue-abandons (s/and int? pos? #(< % 500)))
(s/def ::sla (s/and int? pos? #(< % 100)))
(s/def ::average-time-to-answer (s/and int? pos? #(< % 30)))

(s/def ::queue-stats (s/keys :req-un [::queue-interactions
                                      ::agent-talk-time
                                      ::work-accepted
                                      ::average-handle-time
                                      ::queue-abandons
                                      ::sla
                                      ::average-time-to-answer]))

(defn gen-queue-stats-csv
  [queue-stats interval]
  (let [stats (map #(merge {:date (.format (java.text.SimpleDateFormat. "MM/dd/yyyy") (:interval-start-time %))
                            :start-time (.format (java.text.SimpleDateFormat. "HH:mm") (:interval-start-time %))
                            :end-time (.format (java.text.SimpleDateFormat. "HH:mm") (:interval-end-time %))
                            :interval 15}
                           (select-keys % [:queue-id
                                           :queue-interactions
                                           :work-accepted
                                           :average-handle-time
                                           :queue-abandons
                                           :sla
                                           :average-time-to-answer])) queue-stats)
        
        temp (reduce #(conj %1 (map str (vals %2))) [] stats)
        {:keys [date start-time end-time]} (first params)
        value (format "CxEngage Report\nDate: %s\n Start Time: %s\nEnd Time: %s\n\n%s" date start-time end-time temp)]
    (csv/write-csv value :delimiter \tab)))

(defn gen-csv
  []
  (let[params (map #(reduce-kv (fn [val k v] (assoc val (name k) (str v))) {} %) (gen/sample (s/gen ::queue-stats)))
       value  (reduce #(conj %1 (vals %2)) [] params)]
    (csv/write-csv value :delimiter \tab)))


(defn gen-queue-stats-csv
  [queue-stats interval]
  (let [stats (map #(merge {:date (.format (java.text.SimpleDateFormat. "MM/dd/yyyy") (:interval-start-time %))
                            :start-time (.format (java.text.SimpleDateFormat. "HH:mm") (:interval-start-time %))
                            :end-time (.format (java.text.SimpleDateFormat. "HH:mm") (:interval-end-time %))
                            :interval 15}
                           (select-keys % [:queue-id
                                           :queue-interactions
                                           :work-accepted
                                           :average-handle-time
                                           :queue-abandons
                                           :sla
                                           :average-time-to-answer])) queue-stats)
        temp (reduce #(conj %1 (map str (vals %2))) [] stats)
        {:keys [date start-time end-time]} (first stats)
        value (format "CxEngage Report\nDate: %s\n Start Time: %s\nEnd Time: %s\n\n%s" date start-time end-time temp)]
    (csv/write-csv value :delimiter \tab)))

(defn gen-queue-stats-csv
  [queue-stats interval-end-time interval]
  (let [date (.format (java.text.SimpleDateFormat. "MM/dd/yyyy") interval-end-time)
        start-time (.format (java.text.SimpleDateFormat. "HH:mm") (java.util.Date. (- (.getTime interval-end-time) interval)))
        end-time (.format (java.text.SimpleDateFormat. "HH:mm") interval-end-time) 
        stats (map #(select-keys % [:queue-id
                                    :queue-interactions
                                    :work-accepted
                                    :average-handle-time
                                    :queue-abandons
                                    :sla
                                    :average-time-to-answer])
                   queue-stats)
        params (reduce #(conj %1 (concat [date start-time (str interval)] (map str (vals %2)))) [] stats) 
        value (concat [(list "CxEngage Report")
                       (list (format "Date: %s" date))
                       (list (format "Start Time: %s" start-time))
                       (list (format "End Time: %s" end-time))
                       (list "")]
                      params)]
    (csv/write-csv value :delimiter \tab)))

(def teleopti-integrations {:properties
                            {:ftp-url "ftps://ftp.drivehq.com"
                             :platform-type-id "a8e385a8-13e5-4058-8738-8bd8b94cebf8"
                             :ftp-password "password"
                             :teleopti-url "https://liveops.teleopticloud.com/RTA/TeleoptiRtaService.svc"
                             :auth-key "auth-key"
                             :source-id "1"
                             :ftp-username "tony"}})

(def verint-integration {:properties
                         {:ftp-url "ftps://10.25.44.55"
                          :platform-type-id "a8e385a8-13e5-4058-8738-8bd8b94cebf8"
                          :ftp-password "password"
                          :auth-key "!#¤atAbgT%"
                          :source-id "1"
                          :ftp-username "srlabsftp"}})

(def dev-properties {:org-name "Serenova Tenant 1"
                     :sftp-host "10.25.44.55"
                     :sftp-port 5050
                     :sftp-username "srlabsftp"
                     :sftp-password "password"
                     :multiple-tenants false
                     :historical-enabled true})
 
(def integrations  [{:tenant-id "b1095ee0-eeed-11e6-b864-e898003f3411"
                     :properties dev-properties}
                    {:tenant-id "29c0d2a0-803a-11e6-98e4-ca81484488df"
                     :properties (assoc dev-properties :multiple-tenants true)}
                    {:tenant-id "92f50f30-661c-11e6-b1b9-ca81484488df"
                     :properties (assoc dev-properties :multiple-tenants true)}
                    {:tenant-id "cd741910-188e-11e7-9873-1b92cd79a0c3"
                     :properties dev-properties}])

(defn map-keys
  [m f]
  (->> m
       (reduce-kv (fn [acc k v]
                    (try
                      (assoc! acc (f k) v)
                      (catch Exception e
                        (assoc! acc k v))))
                  (transient {}))
       persistent!))

(defn deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn assoc-if-exist
  "assoc only if the map contains the specified key"
  [map key new-val]
  (if (contains? map key)
    (assoc map key new-val)
    map))

(comment
  Google api key  AIzaSyAJZjjnpEptvSJdzRFa61UHrenVomF1JbQ )

(keywordize-keys {"ww" {"ss" "aa"}})

(comment
  longest common subsrting)
(defn lcs
  [str1 str2]
  (loop [s1 (seq str1), s2 (seq str2), len 0, maxlen 0 acc []]
    (cond
      (>= maxlen (count s1)) acc
      (>= maxlen (+ (count s2) len)) (recur (rest s1) (seq str2) 0 maxlen acc)
      :else (let [a (nth s1 len "")
                  [b & s2r] s2
                  len1 (inc len)]
              (println a b s1 s2r len1 maxlen)
              (if (= a b)
                (recur s1 s2r len1 (if (> len1 maxlen) len1 maxlen) (conj acc a))
                (recur s1 s2r 0 maxlen acc))))))

(max-key count "asd" "bsd" "dsd" "long word")
(apply max-key val {:a 3 :b 7 :c 9})

(comment
  Problem 1
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
  )

(defn sum [n]
  (reduce + (filter #(or (zero? (rem % 3))
                         (zero? (rem % 5)))
                    (range 1 n))))

(sum 1000)

(comment
  Problem 2
  By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.)
(defn sum-fibonacci
  [n]
  (->> (iterate (fn [[a b]] [(+ a b) a]) [1 1])
       (take-while #(< (first %) n))
       (map first)
       (filter even?)
       (reduce +)))

(sum-fibonacci 4000000)

;;note: `take-while' stops traversing the collection when the predicate is false, as is different from `filter'.
(take-while neg? [1 2 3 4 -5 -6 0 1])
;;=>'()

(take-while neg? [-2 1 -2 -6 -7 1 2 3 4 -5 -6 0 1])
;;=>'(-2)
(drop-while neg? [1 -2 -6 -7 1 2 3 4 -5 -6 0 1])
;;=>'(1 -2 -6 -7 1 2 3 4 -5 -6 0 1)

;; not-any? for all elements in seq  , pred will return false
;; every? for all elements in seq  pred will return true
;; not-every? for some elements in seq  pred will return false
;; some
(comment
  Problem 3
  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143)

(defn is-prime? [n]
  (or (= 2 n)
      (not-any? #(= 0 (mod n %)) (cons 2 (range 3 (inc (Math/sqrt n)) 2)))))

(defn max-prime-factor [num]
  (loop [div 1]
    (if (and (zero? (rem num div))
             (is-prime? (quot num div)))
        (quot num div)
        (recur (inc div)))))

(comment
  Problem 4
  A palindromic number reads the same both ways. The largest palindrome made from the product of two 2 digit numbers is 9009 = 91 × 99.
  Find the largest palindrome made from the product of two 3 digit numbers.)

(defn l
  [m n]
  (loop [x m y n]
    (let [v (* x y)]
      (cond
        (= (seq (str v)) (reverse (seq (str v)))) [v x y]
        (= 1 y) (recur (dec x) n)
        :else (recur x (dec y))))))

(l 1000 1000)

;; when while in for
;; filter drop-while in for
(for [x [1 2 3]
      y [1 2 3]
      :while (<= x y)]
  [x y ])


(for [x [1 2 3]
      y [1 2 3]
      :when (<= x y)]
  [x y ])


(comment
  Problem 5
  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
  What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20)

(defn smallest [n]
  (loop [x 1]
    (let [s (filter #(is-prime? %) (range 1 n))
          m (reduce * x s)]
      (if (every? #(= 0 (mod m %)) (range 1 n))
        [m x]
        (recur (inc x))))))

(smallest 20)

(defn factorial
  [n]
  (loop [x n acc 1N]
    (if (= 1 x)
      acc
      (recur (dec x) (bigint (* acc x))))))

(comment
  Problem 6
  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum)

(defn diff [w]
  (let [s (range 1 (inc w))
        m (reduce + s)
        n (reduce (fn [acc v] (+ (* v v) acc)) 0 s)]
    (- (* m m) n)))

(comment
  Problem 7 What is the 10001 st prime number)

(defn prime
  [n]
  (loop [v 2 cnt 1]
    (if (is-prime? v)
      (if (= cnt n) v (recur (inc v) (inc cnt)))
      (recur (inc v) cnt))))

(prime 10001)

(comment
  Problem 8
  Find the thirteen adjacent digits in the 1000 digit number that have the greatest product. What is the value of this product
  (clojure.string/replace "The color is red" #"\n" ""))
(def lstr "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(defn product
  [n]
  (let [s (map #(Integer/parseInt (str %)) lstr)]
    (loop [v 1 s2 s m {}]
      (let [p  (reduce * (take n s2))]
        (if (seq s2)
          (if (> p v) (recur p (drop 1 s2) (assoc m :num (take n s2))) (recur v (drop 1 s2) m))
          [v m])))))
(comment
  Problem 9
  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.)
(defn pythagorean
  [n]
  (for [x (range 1 n) y (range 1 n)
        :let [z (- n (+ x y))]
        :when (and (> y x)
                   (= (* z z) (+ (* x x) (* y y))) )] [x y z]))
(pythagorean 1000)

(comment
  Problem 10
  Find the sum of all the primes below two million)

(defn sum-prime [n]
  (let [s (filter #(is-prime? %) (range 2 n))]
    (reduce + s)))

(def v "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
  49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

(defn get-at [i j matrix]
  (if (and (>= i 0) (< i 20) (>= j 0) (< j 20))
    (nth matrix (+ j (* i 20))) 0))

(defn euler-011 []
  (let [matrix (->> (re-seq #"[^\n]+" v)
                    (map #(re-seq #"[^ ]+" %))
                    (map #(map (fn [v] (Integer/parseInt v)) %))
                    (apply concat))
        ways (for [i (range 20) j (range 20)]
               [(map #(get-at i (+ % j) matrix) (range 4))
                (map #(get-at (+ % i) j matrix) (range 4))
                (map #(get-at (+ % i) (+ % j) matrix) (range 4))
                (map #(get-at (+ % i) (- j %) matrix) (range 4))])]
    (reduce max (map #(reduce * %) (reduce concat ways)))))


(comment
  for m*n matrix
  use vector v to save this matrix  (=  (* m n) (count v))
  use i j (and (>= i 0) (< i m)
               (>= j 0) (< j n)) to transverse matrix
  (+ j (* i m)))

(defn get-matrix  [i j s m n]
  (let [index (+ j (* i m))]
    (if (and (>= i 0)
             (>= j 0)
             (< i m)
             (< j n))
      (nth s index)
      0)))

(comment
  for a m* n matrix
  get setp numbers (4 directions E S SE SW)
  (range n) starts from 0 (included) to n (excluded))
(defn calculate
  [m n s step]
  (for [i (range m)
        j (range n)]
    [(mapv #(get-matrix i (+ % j) s m n) (range step))
     (mapv #(get-matrix (+ % i) j s m n) (range step))
     (mapv #(get-matrix (+ % i) (+ % j) s m n) (range step))
     (mapv #(get-matrix (+ % i) (- j %) s m n) (range step))]))

(comment
  Problem 12
  What is the value of the first triangle number to have over five hundred divisors)

(defn factors
  [n]
  (let [s (filter #(zero? (mod n %)) (range 1 (inc (Math/sqrt n)) (if (even? n) 1 2)))]
    (set (reduce (fn [acc v] (conj acc (quot n v) v)) [] s))))

(defn triangle
  [fnum]
  (loop [n 1]
    (let [m (reduce + (range 1 (inc n)))
          s (seq (factors m))]
      (if (> (count s) fnum)
        m
        (recur (inc n))))))

(triangle 500)

(comment
  (bigint "37107287533902102798797998220837590246510135740250")
  (BigInteger. "37107287533902102798797998220837590246510135740250")
  Problem 13)
(def l-sr  "37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690")

(defn big-sum
  []
  (let [s (->> (re-seq #"[^\n]+" l-sr)
               (map #(read-string %)))]
    (reduce + s)))

(comment
  Problem 14)

(defn longest
  [v acc]
  (cond
    (= 1 v) (conj acc 1)
    (even? v) (recur (/ v 2) (conj acc v))
    (odd? v) (recur (+ 1 (* v 3)) (conj acc v))))

(defn find-longest
  [m]
  (loop [n m f {}]
    (let [v (longest n [])]
      (if (= 1 n)
        f
        (if ( > (count v) (count (:m f)))
          (recur (dec n) (assoc f :m v :v n))
          (recur (dec n) f))))))

(defn combination [n k]
  (cond (zero? n) 0
        (zero? k) 1
        :else (/ (factorial n) (* (factorial (- n k)) (factorial k)))))

(combination (+ 2 2) 2)

(comment
  Problem 15 pascal's triangle)
(defn routes-extend [lst]
  (let [size (count lst)]
    (for [i (range (inc size))]
      (if (or (= 0 i) (= size i))
        1
        (+ (nth lst (dec i)) (nth lst i))))))

;; Elapsed time: 14.809846 msecs
(defn euler-015 []
  (let [n 20 d (inc (* n 2))]
    (nth (last (take d (iterate routes-extend [1]))) n)))

(comment
  Problem 16)

(defn my-pow
  [n m]
  (loop [y m acc 1N]
    (if (= 1 y)
      (* acc n)
      (recur (dec y) (bigint (* acc n))))))

(defn convert
  [n acc]
  (let [d (quot n 10)
        r (int (mod n 10))]
    (if (zero? d)
      (cons r acc)
      (recur d (cons r acc)))))

;;(reduce + (map #(Integer/parseInt (str %)) (seq (str  (my-pow 2 1000)))))
;; 2 1000
(defn  euler-016 [m n]
  (reduce + (convert (my-pow m n) [])))

(def t "75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")

(defn euler-018-f []
  (->> (re-seq #"[^\n]+" t)
       (map #(re-seq #"[^ ]+" %))
       (map #(map (fn [v] (Integer/parseInt v)) %))
       (map #(apply max %))
       (reduce +)))

(defn pos
  [s acc n]
  (if (seq s)
    (let [s1 (first s)
          a (nth s1 n) 
          b (nth s1 (inc n) -1 )
          pos (if (> a b) n (inc n))]
      (recur (rest s) (conj acc (max a b)) pos))
    acc))

(defn palindromic
  [n]
  (letfn [(first-p [n]
            (if (= (seq (str n)) (reverse (seq (str n))))
              n
              (recur (inc n))))
          (find-p [n]
            (if (zero? n)
              1
              (first-p (inc n))))]
    (iterate find-p
             (first-p n))))

(defn max-row [lst]
  (map #(reduce max %) (partition 2 1 lst)))

(defn step-max [lst1 lst2]
  (map + (max-row lst1) lst2))

;; Elapsed time: 0.421325 msecs
(defn euler-018 []
  (let [s (->> (re-seq #"[^\n]+" t)
               (map #(re-seq #"[^ ]+" %))
               (map #(map (fn [v] (Integer/parseInt v)) %)))]
    
    (reduce step-max (reverse s))))

(defn calendar-for [year month]
  (doto (GregorianCalendar.)
    (.set GregorianCalendar/YEAR year)
    (.set GregorianCalendar/MONTH month)
    (.set GregorianCalendar/DAY_OF_MONTH 1)))

;; Elapsed time: 30.138531 msecs
(defn euler-019 []
  (reduce +
          (for [year (range 1901 (inc 2000)) month (range 1 (inc 12))]
            (let [c (calendar-for year month)]
              (if (= GregorianCalendar/SUNDAY 
                     (.get c GregorianCalendar/DAY_OF_WEEK)) 1 0)))))

(defn euler-020 [n]
  (reduce + (convert (factorial n) [])))

(defn euler-021 [n]
  (let [s (for [v (range  n)
                :let [sum1 (reduce + (disj (factors v) v))
                      sum2 (reduce + (disj (factors sum1) sum1))]
                :when (and  (= sum2 v )
                            (not= sum1 sum2))]
            [v sum1])]
    (reduce  + (map first s))))

(defn euler-022 []
  (let [s (->>(slurp "p022_names.txt")
              (re-seq #"\w+" )
              sort
              (map-indexed #(* (inc %)
                               (reduce + (map (fn [v]
                                                (- (int v)  64))
                                              (seq %2))))))]
    (reduce + s)))

(defn redundant? [n ] (> (reduce + (disj (factors n) n)) n))

(defn euler-023 []
  (reduce + (filter
             (fn  [n] (not-any?  #(and (redundant? %)
                                       (redundant? (- n %))) (range 1 (inc (quot n 2)))))
             
             (range 1 28123))))

(defn euler-024 [n]
  (combo/nth-permutation [0 1 2 3 4 5 6 7 8 9 ] (dec n)))

(defn euler-025
  [n]
  (-> (fn [item] (if (< (count (str item)) n) true false))
      (take-while
       (map first (iterate (fn [[a b]] [b (bigint (+ a b))]) [1 1])))
      (count)
      (inc)))

