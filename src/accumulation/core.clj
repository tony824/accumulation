(ns accumulation.core
  (:require [clostache.parser :as mustache]
            [ring.util.codec :as codec]
            [clojure.core.async :as a]
            [clj-time.coerce :as ct]
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
            [clj-http.client :as client]
            [taoensso.carmine :as car :refer (wcar)]
            [compojure.core :refer :all]
            [clojure-csv.core :as csv]
            [clj-ssh.ssh :as ssh]
            [clj-ssh.cli :as cli]
            [miner.ftp :as ftp]
            [clojure-tensorflow.ops :as tf]
            [clojure-tensorflow.layers :as layer]
            [clojure-tensorflow.optimizers :as optimize]
            [clojure-tensorflow.core :refer [run with-graph with-session]]))

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
