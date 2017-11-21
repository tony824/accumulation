(ns accumulation.core
  (:require [ring.util.codec :as codec]
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
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [clojure.walk :refer [keywordize-keys]]))

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

(defn flatten-content [node]
  (lazy-seq
   (if (string? node)
     (list node)
     (mapcat flatten-content (:content node)))))

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



(defn start2
  [p1 p2 &[p3]]
  (str p1 p2 (or p3 "what")))


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


(def server-conn {:pool {}
                  :spec {:host "127.0.0.1"
                         :port 6379}})
(defmacro wcar* [& body] `(car/wcar server-conn ~@body))

#_(wcar* (car/ping))
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


(defn overlap-each-other?
  [a b]
  (let [s1 (set a)
        s2 (set b)]
    (and (seq (clojure.set/difference  s1 s2))
         (seq (clojure.set/difference s2 s1))
         (seq (clojure.set/intersection s2 s1)))))

(defn overlap
  [params]
  (loop [f (first params) r (rest params) acc {}]    
    (if (seq r)
      (let [t (->> (group-by (partial overlap-each-other? f)  r)
                   (reduce-kv (fn [m k v] (if (or (nil? k)
                                                  (contains? m k ))
                                            m
                                            (assoc m k (cons f v))))
                              acc))]
        (recur (first r) (rest r) t))
      acc)))

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

(-> (clojure.lang.PersistentQueue/EMPTY) (conj 1 2 3) peek)
(-> (clojure.lang.PersistentQueue/EMPTY) (conj 1 2 3) pop seq)

(numerator (/ 123 10))
(denominator (/ 123 10))

(zipmap '(1 2 3) (repeat 0))
(re-seq #"\w+" "one-two/three")


(comment
  1. 3 ways to build function
  partial  complement comp
  2. pure functions :no observable side effects  for the same arguments return the same result
  3. functions as arguements functions as results)

(defn fnth [n]
  (apply comp
         (cons first
               (take (dec n) (repeat rest)))))

;; A list of functions. and use apply comp to build another function

'(first rest rest rest rest)

((comp first rest rest rest rest ) [:a :b :c :d :e :f :g])

((fnth 5) '[a b c d e])

(complement even?)

(comp not even?)

(defn join
  {:test (fn []
           (assert
            (= (join "," [1 3 3]) "1,3,3")))}
  [sep s]
  (apply str (interpose sep s)))

(test #'join)
(meta #'join)
(clojure.test/run-tests)

(defn ^:private ^:dynamic sum [nums]
  (map + nums))

(defn-  sum2 [nums]
  (map + nums))

(defn f [] false)

(with-redefs [f  (fn [] true)] 
  (f))

(with-redefs-fn {#'f (fn [] true)}
  #(f))

(defn slope [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(set! *assert* false)

(comment
  return the min key of a vector
  "min-key"/"max-key" to "min"/"max" like "sort-by" to "sort"
  vector as function
  )

(let [v [12 2 3 43]] (apply min-key v (range (count v))))

(apply min-key #(Math/abs %) [-3 1 4])

(comment
  a closure is
  a function that has access to locals from the context where it was created

  http://clojure-doc.org/articles/language/macros.html
  
  The key difference between quote and syntax quote is that symbols within a syntax quoted form are automatically namespace-qualified.
  Another difference between quoting and syntax quoting is that the latter allows you to unquote forms using the tilde)

(list 1 [2 3])
(list* 1 [2 3])


(comment

  special characters in macro

  ~'symbol at times in Clojure macros for selectively capturing a symbolic name in the
  body of a macro
  The reason for this bit of awkwardness 11 is that Clojure’s syntax-
  quote attempts to resolve symbols in the current context, resulting in fully qualified
  symbols. Therefore, ~' avoids that resolution by unquoting a quote
  )

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(mapcat (fn [[k v]] [k `'~v]) '{a 1, b 2})

(contextual-eval '{a 1, b 2} '(+ a b))


(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (if ~'it       
       (do ~@body))))

(awhen [1 2 3] (it 2))

(let [x 9, y '(- x)]
  (println `y)  ;;has nothing to do with bind use y as macro symbol
  (println ``y)  ;; has nothing to do with bind use y as macro symbol and syntax-quote it again
  (println ``~y)  ;; the same as `y
  (println ``~~y) ;; the same as y
  (println 'y) ;; has nothing to do bind , symbol
  )

(comment
  key difference between functions and macros is that function arguments are fully evaluated before they’re passed to the function, whereas macros receive arguments as unevaluated data)
