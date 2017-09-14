(ns accumulation.serenova
  (:require [cheshire.core :as json]
            [clostache.parser :as mustache]
            [clojure.core.async :as a]
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
            [miner.ftp :as ftp]))


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
  " 1)facebook-gaway now need permissions to get access to dynamodb
   2) set properties 
   new properties
   mqtt.inbound.uri.format=/tenants/%s
   gateway.auth=dGl0YW4tZ2F0ZXdheXNAbGl2ZW9wcy5jb206YkNzVzUzbW80NVdXc3VaNQ==
   existing properties
   mqtt.brokerURI=ssl://a325jyvl83vlc7.iot.us-east-1.amazonaws.com
   1) redis
   2) (fn [& args]) (apply hash-map (rest args) ")
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

(def temp-v {:Queue_Name nil
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

(def temp-p  {"tenantId"  "57e2f960-3328-11e6-8dd4-c88eee4d9f61"
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
