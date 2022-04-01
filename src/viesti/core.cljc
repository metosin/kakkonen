(ns viesti.core
  (:require [clojure.set :as set]
            [malli.core :as m]
            [malli.util :as mu]))

;;
;; Protocols
;;

(defprotocol Dispatcher
  (-types [this])
  (-actions [this])
  (-schema [this])
  (-options [this])
  (-check [this env ctx event])
  (-dispatch [this env ctx event]))

;;
;; Schemas
;;

(def Env (m/schema [:maybe :map]))
(def Type (m/schema :qualified-keyword))
(def Data (m/schema [:maybe :map]))
(def Message (m/schema [:tuple Type Data]))
(def MapMessage (m/schema [:map [:type Type] [:data Data]]))
(def Context (m/schema [:maybe :map]))
(def Response (m/schema :any))
(def Handle (m/schema [:=> [:cat Env Context Message] Response]))
(def Handler (m/schema [:map {:closed true} [:handler {:optional true} Handle]]))

;;
;; Impl
;;

(defn -fail!
  ([type] (-fail! type nil))
  ([type data] (throw (ex-info (str type " " (pr-str data)) {:type type, :data data}))))

(defn -nil-handler [_env _ctx _data])

(defn -compile [type data {:keys [modules] :as options}]
  (reduce (fn [data {:keys [compile]}] (or (if compile (compile type data options)) data))
          (update data :handler (fnil identity -nil-handler))
          (reverse modules)))

(defn -ctx [ctx dispatcher validate invoke]
  (-> ctx (assoc ::dispatcher dispatcher) (assoc ::validate validate) (assoc ::invoke invoke)))

(defn -event [data] (if (map? data) data {:type (first data), :data (second data)}))

(defn -proxy [d f]
  (reify Dispatcher
    (-types [_] (-types d))
    (-actions [_] (-actions d))
    (-schema [_] (-schema d))
    (-options [_] (-options d))
    (-check [_ env ctx event] (-check d env ctx event))
    (-dispatch [_ env ctx event] (f env ctx event))))

;;
;; Modules
;;

(defn -invoke-handler-module
  ([] (-invoke-handler-module nil))
  ([{:keys [dispatch]}]
   {:name '-invoke-handler-module
    :compile (fn [_ {:keys [handler] :as data} _]
               (assoc data :handler (fn [env ctx data]
                                      (let [f (if (::invoke ctx) (or dispatch handler) -nil-handler)]
                                        (f env ctx data)))))}))

(defn -assoc-type-module []
  {:name '-assoc-type-module
   :compile (fn [type data _] (assoc data ::type type))})

(defn -kind-module [{:keys [values]}]
  {:name '-kind-module
   :schema [:map [:kind (into [:enum] values)]]})

(defn -documentation-module
  ([] (-documentation-module nil))
  ([{:keys [required]}]
   {:name '-documentation-module
    :schema [:map [:description (when-not required {:optional true}) :string]]}))

(defn -validate-input-module []
  {:name '-validate-input-module
   :schema [:map [:input {:optional true} :any]]
   :compile (fn [type {:keys [input handler] :as data} _]
              (when input
                (let [schema (m/schema input)
                      validate (m/validator schema)
                      explain (m/explainer schema)
                      handler (fn [env ctx data]
                                (when (and (::validate ctx) (not (validate data)))
                                  (-fail! ::input-schema-error {:type type, :explanation (explain data)}))
                                (handler env ctx data))]
                  (-> data
                      (assoc :handler handler)
                      (assoc :input (m/form schema))))))})

(defn -validate-output-module []
  {:name '-validate-output-module
   :schema [:map [:output {:optional true} :any]]
   :compile (fn [type {:keys [output handler] :as data} _]
              (when output
                (let [schema (m/schema output)
                      validate (m/validator schema)
                      explain (m/explainer schema)
                      handler (fn [env ctx data]
                                (if (::invoke ctx)
                                  (let [response (handler env ctx data)]
                                    (when-not (validate response)
                                      (-fail! ::output-schema-error {:type type, :explanation (explain response)}))
                                    response)
                                  (handler env ctx data)))]
                  (-> data
                      (assoc :handler handler)
                      (assoc :output (m/form schema))))))})

(defn -permissions-module [{:keys [required permissions get-permissions]
                            :or {get-permissions (fn [_ ctx _] (:permissions ctx))}}]
  {:name '-permissions-module
   :schema [:map [:permissions (if-not required {:optional true}) [:set (into [:enum] permissions)]]]
   :compile (fn [type {:keys [handler permissions] :as data} _]
              (when permissions
                (let [handler (fn [env ctx data]
                                (let [user-permissions (get-permissions env ctx data)]
                                  (let [missing (set/difference permissions user-permissions)]
                                    (if (seq missing)
                                      (-fail! ::missing-permissions {:permissions user-permissions
                                                                     :expected permissions
                                                                     :missing missing
                                                                     :type type})
                                      (handler env ctx data)))))]
                  (assoc data :handler handler))))})

(defn -features-module [{:keys [required features get-features]
                         :or {get-features (fn [env _ _] (:features env))}}]
  {:name '-features-module
   :schema [:map [:features (if-not required {:optional true}) [:set (into [:enum] features)]]]
   :compile (fn [type {:keys [handler features] :as data} _]
              (when features
                (let [handler (fn [env ctx data]
                                (let [env-features (get-features env ctx data)]
                                  (let [missing (set/difference features env-features)]
                                    (if (seq missing)
                                      (-fail! ::missing-features {:features env-features
                                                                  :expected features
                                                                  :missing missing
                                                                  :type type})
                                      (handler env ctx data)))))]
                  (assoc data :handler handler))))})

;;
;; Default Handlers
;;

(defn -actions-handler [_ ctx _]
  (reduce-kv (fn [acc type handler] (assoc acc type (dissoc handler :handler))) {} (-actions (::dispatcher ctx))))

(defn -available-actions-handler [env {:keys [::dispatcher] :as ctx} _]
  (reduce-kv
   (fn [acc type data]
     (assoc acc type (-check dispatcher env (-ctx ctx dispatcher false false) [type data]))) {} (-actions dispatcher)))

;;
;; Defaults
;;

(defn -default-options []
  {:modules [(-assoc-type-module)
             (-documentation-module)
             (-validate-input-module)
             (-validate-output-module)
             (-invoke-handler-module)]})

;;
;; Public API
;;

;; TODO: map-first dispatching
;; TODO: naming of things
(defn dispatcher
  ([actions] (dispatcher actions (-default-options)))
  ([actions {:keys [modules] :as options}]
   (let [schema (->> modules (keep :schema) (reduce mu/merge Handler))]
     (when-let [explanation (m/explain [:map-of Type schema] actions)]
       (-fail! ::invalid-actions {:explanation explanation}))
     (let [actions (reduce-kv (fn [acc key data] (assoc acc key (-compile key data options))) {} actions)
           types (set (keys actions))]
       (reify Dispatcher
         (-types [_] types)
         (-actions [_] actions)
         (-schema [_] schema)
         (-options [_] options)
         (-check [_ env ctx event]
           (let [{:keys [type data]} (-event event)]
             (if-let [action (actions type)]
               (let [handler (:handler action)]
                 (try (handler env ctx data) (catch #?(:clj Exception, :cljs js/Error) e (ex-data e))))
               (-fail! ::invalid-action {:type type, :types types}))))
         (-dispatch [_ env ctx event]
           (let [{:keys [type data]} (-event event)]
             (if-let [action (actions type)]
               (let [handler (:handler action)]
                 (handler env ctx data))
               (-fail! ::invalid-action {:type type, :types types})))))))))

(defn check [dispatcher env ctx event] (-check dispatcher env (-ctx ctx dispatcher false false) event))
(defn dry-run [dispatcher env ctx event] (-check dispatcher env (-ctx ctx dispatcher true false) event))
(defn dispatch [dispatcher env ctx event] (-dispatch dispatcher env (-ctx ctx dispatcher true true) event))