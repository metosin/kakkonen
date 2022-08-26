(ns viesti.dev
  (:require [malli.core :as m]
            [malli.dev.pretty :as pretty]
            [malli.dev.virhe :as virhe]
            [malli.error :as me]
            [viesti.core :as v]))

(defmethod virhe/-format ::v/invalid-action [_ _ {:keys [type types]} p]
  {:body [:group
          (pretty/-block "Dispatch value:" (virhe/-visit type p) p) :break :break
          (pretty/-block "Should be one of:" (virhe/-visit types p) p)]})

(defmethod virhe/-format ::v/no-handler [_ _ {:keys [type action]} p]
  {:body [:group
          (pretty/-block "No handler for type:" (virhe/-visit type p) p) :break :break
          (pretty/-block "Action:" (virhe/-visit action p) p)]})

(defmethod virhe/-format ::v/missing-permissions [_ _ {:keys [type permissions expected missing]} p]
  {:body [:group
          (pretty/-block "With action:" (virhe/-visit type p) p) :break :break
          (pretty/-block "Missing permission:" (virhe/-visit missing p) p) :break :break
          [:group (virhe/-text "Found " p) (virhe/-visit permissions p) (virhe/-text ", expected " p) (virhe/-visit expected p)]]})

(defmethod virhe/-format ::v/invalid-actions [_ _ {:keys [explanation]} p]
  (assoc (virhe/-format ::m/explain nil (me/with-spell-checking explanation) p) :title "Invalid Actions"))

(defmethod virhe/-format ::v/input-schema-error [_ _ {:keys [explanation]} p]
  (assoc (virhe/-format ::m/explain nil explanation p) :title "Input Schema Error"))

(defmethod virhe/-format ::v/output-schema-error [_ _ {:keys [explanation]} p]
  (assoc (virhe/-format ::m/explain nil explanation p) :title "Output Schema Error"))

;;
;; Public API
;;

(defn dispatcher
  ([actions] (dispatcher actions (v/-default-options)))
  ([actions options]
   (let [thrower (pretty/thrower (pretty/-printer {:title "Dispatch Error"}))
         catch! (fn [e] (if (v/-managed? e)
                          (let [{:keys [type data]} (or (ex-data e) {:type (type e), :data {:message (ex-message e)}})]
                            (thrower type data))
                          (throw e)))
         dispatcher (try (v/dispatcher actions options)
                         (catch #?(:clj Exception, :cljs js/Error) e (catch! e)))]
     (v/-proxy
      dispatcher
      (fn [env ctx event]
        (try (v/-dispatch dispatcher env ctx event)
             (catch #?(:clj Exception, :cljs js/Error) e (catch! e))))))))
