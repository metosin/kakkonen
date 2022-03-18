(ns kakkonen.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [kakkonen.core :as k]
            [malli.core :as m]))

(def data
  {::inc {:handler (fn [{:keys [value]} _ _] (swap! value (fnil inc 0)))}
   ::plus {:handler (fn [{:keys [value]} _ x] (swap! value (fnil + 0) x))}
   ::get {:handler (fn [{:keys [value]} _ _] @value)}
   ::nop {}})

(def d (k/dispatcher data))

(deftest dispatcher-test
  (testing "accumulated schema"
    (is (= (m/form [:map
                    {:closed true}
                    [:handler {:optional true} k/Handler]
                    [:description {:optional true} :string]
                    [:input {:optional true} :any]
                    [:output {:optional true} :any]])
           (m/form (k/-schema d)))))

  (testing "types"
    (is (= #{::inc ::plus ::get ::nop} (k/-types d))))

  (testing "actions"
    (let [actions (k/-actions d)]
      (is (= #{::inc ::plus ::get ::nop} (set (keys actions))))
      (doseq [[k _] data] (is (-> k actions :handler)))))

  (testing "dispatching"
    (let [env {:value (atom 0)}
          ! (fn [event] (k/dispatch d env nil event))]

      (testing "succesful"
        (is (! [::inc]))
        (is (! {:type ::inc}))

        (is (! [::plus 3]))
        (is (! {:type ::plus :data 3}))

        (is (= 8 (! [::get])))
        (is (= 8 (! {:type ::get}))))

      (testing "invalid key throws"
        (is (thrown? #?(:clj Exception, :cljs js/Error) (! [::invalid])))
        (is (thrown? #?(:clj Exception, :cljs js/Error) (! {:type ::invalid}))))

      (testing "no handler does not throw"
        (is (nil? (! [::nop])))
        (is (nil? (! {:type ::nop})))))))

(deftest dispatcher-creation-test

  (testing "no actions are required"
    (is (k/dispatcher {})))

  (testing "handler is not required"
    (is (k/dispatcher {::inc {}})))

  (testing "extra keys are not accepted"
    (is (thrown?
         #?(:clj Exception, :cljs js/Error)
         (k/dispatcher {::inc {:title "123"}})))))

(deftest custom-dispatcher-test
  (let [d (k/dispatcher
           {::read {:type :query
                    :permissions #{:test/read}
                    :handler (fn [_ _ _] "read")}
            ::write {:type :query
                     :permissions #{:test/write}
                     :handler (fn [_ _ _] "write")}
            ::list {:type :query
                    :handler k/-actions-handler}
            ::available {:type :query
                         :handler k/-available-actions-handler}}
           {:modules [(k/-assoc-key-module)
                      (k/-documentation-module)
                      (k/-cqrs-module)
                      (k/-permissions-module {:permissions #{:test/read :test/write}
                                              :required false
                                              :get-permissions #(-> % :user :permissions (or #{}))})
                      (k/-validate-input-module)
                      (k/-validate-output-module)
                      (k/-invoke-handler-module)]})]

    (testing "accumulated schema"
      (is (= (m/form [:map
                      {:closed true}
                      [:handler {:optional true} k/Handler]
                      [:description {:optional true} :string]
                      [:type [:enum :command :query]]
                      [:permissions {:optional true} [:set [:enum :test/read :test/write]]]
                      [:input {:optional true} :any]
                      [:output {:optional true} :any]])
             (m/form (k/-schema d)))))

    (testing "user with read permissions"
      (let [ctx {:user {:permissions #{:test/read}}}]
        (testing "can dispatch to read"
          (is (nil? (k/check d nil ctx [::read])))
          (is (nil? (k/dry-run d nil ctx [::read])))
          (is (= "read" (k/dispatch d nil ctx [::read]))))
        (testing "can't dispatch to write"
          (is (= {:data {:expected #{:test/write}
                         :key ::write
                         :missing #{:test/write}
                         :permissions #{:test/read}}
                  :type ::k/missing-permissions}
                 (k/check d nil ctx [::write])))
          (is (= {:data {:expected #{:test/write}
                         :key ::write
                         :missing #{:test/write}
                         :permissions #{:test/read}}
                  :type ::k/missing-permissions}
                 (k/dry-run d nil ctx [::write])))
          (is (thrown?
               #?(:clj Exception, :cljs js/Error)
               (k/dispatch d nil ctx [::write]))))

        (testing "list actions"
          (is (= {::available {:type :query, ::k/key ::available}
                  ::list {:type :query, ::k/key ::list}
                  ::read {:type :query, ::k/key ::read, :permissions #{:test/read}}
                  ::write {:type :query, ::k/key ::write, :permissions #{:test/write}}}
                 (k/dispatch d nil ctx [::list]))))

        (testing "list available commands"
          (is (= {::available nil
                  ::list nil
                  ::read nil
                  ::write {:data {:expected #{:test/write}
                                  :key ::write
                                  :missing #{:test/write}
                                  :permissions #{:test/read}}
                           :type ::k/missing-permissions}}
                 (k/dispatch d nil ctx [::available]))))))))
