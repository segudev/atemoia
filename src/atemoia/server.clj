(ns atemoia.server
  (:gen-class)
  (:require [atemoia.parsing :as parse]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [hiccup2.core :as h]
            [hiccup.page :as p]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.sse :as sse]
            [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as chan])
  (:import (java.net URI)
           (org.eclipse.jetty.servlet ServletContextHandler)
           (org.eclipse.jetty.server.handler.gzip GzipHandler)))

(set! *warn-on-reflection* true)

(extend-protocol cheshire.generate/JSONable
  java.time.LocalDateTime
  (to-json [dt gen]
    (cheshire.generate/write-string gen (str dt))))


(defonce state (atom nil))
(defonce results (atom nil))

(defn index
  [_]
  (let [html [:html
              {:lang "en"}
              [:head
               [:meta {:charset "UTF-8"}]
               [:link {:rel "icon" :href "data:"}]
               (p/include-css "/atemoia/styles.css")
               [:meta {:name    "viewport"
                       :content "width=device-width, initial-scale=1.0"}]
               [:meta {:name    "theme-color"
                       :content "#00000"}]
               [:meta {:name    "description"
                       :content "A simple full-stack clojure app"}]
               [:title "atemoia"]]
              [:body
               [:div {:id "atemoia"} "loading ..."]
               [:script
                {:src "/atemoia/main.js"}]]]]
    {:body    (->> html
                   (h/html {:mode :html})
                   (str "<!DOCTYPE html>\n"))
     :headers {"Content-Security-Policy" ""
               "Content-Type"            "text/html"}
     :status  200}))

(defn af-get
  [_]
  {:body   (->> @results
                json/generate-string)
   :headers {"Content-Type" "application/json"}
   :status  200})

(defn stream-ready [evt-chan context]
  (dotimes [i 10]
    (when-not (chan/closed? evt-chan)
      (async/>!! evt-chan {:name "counter" :data i})
      (Thread/sleep 1000)))
  (async/close! evt-chan))

(def routes
  `#{["/" :get index]
     ["/af" :get af-get]
     ["/counter" :get (sse/start-event-stream stream-ready)]})

(defn fetch-first-page [state]
  (clojure.pprint/pprint (str "Parsing @ " (java.time.LocalDateTime/now)))
  (swap! state update-in [:results] conj (parse/sorted-annonces))
  (clojure.pprint/pprint (first (first (:results @state)))))

(defn update-state-results! [state]
  (swap! state update-in [:results] conj (parse/sorted-annonces))
  (println "state results updated"))

(defn parsing-loop []
  (async/go-loop []
    #_(update-state-results! state)
    (fetch-first-page state)
    (async/<! (async/timeout 30000))
    (recur)))

(defn -main
  [& _]
  (let [port (or (edn/read-string (System/getenv "PORT")) 8080)]
    (swap! state
           (fn [st]
             (some-> st http/stop)
             (-> {::http/port              port
                  ::http/file-path         "target/classes/public"
                  ::http/resource-path     "public"
                  ::http/host              "0.0.0.0"
                  ::http/type              :jetty
                  ::http/routes            (fn [] (route/expand-routes routes))
                  ::http/join?             false
                  ::http/container-options {:context-configurator (fn [^ServletContextHandler context]
                                                                    (let [gzip-handler (GzipHandler.)]
                                                                      (.addIncludedMethods gzip-handler (into-array ["GET" "POST"]))
                                                                      (.setExcludedAgentPatterns gzip-handler (make-array String 0))
                                                                      (.setGzipHandler context gzip-handler))
                                                                    context)}}
                 http/default-interceptors
                 http/dev-interceptors
                 http/create-server
                 http/start)))
    (println "started: " port)
    (reset! results (parse/sorted-annonces 1 11))
    #_(parsing-looper)))

(defn dev-main
  [& _]
  (-> `shadow.cljs.devtools.server/start!
      requiring-resolve
      (apply []))
  (-> `shadow.cljs.devtools.api/watch
      requiring-resolve
      (apply [:atemoia]))
  (-main))

(comment

  (parsing-loop)

  (def s #{})
  (def one {:id 1})
  (def two {:id 2})
  (conj s one two))

(comment

  (spit "./art.json"
        (json/generate-string (parse/sorted-annonces))))
