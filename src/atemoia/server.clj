(ns atemoia.server
  (:gen-class)
  (:require [atemoia.parsing :as parse]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [hiccup2.core :as h]
            [hiccup.page :as p]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.interceptor :as interceptor]
            [next.jdbc :as jdbc])
  (:import (java.net URI)
           (org.eclipse.jetty.servlet ServletContextHandler)
           (org.eclipse.jetty.server.handler.gzip GzipHandler)))

(set! *warn-on-reflection* true)

(defn database->jdbc-url
  [database-url]
  (let [uri (URI/create database-url)
        creds (.getUserInfo uri)
        base-uri (URI. "postgresql" nil
                       (.getHost uri) (.getPort uri) (.getPath uri)
                       (string/join "&"
                                    (map (partial string/join "=")
                                         (zipmap ["user" "password"] (string/split creds #":" 2))))
                       nil)]
    (str (URI. "jdbc" (str base-uri) nil))))

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

(defn list-todo
  [{::keys [atm-conn]}]
  (let [response (jdbc/execute! atm-conn
                                ["SELECT * FROM todo"])]
    {:body    (json/generate-string response)
     :headers {"Content-Type" "application/json"}
     :status  200}))

(defn create-todo
  [{::keys [atm-conn]
    :keys  [body]}]
  (let [note (some-> body
                     io/reader
                     (json/parse-stream true)
                     :note)]
    (jdbc/execute! atm-conn
                   ["INSERT INTO todo (note) VALUES (?);
        DELETE FROM todo WHERE id IN (SELECT id FROM todo ORDER BY id DESC OFFSET 10)"
                    note])
    {:status 201}))

(defn install-schema
  [{::keys [atm-conn]}]
  (jdbc/execute! atm-conn
                 ["CREATE TABLE todo (id serial, note text)"])
  {:status 202})

(defonce state
  (atom nil))

(defn af-get
  [_]
  {:body   (-> (:results @state)
               json/generate-string) #_(let [page (-> (get-in request [:query-params :page])
                             Integer/valueOf)]
                (-> (parse/parse-range 1 page)
                    json/generate-string))
  
   :headers {"Content-Type" "application/json"}
   :status  200})

#_(spit "./range2.json" (-> (parse/parse-range 1 10)
                          json/generate-string))
(def routes
  `#{["/" :get index]
     ["/todo" :get list-todo]
     ["/todo" :post create-todo]
     ["/install-schema" :post install-schema]
     ["/af" :get af-get]})

(defn -main
  [& _]
  (let [port (or (edn/read-string (System/getenv "PORT"))
                 8080)
        database-url (or (System/getenv "DATABASE_URL")
                         "postgres://postgres:postgres@127.0.0.1:5432/postgres")
        jdbc-url (database->jdbc-url database-url)]
    (swap! state
           (fn [st]
             (some-> st http/stop)
             (-> {::http/port              port
                  ::http/file-path         "target/classes/public"
                  ::http/resource-path     "public"
                  ::http/host              "0.0.0.0"
                  ::http/type              :jetty
                  ::http/routes            (fn []
                                             (route/expand-routes routes))
                  ::http/join?             false
                  ::http/container-options {:context-configurator (fn [^ServletContextHandler context]
                                                                    (let [gzip-handler (GzipHandler.)]
                                                                      (.addIncludedMethods gzip-handler (into-array ["GET" "POST"]))
                                                                      (.setExcludedAgentPatterns gzip-handler (make-array String 0))
                                                                      (.setGzipHandler context gzip-handler))
                                                                    context)}
                  :results (parse/parse-range 1 11)}

                 http/default-interceptors
                 (update ::http/interceptors
                         (partial cons
                                  (interceptor/interceptor {:enter (fn [ctx]
                                                                     (-> ctx
                                                                         (assoc-in [:request
                                                                                    ::atm-conn
                                                                                    :jdbcUrl]
                                                                                   jdbc-url)))})))
                 http/dev-interceptors
                 http/create-server
                 http/start)))
    (println "started: " port)))

(defn dev-main
  [& _]
  ;; docker run --name my-postgres --env=POSTGRES_PASSWORD=postgres --rm -p 5432:5432 postgres:alpine
  (-> `shadow.cljs.devtools.server/start!
      requiring-resolve
      (apply []))
  (-> `shadow.cljs.devtools.api/watch
      requiring-resolve
      (apply [:atemoia]))
  (-main))
