(ns atemoia.parsing
  (:require [hickory.core :as h]
            [hickory.select :as s]
            [clj-http.client :as client]
            [clojure.string :as string]))

(def DOMAIN "https://fr.audiofanzine.com")
(def ENDPOINT "/synthetiseur/petites-annonces")

(defn url [page]
  (str DOMAIN ENDPOINT (format "/p.%d.html" page)))

(defn af-selling [page]
  (-> (client/get (url page) {:insecure? true})
      :body
      h/parse
      h/as-hickory))

(defn extract-annonces [page]
  (s/select (s/class "clearfix") page))

(defn handle-error [e s]
  (println "Price parsing exception for " s)
  (println e)
  nil)

(defn sanitize-price [s]
  (try (-> s
           (string/replace #"[  €]" "")
           (string/replace #"," ".")
           (Float/parseFloat))
       (catch Exception error
         (handle-error error s))))

(defn id [annonce]
  (:id (:attrs annonce)))

(defn sanitize-img-url [url]
  (string/replace
   (second (string/split url #", "))
   #" 2x" ""))

(defn img [annonce]
  (if-let [url (-> (s/select 
                    (s/class "playlist-row-thumbnail") 
                    annonce)
                   first :content first :content first :attrs :srcset)]
    (sanitize-img-url url)))

(defn title [annonce]
  (-> (s/select (s/class "playlist-row-title")
                annonce)
      first :content first))

(defn link [annonce]
  (str DOMAIN (-> (s/select (s/class "link-wrapper")
                            annonce)
                  first :attrs :href)))

(defn price [annonce]
  (-> (s/select (s/class "playlist-price")
                annonce)
      first :content first sanitize-price))

(defn replace-and-trim [sentence char]
  (string/trim (string/replace sentence (str char " ") "")))

(defn hour [annonce]
  (-> (s/select (s/class "playlist-row-meta")
                annonce)
      first :content first :content last (replace-and-trim "à")))

(defn place [annonce]
  (if-let [place (-> (s/select (s/class "playlist-row-meta") annonce)
                     first :content second :content first)]
    (replace-and-trim place "-")))

(defn timeplace [annonce]
  (let [time (hour annonce)]
    (if-let [place (place annonce)]
      (str time " - " place)
      time)))

(defn summary [annonce]
  (-> (s/select (s/class "main-text")
                annonce)
      first :content first))

(defn parse-annonce [annonce]
  (zipmap [:id :img :title :link :price :timeplace :summary]
          ((juxt id img title link price timeplace summary) annonce)))

(defn current-annonces [page]
  (map (partial parse-annonce)
       (extract-annonces (af-selling page))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defn test-parser [parser]
;;   (map #(parser %)
;;        (extract-annonces (af-selling 0))))
;; (test-parser place)

;;(def now (.atZone (java.time.Instant/now ) (java.time.ZoneId/of "Europe/Paris")))

;;(def testing (extract-annonces af-selling-main-page))
;;(map #(id %) testing)
;;(map #(img %) testing)
;;(map #(title %) testing)
;;(map #(rel-url %) testing)
;;(map #(price %) testing)