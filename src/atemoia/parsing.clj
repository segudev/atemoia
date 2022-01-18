(ns atemoia.parsing
  (:require [hickory.core :as h]
            [hickory.select :as s]
            [clojure.string :as string]
            [clojure.core.async :as async])
  (:import [java.time LocalDateTime]))


(def DOMAIN "https://fr.audiofanzine.com")
(def ENDPOINT "/synthetiseur/petites-annonces")

(defn url [page]
  (str DOMAIN ENDPOINT (format "/p.%d.html" page)))

(defn af-selling [page]
  (-> (slurp (url page))
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
  (if-let [url (some-> (s/select
                        (s/class "playlist-row-thumbnail")
                        annonce)
                       first :content first :content first :attrs :srcset)]
    (sanitize-img-url url)))

(defn title [annonce]
  (some-> (s/select (s/class "playlist-row-title")
                    annonce)
          first :content first))

(defn link [annonce]
  (str DOMAIN (-> (s/select (s/class "link-wrapper")
                            annonce)
                  first :attrs :href)))

(defn price [annonce]
  (some-> (s/select (s/class "playlist-price")
                    annonce)
          first :content first sanitize-price))

(defn replace-and-trim [sentence char]
  (string/trim (string/replace sentence (str char " ") "")))

(defn summary [annonce]
  (-> (s/select (s/class "main-text")
                annonce)
      first :content first))

(defn hour [annonce]
  (some-> (s/select (s/class "playlist-row-meta")
                    annonce)
          first :content first :content last (replace-and-trim "à")))

(defn place [annonce]
  (if-let [place (some-> (s/select (s/class "playlist-row-meta") annonce)
                         first :content second :content first)]
    (replace-and-trim place "-")))

(defn timeplace [annonce]
  (let [time (hour annonce)]
    (if-let [place (place annonce)]
      (str time " - " place)
      time)))

(defn to-LocalTime [time]
  (try
    (if (string/starts-with? time "le")
      (.atStartOfDay (java.time.LocalDate/parse (string/replace time #"le " "") (java.time.format.DateTimeFormatter/ofPattern "dd/MM/yy")))
      (.atTime (java.time.LocalDate/now) (java.time.LocalTime/parse time (java.time.format.DateTimeFormatter/ofPattern "HH:mm"))))
    (catch Exception e (println (.getMessage e)))))

(defn local-date-time [annonce]
  (to-LocalTime (hour annonce)))



(defn parse-annonce [annonce]
  (zipmap [:id :img :title :link :price :time :place :summary]
          ((juxt id img title link price local-date-time place summary) annonce)))

#_(defn assoc-hash [obj]
    {:hash (hash obj) :annonce obj})

(defn current-annonces [page]
  (mapv (partial parse-annonce)
        (extract-annonces (af-selling page))))

; Synchronously parse reducing on page range
(defn parse-range [start end]
  (set (reduce (fn [res next]
                 (concat res (current-annonces next)))
               []
               (range start end))))

(comment
  
  (clojure.pprint/pprint
  (sort-by :time (parse-range 1 5)))
  
  )

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