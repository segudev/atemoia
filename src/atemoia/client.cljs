(ns atemoia.client
  (:require [reagent.core :as r]
            [reagent.dom :as rd]))

(defonce state (r/atom {}))

(defn fetch-todos
  []
  (-> (js/fetch "/af")
      (.then (fn [response]
               (when-not (.-ok response)
                 (throw (ex-info (.-statusText response)
                                 {:response response})))
               (swap! state dissoc :error)
               (.json response)))
      (.then (fn [items]
               (swap! state assoc :todos (js->clj items
                                                  :keywordize-keys true))))
      (.catch (fn [ex]
                (swap! state assoc :error (ex-message ex))))))

(defn map-idx 
  [idx item]
  (let [{:keys [:id :img :title :link :price :time :place :summary]} item]
    ^{:key id} 
    [:div.container 
     [:div.time (str time " - "place)]
      [:a {:href link} [:img.image {:src img}] 
       [:div.middle
        [:div.text price "€"]]]]
    ))

(defn ui-root
  []
  (let [{:keys [error todos]} @state]
    #_[:main
     (when error [:<> [:pre (str error)] "Error"])
     [:div.container (map item->li todos)]]
  [:main (map-indexed map-idx todos)]))

(defn start
  []
  (some->> (js/document.getElementById "atemoia")
           (rd/render [ui-root]))
  (fetch-todos))

(defn after-load
  []
  (some->> (js/document.getElementById "atemoia")
           (rd/render [ui-root])))
