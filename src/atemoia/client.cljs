(ns atemoia.client
  (:require [reagent.core :as r]
            [reagent.dom :as rd]))

(defonce state (r/atom {}))

(defn fetch-todos
  []
  (-> (js/fetch "/af?page=0")
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


(defn item->li
  [item]
  (let [{:keys [:id :img :title :link :price :timeplace :summary]} item]
  [:figure {:key id}
   [:a {:href link} 
    [:img {:src img}]]]))

(defn map-idx 
  [idx item]
  (let [{:keys [:id :img :title :link :price :timeplace :summary]} item]
    ^{:key id} 
    [:div.container
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
