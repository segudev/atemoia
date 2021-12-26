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

(defn ui-root
  []
  (let [{:keys [error todos]} @state]
    [:div
     (when error [:<>[:pre (str error)] "Error"])
     [:ul
      (for [i todos]
        [:li
         (get i :title)])]]))

(defn start
  []
  (some->> (js/document.getElementById "atemoia")
    (rd/render [ui-root]))
  (fetch-todos))

(defn after-load
  []
  (some->> (js/document.getElementById "atemoia")
    (rd/render [ui-root])))
