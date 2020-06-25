(ns home-finder.core
  (:gen-class)
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [hickory.select :as s]
            [hickory.core :as h]
            [tupelo.core :as tup]
            [tupelo.forest :as forest]))

(def config
  {:ZWSID ""
   :neighborhood "Albany Park, Chicago"
   :sort "days"
   :filters {:price_min 0
             :price_max 550000
             :rent_min 0
             :rent_max -1
             :beds "4+"
             :baths 0
             :sqft_min 1400
             :sqft_max 0}})

(def x
  (http/request {:method :get
                 :url "https://www.zillow.com/chicago-il"
                 :headers {:User-Agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.87 Safari/537.36"}}))

;;(def parsed-x
;;(h/as-hiccup (h/parse (:body x))))
;;
;;(forest/with-forest (forest/new-forest)
;;(let [root-hid (forest/add-tree-hiccup parsed-x)]))

(def parsed-x
  (h/as-hickory (h/parse (:body x))))

(def cleaned-x
  (:content (first (s/select (s/and (s/class "photo-cards") (s/tag :ul))
                             parsed-x))))

(defn extractor [selector-fn]
  (fn [listing]
    (-> (s/select selector-fn listing)
        first
        :content
        first)))

(def address-extractor
  (extractor (s/tag :address)))

(def list-price-extractor
  (extractor (s/class "list-card-price")))

(def days-on-zillow-extractor
  (extractor (s/class "list-card-variable-text")))

(defn brokerage-extractor [listing]
  (-> (s/select (s/class "list-card-brokerage") listing)
      first
      :content
      first
      :content
      first))

(defn hyperlink-extractor [listing]
  (-> (s/select (s/class "list-card-link") listing)
      first
      :attrs
      :href))

(parse-details ["3"
                {:type :element,
                 :attrs {:class "list-card-label"},
                 :tag :abbr,
                 :content [" " {:type :comment, :content [" "]} "bds"]}])

(defn parse-details [[n {:keys [content]}]]
  (clojure.string/join " " [n (last content)]))

(defn details-extractor [listing]
  (->> (s/select (s/class "list-card-details") listing)
       first
       :content
       (map :content)
       (map parse-details)
       ))

(defn favorite-extractor [listing]
  (let [button-label (->> (s/select (s/class "list-card-save") listing)
                          first
                          :attrs
                          :aria-label)]
    button-label
    (case button-label
      "Save" "Not favorited"
      "Unsave" "Favorited"
      "Not sure")))

(defn sale-type-extractor [listing]
  (->> (s/select (s/class "list-card-type") listing)
       first
       :content
       last))

(defn lat-long-extractor [listing]
  (let [data (-> listing
                 :content
                 first
                 :content
                 first)

        extract-geo (fn [m]
                      {:lat (get-in m [:geo :latitude])
                       :long (get-in m [:geo :longitude])})]

    (cond
      (string? data) (-> data
                         (json/parse-string keyword)
                         extract-geo)
      :else "Unknown lat-long: cannot be parsed")))

;; Useful for figuring out if I've found everything I can from a given card
(clojure.pprint/pprint (first cleaned-x))

;; The actual transformations
(clojure.pprint/pprint (map (comp flatten
                                  (juxt brokerage-extractor
                                        hyperlink-extractor
                                        days-on-zillow-extractor
                                        list-price-extractor
                                        details-extractor
                                        favorite-extractor
                                        sale-type-extractor
                                        lat-long-extractor
                                        address-extractor)) cleaned-x))

;; Trying one at a time
(clojure.pprint/pprint (map sale-type-extractor cleaned-x))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
