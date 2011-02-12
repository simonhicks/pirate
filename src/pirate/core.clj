(ns pirate.core
  (:use [org.simonhicks.debug :only (db+ ct)])
  (:use [clojure.contrib.math :only (ceil)])
  (:require [net.cgrand.enlive-html :as html])
  (:use [clojure.contrib.str-utils :only (re-split)]
        [clojure.contrib.duck-streams :only (copy)]
        [clojure.contrib.io :only (input-stream)]
        [clojure.java.io :only (file)]
        [clojure.contrib.str-utils2 :only (join)]))

(defn- *categories* 
  "returns the code for the given category"
  [cat] (get {:all     "0"   :audio   "100"
              :video   "200" :apps    "300"
              :games   "400" :other   "600"} cat "0"))

(defn- category-string 
  "maps a vector of category keywords to a part of the url string"
  [cats] (if (keyword? cats)
          (*categories* cats)
          (join "," (map *categories* cats))))

(defn- escape-url 
  "returns a valid url that corresponds to the given domain and path"
  [domain path]
  (let [uri (java.net.URI. "http" domain path nil)]
    (.toASCIIString uri)))

(defn- get-url 
  "returns the url for the given pirate bay search. Currently, the only option 
  is :category which can either take one of :all, :audio, :video, :apps, :games
  or :other or a vector containing several of them."
  [query opts]
  (let [domain "thepiratebay.org" 
        {:keys [only-page category]} opts
        path (join "/" ["/search" query only-page "7" (category-string category)])]
    (escape-url domain path)))

(def *result-selector*
  [[:tr (html/but :.header)]])

(def *name-selector*
  [[:div.detName]])

(def *size-selector*
  [[:font.detDesc]])

(def *vip-selector*
  [[:img (html/attr= :title "VIP")]])

(def *trusted-selector*
  [[:img (html/attr= :title "Trusted")]])

(def *health-selectors*
  [[:td (html/attr= :align "right")]])

(def *category-selector*
  [:td.vertTh :center :a])

(def *download-link-selector*
  [[:a (html/attr= :title "Download this torrent")]])

(defn- fetch-url 
  "fetches and parses the content at the gven url"
  [url] (html/html-resource (java.net.URL. url)))


(defn- search-data 
  "performs the given query and returns a sequence of result rows as parsed html"
  [query opts] (html/select (fetch-url (get-url query opts)) 
                            *result-selector*))

(defn- extract-name 
  "Extracts the name from a given result row"
  [h] (-> h (first) (:content) (first) (:content) (first)))

(defn- extract-size-number 
  "extracts the number from the file size"
  [s] (Float/parseFloat (re-find #"\d+\.?\d*" s)))

(defn- extract-size-unit 
  "extracts the unit from the file size"
  [s] (keyword (re-find #"\w+$" s)))

(def *unit-mult-factors*
  {\B 1/1048576 \K 1/1024
   \M 1         \G 1024
   \T 1048576})

(defn- to-bytes
  "converts from other units to bytes"
  [[n unit]]
  (let [[mult _ _] (name unit)]
    (* n (*unit-mult-factors* mult))))

(defn- extract-size 
  "extracts the file size"
  [h] (let [text (first (:content (first h)))
            f (juxt extract-size-number extract-size-unit)]
        (f (second (re-split #"," text)))))

(defn- extract-health-indicator 
  "extracts either the number of seeds or leeches"
  [h] (let [text (first (:content h))]
        (when (re-find #"\d" text)
          (Integer/parseInt text))))

(defn- extract 
  "extracts the data from the given result row"
  [result]
  (let [extr (partial html/select [result])
        nme (extract-name 
              (extr *name-selector*))
        sze (to-bytes (extract-size
              (extr *size-selector*)))
        [se le] (map extract-health-indicator
                   (extr *health-selectors*))
        trusted (boolean (first (extr *trusted-selector*)))
        vip     (boolean (first (extr *vip-selector*)))
        url     (:href (:attrs (first (extr *download-link-selector*))))
        [mjr mnr] (map (comp first :content) (extr *category-selector*))]
    {:name nme :size sze :seeds se :leeches le :trusted trusted :vip vip :category mjr :sub-category mnr :url url}))

(defn trusted?
  "returns true if the given torrent is from a trusted or vip member"
  [t] (or (:vip t) (:trusted t)))

(defn search 
  "Performs a pirate bay search for the given query and keyword/value option 
  pairs. Options are as follows:

  :category    The content type to search for. Either a single value or a 
               vector. The categories are :all, :audio, :video, :apps, 
               :games and :other. (default :all)
  
  :filter      Takes a predicate that is used to filter the results.

  :results     Takes a number that is used as a limit to the number of results.
               One http round-trip is required for each set of 30 results.
               (default 30).

  :only-page   Only fetches the given page of results, starting at 0.
  "
  [query & {limit :results filt :filter srt :sort-by :as opts}]
  (let [filt (or filt identity)
        srt (or srt (comp - :seeds))
        limit (or limit 30)
        pages (range 0 (ceil (/ limit 30.0)))
        result-data (apply concat (map #(search-data query (assoc opts :only-page %)) pages))
        results (map extract (take limit result-data))]
      (filter filt results)))

(def *torrent-dir* (str (System/getProperty "user.home") "/Downloads"))

(defn get-movie 
  "Searches pirate bay for \"title\" and fetches the best available torrent 
  (sorting first by (or :trusted :vip) then by number of seeds descending.
  The torrent file is saved in *torrent-dir* (which defaults to ~/Downloads)"
  [title]
  (when-let [results (search title :category :video)]
    (let [result (or (first (filter trusted? results)
                     (first results)))
          url (:url result)
          file-name (last (re-split #"\/" url))
          path    (str *torrent-dir* "/" file-name)]
      (copy (input-stream url) (file path))
      path)))

(defn get-episode 
  "Searches for \"Series sXXeYY\" and fetches the best available torrent. Torrents 
  are sorted according to (or :trusted :vip) then number of seeds descending.
  The torrent file is saved to *torrent-dir* (which defaults to ~/Downloads)"
  [series season episode]
  (let [query-string (str series " " (format "s%02de%2d" season episode))]
    (get-movie query-string)))
