(ns steamprice.core
  (:use (clojure-mail core store message folder)
        (clj-time core format)))

(use '[clojure.pprint :only [print-table]])

(remove-ns 'steamprice.core)

(def date-regex #"Date Confirmed: +([A-Z][a-z]{2} [A-Z][a-z]{2} \d\d \d\d:\d\d:\d\d \d\d\d\d)")
(def price-regex #"\r\n(\w[^:]+): *(\d+\.\d{2}) *(EUR|\$)")
(def date-formatter (formatter "EEE MMM dd HH:mm:ss yyyy"))

(def auths [{:user "bjmadsen@gmail.com" :pass ""}
            {:user "bm@aeons.dk" :pass ""}])

(defn get-store [{:keys [user pass]}]
  (auth! user pass)
  (gen-store))

(defn get-steam-messages [store]
  (all-messages store "Steam purchase"))

(defn get-all-steam-messages [auths]
  (reduce concat (map #(-> %
                           get-store
                           get-steam-messages) auths)))

(defn msg-text [msg]
  (->> msg
       message-body
       first
       first
       last))

(defn confirmation-date [msg-text]
  (->> msg-text
       (re-find date-regex)
       last
       prn
       (parse date-formatter)))

(defn products [msg-text]
  (->> msg-text
       (re-seq price-regex)
       (map rest)
       (map #(zipmap [:product :price :currency] %))))

(defn usd-to-eur [usd]
  (->> usd
       read-string
       (* 0.77)
       (format "%.2f")))

(defn convert-product [{:keys [price currency] :as prod}]
  (if (= currency "$")
    (assoc prod :currency "EUR" :price (usd-to-eur price))
    prod))

(defn combine-purchase [prod date]
  (assoc prod :date date))

(defn extract-purchases [auths]
  (let [msgs (get-all-steam-messages auths)
        msgs-text (map msg-text msgs)
        dates (map confirmation-date msgs-text)
        products (flatten (map products msgs-text))
        converted-products (map convert-product products)
        purchases (map combine-purchase converted-products dates)]
    {dates products}))

(def p (extract-purchases auths))

(def msgs (get-all-steam-messages auths))
(def msgs-text (map msg-text msgs))
(def dates (map confirmation-date msgs-text))
(def prods (flatten (map products msgs-text)))
(def converted (map convert-product prods))
(def purchs (map combine-purchase converted dates))

(def asd(first (rest msgs-text)))

(confirmation-date asd)
(parse date-formatter "Thu Oct 07 10:34:36 2010")
(first purchs)

