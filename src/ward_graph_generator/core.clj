(ns ward-graph-generator.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [clj-time.coerce :as c]
   [clj-time.format :as f]
   ))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def top-level-keys
  [:children
   :name
   :parentName
   :callings
   :defaultOrgName
   :members
   :firstOrgTypeName
   :classes
   :customOrgName])

(defn flatten-children
  [acc map-vec]
  (if (empty? map-vec)
    acc
    (let [first-map (first map-vec)
          rest-map (rest map-vec)
          children (:children first-map)
          callings (:callings first-map)]
      (flatten-children
       (if (not-empty callings)
         (apply conj acc callings)
         acc)
       (if (seq children)
         (apply conj rest-map (:children first-map))
         rest-map)))))


(defn read-json-file [file-path]
  (with-open [reader (io/reader (io/resource file-path))]
    (json/parse-string (slurp reader) true)))

(defn generate-mermaid-code [data]
  (let [data    (->> data
                     (flatten-children [])
                     (map #(select-keys % [:organization
                                           :memberName
                                           :position
                                           :setApartDate
                                           :callings
                                           :defaultOrgName
                                           :members
                                           :firstOrgTypeName
                                           :classes
                                           :customOrgName])))
        org-map (reduce (fn [acc {:keys [organization position]}]
                          (if organization
                            (assoc acc position organization)
                            acc)) {} data)
        combined-data (reduce (fn [acc [position organization]]
                                (assoc acc organization (reduce (fn [acc n]
                                                     (if (= organization (:organization n))
                                                       (conj acc n)
                                                       acc)) [] data))) {} org-map)]
    (prn (json/generate-string combined-data))
    (str/join
     "\n"
     (flatten ["classDiagram"
               (map
                (fn [{:keys [position memberName setApartDate]}]
                  (if (seq memberName)
                    [(str "class `" position "`{")
                     [(str "  Name -- " memberName)
                      (str "  SetApart -- " (if (= 8 (count setApartDate)) (str/join "/" [(subs setApartDate 6 8) (subs setApartDate 4 6) (subs setApartDate 0 4)]) "Nope"))]
                     "}"]
                    (str "class `" position "`")
                      ;; []
                    ))
                data)
               (map
                (fn [[k v]]
                  (str "`"v "` -- `" k "`"))
                org-map)]))))

(def json-data (read-json-file "ward.json"))

(defn -main
  [& args]
  (generate-mermaid-code json-data)
  ;; (println (generate-mermaid-code json-data))
  )

