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
         (conj acc callings)
         acc)
       (if (seq children)
         (conj rest-map (:children first-map))
         rest-map)))))


(defn read-json-file [file-path]
  (with-open [reader (io/reader (io/resource file-path))]
    (json/parse-string (slurp reader) true)))

(defn generate-mermaid-code [data]
  (prn (->> data
            (flatten-children [])
            ;; (map #(select-keys % [:children
            ;;                       :name
            ;;                       :parentName
            ;;                       :callings
            ;;                       :defaultOrgName
            ;;                       :members
            ;;                       :firstOrgTypeName
            ;;                       :classes
            ;;                       :customOrgName]))
            ;; (map #(merge % {:callings (map (fn [calling] (select-keys calling [:memberName
            ;;                                                                    :organization
            ;;                                                                    :setApartDate
            ;;                                                                    :position]))
            ;;                                (:callings %))}))
            ;; (map #(merge % {:children (map (fn [calling] (select-keys calling [:memberName
            ;;                                                                    :organization
            ;;                                                                    :setApartDate
            ;;                                                                    :position]))
            ;;                                (:children %))}))
            ;; second
            ))
  (str/join "\n"
            (flatten ["classDiagram"
                        ;; (map #(str "  " % "[\"" (-> % str/trim (str/replace "_" " ")) "\"];") org-nodes)
                        ;; (map #(str "  " % "[\"" (-> % str/trim (str/replace "_" " ")) "\"];") calling-nodes)
                      (map (fn [thing] (let [org-name (str/replace (:name thing) #" " "_")
                                             callings (:callings thing)]
                                         [(str "class `" org-name "`")
                                          (map (fn [{:keys [position memberName setApartDate]
                                                     :as   calling}]
                                                 (if (seq memberName)
                                                   [(str "class `" position "`{")
                                                    (str "  Name -- " memberName)
                                                    (str "  SetApart -- " (if (= 8 (count setApartDate)) (str/join "/" [(subs setApartDate 6 8) (subs setApartDate 4 6) (subs setApartDate 0 4)]) "?")
                                                         "}")
                                                    (str org-name " -- `" position "`")]
                                                   [])) callings)]
                                              ;; (if (seq (:callings thing))
                                              ;;   (map #(str "  " org-name " --> " (str/replace (:position %) #" " "_") ";") (:callings thing))
                                              ;;   [])
                                         ))
                           data)])))

(def json-data (read-json-file "ward.json"))

(defn -main
  [& args]
  (generate-mermaid-code json-data)
  ;; (println (generate-mermaid-code json-data))
  )

