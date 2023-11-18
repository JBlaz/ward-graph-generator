(ns ward-graph-generator.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [clj-time.coerce :as c]
   [clj-time.format :as f]
   ))

(defn all-in-one
  [data org-map]
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
                (str "`" v "` -- `" k "`"))
              org-map)])))

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
    ;; (prn (json/generate-string combined-data))
    ;; (prn combined-data)
    (str "<html><body><h1>Callings (11/18/23)</h1><pre class=\"mermaid\">\n"
         (str/join
          "\n</pre><pre class=\"mermaid\">\n"
          (map (fn [[org callings]]
                 (str/join "\n"
                           (flatten ["classDiagram"
                                     (str "class `" org "`")
                                     (:strings (reduce (fn [{:keys [connections strings]} {:keys [position memberName setApartDate]}]
                                                         (let [connection (str "`" org "` -- `" position "`")]
                                                           {:strings
                                                            (apply conj strings [(if (seq memberName)
                                                                                   [(str "class `" position "`{")
                                                                                    [(str "  Name -- " memberName)
                                                                                     (str "  SetApart -- " (if (= 8 (count setApartDate)) (str/join "/" [(subs setApartDate 6 8) (subs setApartDate 4 6) (subs setApartDate 0 4)]) "Nope"))]
                                                                                    "}"]
                                                                                   (str "class `" position "`")
                                                                                   )
                                                                                 (when-not (connections connection) (str "`" org "` -- `" position "`"))])
                                                            :connections (conj connections connection)})) {:connections #{} :strings []} callings))]))) combined-data))
         "\n</pre><script type=\"module\">\nimport mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';\nmermaid.initialize({ startOnLoad: true });\n</script></body></html>")))

(def json-data (read-json-file "ward.json"))

(defn -main
  [& args]
  ;; (generate-mermaid-code json-data)
  (spit "index.html" (generate-mermaid-code json-data))
  )