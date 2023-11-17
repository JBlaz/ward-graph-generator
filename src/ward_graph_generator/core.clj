(ns ward-graph-generator.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cheshire.core :as json]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))




(defn read-json-file [file-path]
  (with-open [reader (io/reader (io/resource file-path))]
    (json/parse-string (slurp reader) true)))

(defn generate-mermaid-code [data]
  (let [org-nodes (map #(str/replace (:name %) #" " "_") data)
        calling-nodes (for [org data
                            calling (:callings org)]
                        (str/replace (:position calling) #" " "_"))]
    (str/join "\n"
              (flatten ["classDiagram"
                        ;; (map #(str "  " % "[\"" (-> % str/trim (str/replace "_" " ")) "\"];") org-nodes)
                        ;; (map #(str "  " % "[\"" (-> % str/trim (str/replace "_" " ")) "\"];") calling-nodes)
                        (map (fn [thing] (let [org-name (str/replace (:name thing) #" " "_")
                                                  callings (:callings thing)]
                                              (map (fn [{:keys [position memberName] :as calling}]
                                                     (if (seq memberName)
                                                       [ (str "class " (-> position
                                                                           (str/replace #" " "")
                                                                           (str/replace #"--" "-")) "{")
                                                        (str "  Name -- " memberName)
                                                        "}"]
                                                       [])
                                                     ) callings)
                                              ;; (if (seq (:callings thing))
                                              ;;   (map #(str "  " org-name " --> " (str/replace (:position %) #" " "_") ";") (:callings thing))
                                              ;;   [])
                                              ))
                                data)]))))

(def json-data (read-json-file "ward.json"))

(defn -main
  [& args]
  (println (generate-mermaid-code json-data)))

