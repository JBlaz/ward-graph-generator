(ns ward-graph-generator.text
  (:require
   [clojure.string :as str]))

(def prefix
  "<html>\n  <body>\n    <h1>Callings (11/18/23)</h1>\n    <pre class=\"mermaid\">\n")

(def postfix
  "\n</pre><script type=\"module\">\nimport mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';\nmermaid.initialize({ startOnLoad: true });\n</script></body></html>")

(defn create-class-name
  [name]
  (str "class `" name "`"))

(defn create-class
  [name]
  (str (create-class-name name) "{"))

(defn get-date
  [date]
  (if (= 8 (count date))
    (str/join "/" [(subs date 6 8) (subs date 4 6) (subs date 0 4)])
    "Nope"))