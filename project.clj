(defproject real-wsdm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version  "2.0.0"
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.5"]
                 [org.lemurproject.galago/core "3.7-SNAPSHOT"]]
  :main ^:skip-aot real-wsdm.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
