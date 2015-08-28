(defproject cljds/ch3 "0.1.0"
  :description "Example code for the book Clojure for Data Science"
  :url "https://github.com/clojuredatascience/ch3-correlation"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [incanter/incanter "1.5.6"]
                 [clj-time "0.8.0"]]
  :resource-paths ["data"]
  :aot [cljds.ch3.core]
  :main cljds.ch3.core
  :repl-options {:init-ns cljds.ch3.examples}
  :profiles {:dev {:dependencies [[org.clojure/tools.cli "0.3.1"]]}})
