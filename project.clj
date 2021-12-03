(defproject aoc2021 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2021"
  :url "https://github.com/root42/aoc2021"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [flames "0.4.0"]]
  :main ^:skip-aot aoc2021.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :plugins [[cider/cider-nrepl "0.25.5"]]
  )
