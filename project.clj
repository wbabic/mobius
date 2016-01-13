(defproject mobius "0.1.2"
  :description "Project Mobius"
  :url "http://wbabic.github.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.clojure/tools.reader "1.0.0-alpha1"]
                 [org.clojure/core.async "0.2.374" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/core.match "0.3.0-alpha4"
                  :exclusions [org.ow2.asm/asm-all
                               org.clojure/tools.analyzer
                               org.clojure/tools.analyzer.jvm]]
                 [org.omcljs/om "0.9.0"
                  :exclusions [cljsjs/react]]
                 [org.clojure/test.check "0.8.2"]
                 [prismatic/schema "1.0.1"]
                 [complex/complex "0.1.9"]
                 [devcards "0.2.1-4" :exclusions [org.clojure/tools.reader]]
                 [ring/ring-core "1.4.0"]
                 [clj-time "0.9.0"]]

  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-figwheel "0.5.0-3"
             :exclusions [org.clojure/clojure
                          ring/ring-core
                          org.clojure/tools.reader]]]

  :source-paths ["src"]
  :clean-targets ^{:protect false} ["resources/public/cljs" "target"]

  :cljsbuild
  {
   :builds [{:id "devcards"
             :source-paths ["src"]
             :figwheel {:on-jsload "mobius.core/on-js-reload"
                        :devcards true}
             :compiler {:main mobius.devcards
                        :asset-path "cljs/out"
                        :output-to "resources/public/cljs/devcards.js"
                        :output-dir "resources/public/cljs/out"}}
            {:id "dev"
             :source-paths ["src"]
             :figwheel {:on-jsload "mobius.core/on-js-reload"}
             :compiler {:main mobius.core
                        :asset-path "cljs/out"
                        :output-to "resources/public/cljs/main.js"
                        :output-dir "resources/public/cljs/out"}}
            {:id "min"
             :source-paths ["src"]
             :compiler {:output-to "resources/public/cljs/main-min.js"
                        :main mobius.core
                        :optimizations :advanced
                        :pretty-print false}}]}

  :figwheel
  {:css-dirs ["resources/public/css"]})
