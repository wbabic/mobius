(defproject mobius "0.1.0"
  :description "Project Mobius"
  :url "http://wbabic.github.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0-beta3"]
                 [org.clojure/clojurescript "0.0-3269"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/core.match "0.3.0-alpha4"
                  :exclusions [org.ow2.asm/asm-all
                               org.clojure/tools.analyzer
                               org.clojure/tools.analyzer.jvm]]
                 [org.omcljs/om "0.8.8"]]

  :plugins [[lein-cljsbuild "1.0.6"]
            [lein-figwheel "0.3.3"
             :exclusions [org.clojure/clojure
                          org.codehaus.plexus/plexus-utils]]]

  :source-paths ["src" "test"]

  :clean-targets ^{:protect false} ["resources/public/cljs" "target"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src" "test"]
              :figwheel { :on-jsload "mobius.core/on-js-reload" }
              :compiler {:main mobius.core
                         :asset-path "cljs/out"
                         :output-to "resources/public/cljs/main.js"
                         :output-dir "resources/public/cljs/out"}}
             {:id "min"
              :source-paths ["src"]
              :compiler {:output-to "resources/public/cljs/main.js"
                         :main mobius.core
                         :optimizations :advanced
                         :pretty-print false}}]}

  :figwheel
  {:css-dirs ["resources/public/css"]})
