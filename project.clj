(defproject advent "1.0.0"
  :description "Advent of Code solutions for multiple years (2017-2025)"
  :url "https://github.com/pete23/advent"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/core.logic "1.0.0"]
                 [org.clojure/data.finger-tree "0.0.3"]
                 [com.carrotsearch/hppc "0.9.0.RC2"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.3.1"]
                 [com.clojure-goes-fast/clj-async-profiler "1.5.1"]
                 [criterium "0.4.6"]
                 [dom-top "1.0.9"]
                 [net.mikera/core.matrix "LATEST"]
                 [net.mikera/vectorz-clj "LATEST"]]
  :java-source-paths ["java"]
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :repl-options {:init-ns advent.core})
