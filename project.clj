(defproject yyy-data "0.1.0-SNAPSHOT"
  :description "Data import stuff for youyesyet, q.v."
  :url "http://example.com/FIXME"
  :license {:name "GNU General Public License version 2"
            :url "https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html"}
  :dependencies [[adl-support "0.1.0-SNAPSHOT"]
                 [generateme/fastmath "1.0.1"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [lein-light-nrepl "0.0.4"]
                 [net.mikera/core.matrix "0.62.0"]]
   :plugins [[lein-gorilla "0.4.0"]]
   :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]}
  )
