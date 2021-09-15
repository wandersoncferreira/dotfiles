{:user {:plugins [[mx.cider/enrich-classpath "1.4.1"]
                  [lein-collisions "0.1.4"]]
        :jvm-opts [;; disable ui features - personal security
                   "-Djava.awt.headless=true"
                   ]}
 :nvd {:dependencies [[lein-nvd "1.4.1"]]}

 :rebel {:dependencies [[com.bhauman/rebel-readline "0.1.4"]]}

 :reveal {:dependencies [vlaaad/reveal "1.2.186"]
          :repl-options {:nrepl-middleware [vlaaad.reveal.nrepl/middleware]}}

 :repl {:middleware [cider.enrich-classpath/middleware]
        :plugins [[mx.cider/enrich-classpath "1.4.1"]]
        :jvm-opts ["-Dcider.enrich-classpath.throw=true"]}

 :portal {:dependencies [[djblue/portal "0.6.1"]]}}
