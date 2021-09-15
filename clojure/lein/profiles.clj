{:user {:plugins [[mx.cider/enrich-classpath "1.4.1"]
                  [lein-collisions "0.1.4"]]
        :jvm-opts [;; disable ui features - personal security
                   "-Djava.awt.headless=true"
                   ]}
 :nvd {:dependencies [[lein-nvd "1.4.1"]]}
 :rebel {:dependencies [[com.bhauman/rebel-readline "0.1.4"]]}
 :repl {:middleware [cider.enrich-classpath/middleware]
        :plugins [[mx.cider/enrich-classpath "1.4.1"]]
        :jvm-opts ["-Dcider.enrich-classpath.throw=true"]}}
