{
 :nvd {:dependencies [[lein-nvd "1.4.1"]]}

 :reveal {:dependencies [[vlaaad/reveal "1.2.186"]]
          :repl-options {:nrepl-middleware [vlaaad.reveal.nrepl/middleware]}
          :jvm-opts ["-Dvlaaad.reveal.prefs={:font-family \"Monaco\" :theme :light :font-size 18}"]}

 :enrich {:middleware [cider.enrich-classpath/middleware]
          :dependencies [[fipp "0.6.24"]]
          :plugins [[mx.cider/enrich-classpath "1.4.1"]]
          :jvm-opts ["-Dcider.enrich-classpath.throw=true"]}
 }
