(use java.util.jar.JarFile)
(def jarfile (JarFile. "/Users/main/src/applet-test/lib/SwingSet2.jar"))
(into #{}
      (.entrySet (.getMainAttributes (.getManifest jarfile))))

(use 'clojure.contrib.pprint)
(pprint (map (fn [x]
               [(.getKey x) (.getValue x)])
             (into #{} (.entrySet (.getMainAttributes (.getManifest jarfile))))))