(ns swing-interactor.start-applet
  (:import (java.awt Graphics Color Font RenderingHints Frame GridLayout)
           (java.awt.event AWTEventListener WindowEvent)
           (javax.swing SwingUtilities JApplet JButton)
           SwingApplet SwingSet2Applet)
  (:use clojure.set)
  (:use clojure.contrib.swing-utils)
  (:use clojure.contrib.pprint)
  (:use clojure.contrib.seq-utils))


(defonce frame  (Frame. "SwingSet2 applet"))
#_(defonce applet (SwingApplet.))
(defonce applet (SwingSet2Applet.))

(defn pop-up-applet []
  (SwingUtilities/invokeAndWait
   (fn []
     (doto frame
       (.setVisible false)
       (.setLayout (GridLayout. 1 0))
       (.show)
       (.add applet))
     (doto applet
       (.init)
       (.start))
     (doto frame
       (.pack)
       (.setVisible true)))))

#_(pop-up-applet)
