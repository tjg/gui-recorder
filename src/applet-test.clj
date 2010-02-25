(ns applet-test
  (:import (java.awt Graphics Color Font RenderingHints Frame GridLayout)
           (javax.swing SwingUtilities JApplet JButton)
           SwingApplet SwingSet2Applet)
  (:use clojure.contrib.swing-utils)
  (:use clojure.contrib.pprint)
  (:use clojure.contrib.seq-utils))

(comment "
Notes: 

JOptionPane.showMessageDialog(..) pops something up which can't be found in the normal
applet hierarchy. perhaps it's within a separate root node stored somewhere in swing.

Components are in a stable order within a container.
http://java.sun.com/javase/7/docs/api/java/awt/Container.html#add%28java.awt.Component,%20int%29

")

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

(def get-components (comp vec (memfn getComponents)))

(defstruct gui-node :content :ancestors :position)
(defn make-gui-node [content ancestors position]
  (let [x (struct gui-node content ancestors position)]
    (with-meta x
      (merge (meta x) {:type ::gui-node}))))
(defmethod print-method ::gui-node [x writer]
  (let [{:keys [content ancestors position]} x]
    (cl-format writer "#<~S ~S>" (class content) position)))

(def natural-numbers (iterate inc 0))

(defn top-of-gui-chain [gui-obj]
  (let [parent (.getParent gui-obj)]
    (if parent
      (top-of-gui-chain parent)
      gui-obj)))

(defn hierarchy-helper [gui-node]
  (let [{:keys [content ancestors position]} gui-node]
    (if (not (isa? (class content) java.awt.Container))
      []
      (let [children (get-components content)
            children (map (fn [c counter]
                            (make-gui-node c (conj ancestors gui-node) counter))
                          children natural-numbers)]
        (cons gui-node
              (map hierarchy-helper children))))))

(defn hierarchy [gui-obj]
  (let [gui-obj (top-of-gui-chain gui-obj)
        node (make-gui-node gui-obj [] 0)]
    (hierarchy-helper node)))

(defn buttons []
  (for [component (flatten (hierarchy applet))
        :when (isa? (class component) javax.swing.JButton)]
    component))

;; javax.swing.text.JTextComponent javax.swing.JScrollBar



(def dont-care
     #{;; first pass
       javax.swing.JComponent java.awt.image.ImageObserver
       java.awt.Container 
       java.awt.Component javax.accessibility.Accessible
       javax.swing.Scrollable java.lang.Object java.awt.MenuContainer
       java.io.Serializable
       ;; second pass
       javax.swing.SwingConstants javax.swing.plaf.UIResource
       javax.swing.AbstractButton java.awt.ItemSelectable
       java.awt.event.ContainerListener java.util.EventListener
       javax.swing.ScrollPaneConstants java.awt.Adjustable})

(defn interesting-objects []
  (let [graphics-objects (map (fn [x]
                                (conj (ancestors (class x)) (class x)))
                              (flatten (hierarchy (top-of-gui-chain applet))))]
    (map (fn [x] (clojure.set/difference x dont-care))
         graphics-objects)))




#_(def jbutton (first (mapcat get-components (mapcat get-components (mapcat get-components (.getComponents applet))))))

#_(defonce listener
  (add-action-listener
   jbutton
   (fn [event]
     (cl-format true "Button text: ~S~%" (.getText jbutton)))))

#_(defn remove-listener []
  (.removeActionListener jbutton listener))