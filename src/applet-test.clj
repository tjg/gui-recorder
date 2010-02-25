(ns applet-test
  (:import (java.awt Graphics Color Font RenderingHints Frame GridLayout)
           (javax.swing SwingUtilities JApplet JButton)
           SwingApplet SwingSet2Applet)
  (:use clojure.contrib.swing-utils)
  (:use clojure.contrib.pprint))

(comment "
Notes: 

JOptionPane.showMessageDialog(..) pops something up which can't be found in the normal
applet hierarchy. perhaps it's within a separate root node stored somewhere in swing.

Components are in a stable order within a container.
http://java.sun.com/javase/7/docs/api/java/awt/Container.html#add%28java.awt.Component,%20int%29

")

(defonce frame  (Frame. "SwingSet2 applet"))
(defonce applet (SwingApplet.))
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

(pop-up-applet)

(def gc (memfn getComponents))

(defstruct gui-node :content :ancestors :position)
(defn make-gui-node [content ancestors position]
  (let [x (struct gui-node content ancestors position)]
    (with-meta x
      (merge (meta x) {:type ::gui-node}))))
(defmethod print-method ::gui-node [x writer]
  (let [{:keys [content ancestors position]} x]
    (cl-format writer "#<~S ~S>" (class content) position)))

(def natural-numbers (iterate inc 0))

(defn hierarchy [gui-obj]
  (let [node (make-gui-node gui-obj [] 0)]
    (conj (hierarchy-helper node) node)))

(defn hierarchy-helper [gui-node]
  (lazy-seq
   (let [{:keys [content ancestors position]} gui-node]
     (if (not (isa? (class content) java.awt.Container))
       []
       (let [children (vec (.getComponents content))
             children (map (fn [c counter]
                             (make-gui-node c (conj ancestors gui-node) counter))
                           children natural-numbers)]
         (conj children
               (map hierarchy-helper children)))))))

(defn buttons []
  (for [component (flatten (blah applet))
        :when (isa? (class component) javax.swing.JButton)]
    component))

;; javax.swing.text.JTextComponent javax.swing.JScrollBar


(defn top-of-gui-chain [gui-obj]
  (let [parent (.getParent gui-obj)]
    (if parent
      (top-of-gui-chain parent)
      gui-obj)))

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
                              (flatten (blah (top-of-gui-chain applet))))]
    (map (fn [x] (clojure.set/difference x dont-care))
         graphics-objects)))




(def jbutton (first (mapcat gc (mapcat gc (mapcat gc (.getComponents applet))))))

(defonce listener
  (add-action-listener
   jbutton
   (fn [event]
     (cl-format true "Button text: ~S~%" (.getText jbutton)))))

(defn remove-listener []
  (.removeActionListener jbutton listener))