(ns applet-test
  (:import (java.awt Graphics Color Font RenderingHints Frame GridLayout)
           (java.awt.event AWTEventListener WindowEvent)
           (javax.swing SwingUtilities JApplet JButton)
           SwingApplet SwingSet2Applet)
  (:use clojure.set)
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

(defn my-pprint [object]
  (let [old-dispatch *print-pprint-dispatch*]
    (with-pprint-dispatch
      (fn [x]
        (if (:type (meta x))
          (pr x)
          (old-dispatch x)))
      (pprint object))))

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

(defn hierarchy-at [gui-obj]
  (let [node (make-gui-node gui-obj [] 0)]
    (hierarchy-helper node)))

(defn hierarchy [gui-obj]
  (hierarchy-at (top-of-gui-chain gui-obj)))

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


(def *instrumented* (agent #{}))

(defn instrument-buttons [gui-obj]
  (let [button-nodes (filter (fn [x]
                               (isa? (class (:content x)) javax.swing.JButton))
                             (flatten (hierarchy gui-obj)))
        button-nodes (set button-nodes)]
    (doseq [node button-nodes]
      (let [jbutton (:content node)]
        (try (add-action-listener
              jbutton
              (fn [event]
                (cl-format true "Button text: ~S~%" (.getText jbutton))))
             (finally (send *instrumented*
                            (fn [instrumented-nodes node]
                              (let [instrumented-buttons (set (map :content instrumented-nodes))
                                    button (:content node)]
                                (if (instrumented-buttons button)
                                  (conj instrumented-nodes node)
                                  instrumented-nodes)))
                            jbutton)))))))


#_(bit-or java.awt.AWTEvent/ACTION_EVENT_MASK
        java.awt.AWTEvent/ADJUSTMENT_EVENT_MASK
        java.awt.AWTEvent/COMPONENT_EVENT_MASK
        java.awt.AWTEvent/CONTAINER_EVENT_MASK
        java.awt.AWTEvent/FOCUS_EVENT_MASK
        java.awt.AWTEvent/HIERARCHY_BOUNDS_EVENT_MASK
        java.awt.AWTEvent/HIERARCHY_EVENT_MASK
        java.awt.AWTEvent/INPUT_METHOD_EVENT_MASK
        java.awt.AWTEvent/INVOCATION_EVENT_MASK
        java.awt.AWTEvent/ITEM_EVENT_MASK
        java.awt.AWTEvent/KEY_EVENT_MASK
        java.awt.AWTEvent/MOUSE_EVENT_MASK
        java.awt.AWTEvent/MOUSE_MOTION_EVENT_MASK
        java.awt.AWTEvent/MOUSE_WHEEL_EVENT_MASK
        #_java.awt.AWTEvent/PAINT_EVENT_MASK
        java.awt.AWTEvent/TEXT_EVENT_MASK
        java.awt.AWTEvent/WINDOW_EVENT_MASK
        java.awt.AWTEvent/WINDOW_FOCUS_EVENT_MASK
        java.awt.AWTEvent/WINDOW_STATE_EVENT_MASK)



(defn add-awt-event-listener
  "Adds an ActionLister to component. When the action fires, f will be
  invoked with the event as its first argument followed by args.
  Returns the listener."
  [toolkit event-mask f & args]
  (let [listener (proxy [AWTEventListener] []
                   (eventDispatched [event] (apply f event args)))]
    (.addAWTEventListener toolkit listener event-mask)
    listener))

(def *events* (agent []))

#_(.addAWTEventListener
   (.getToolkit (top-of-gui-chain applet))
   (proxy [AWTEventListener] []
     (eventDispatched [event] (apply
                               (fn [event] (send *events*
                                                 (fn [events ev]
                                                   (conj events ev))
                                                 event))
                               event
                               [])))
   (reduce bit-or
           [java.awt.AWTEvent/ACTION_EVENT_MASK
            java.awt.AWTEvent/ADJUSTMENT_EVENT_MASK
            java.awt.AWTEvent/COMPONENT_EVENT_MASK
            java.awt.AWTEvent/CONTAINER_EVENT_MASK
            java.awt.AWTEvent/FOCUS_EVENT_MASK
            java.awt.AWTEvent/HIERARCHY_BOUNDS_EVENT_MASK
            java.awt.AWTEvent/HIERARCHY_EVENT_MASK
            java.awt.AWTEvent/INPUT_METHOD_EVENT_MASK
            java.awt.AWTEvent/INVOCATION_EVENT_MASK
            java.awt.AWTEvent/ITEM_EVENT_MASK
            java.awt.AWTEvent/KEY_EVENT_MASK
            java.awt.AWTEvent/MOUSE_EVENT_MASK
            java.awt.AWTEvent/MOUSE_MOTION_EVENT_MASK
            java.awt.AWTEvent/MOUSE_WHEEL_EVENT_MASK
            java.awt.AWTEvent/PAINT_EVENT_MASK
            java.awt.AWTEvent/TEXT_EVENT_MASK
            java.awt.AWTEvent/WINDOW_EVENT_MASK
            java.awt.AWTEvent/WINDOW_FOCUS_EVENT_MASK
            java.awt.AWTEvent/WINDOW_STATE_EVENT_MASK]))

#_(map (fn [x]
         (.removeAWTEventListener (.getToolkit (top-of-gui-chain applet))
                                  x))
       (vec (.getAWTEventListeners
             (.getToolkit (top-of-gui-chain applet)))))

#_(nth (filter (fn [event]
                 (isa? (class event) java.awt.event.WindowEvent))
               @*events*)
       8)

#_(set (map class @*events*))

#_(pprint
 (reduce (fn [a x]
           (assoc a x (inc (get a x 0))))
         {}
         (map class @*events*)))

(def *events-backup* (agent []))

(defn remove-awt-listeners []
  (map (fn [x]
         (.removeAWTEventListener (.getToolkit (top-of-gui-chain applet))
                                  x))
       (vec (.getAWTEventListeners
             (.getToolkit (top-of-gui-chain applet))))))

(defn reinstall-awt-listener []
  ;; backup
  (send *events-backup*
        (fn [agent val]
          (cons agent val))
        @*events*)
  ;; remove old event listeners
  (remove-awt-listeners)
  ;; add event listener
  (.addAWTEventListener
   (.getToolkit (top-of-gui-chain applet))
   (proxy [AWTEventListener] []
     (eventDispatched [event] (apply
                               (fn [event] (send *events*
                                                 (fn [events ev]
                                                   (conj events ev))
                                                 event))
                               event
                               [])))
   (reduce bit-or
           [java.awt.AWTEvent/ACTION_EVENT_MASK
            java.awt.AWTEvent/ADJUSTMENT_EVENT_MASK
            java.awt.AWTEvent/COMPONENT_EVENT_MASK
            java.awt.AWTEvent/CONTAINER_EVENT_MASK
            java.awt.AWTEvent/FOCUS_EVENT_MASK
            java.awt.AWTEvent/HIERARCHY_BOUNDS_EVENT_MASK
            java.awt.AWTEvent/HIERARCHY_EVENT_MASK
            java.awt.AWTEvent/INPUT_METHOD_EVENT_MASK
            java.awt.AWTEvent/INVOCATION_EVENT_MASK
            java.awt.AWTEvent/ITEM_EVENT_MASK
            java.awt.AWTEvent/KEY_EVENT_MASK
            java.awt.AWTEvent/MOUSE_EVENT_MASK
            java.awt.AWTEvent/MOUSE_MOTION_EVENT_MASK
            java.awt.AWTEvent/MOUSE_WHEEL_EVENT_MASK
            java.awt.AWTEvent/PAINT_EVENT_MASK
            java.awt.AWTEvent/TEXT_EVENT_MASK
            java.awt.AWTEvent/WINDOW_EVENT_MASK
            java.awt.AWTEvent/WINDOW_FOCUS_EVENT_MASK
            java.awt.AWTEvent/WINDOW_STATE_EVENT_MASK])))


(defn all-events []
  @*events*)

(defn suspiciously-new-window [ev]
  )

(defn interesting-user-actions [])
