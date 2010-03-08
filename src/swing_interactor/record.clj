(ns swing-interactor.record
  (:import (java.awt Graphics Color Font RenderingHints Frame GridLayout)
           (java.awt.event AWTEventListener WindowEvent)
           (javax.swing SwingUtilities JApplet JButton)
           SwingApplet SwingSet2Applet)
  (:use clojure.set)
  (:use clojure.contrib.swing-utils)
  (:use clojure.contrib.pprint)
  (:use clojure.contrib.seq-utils)
  (:use swing-interactor.start-applet)
  (:use swing-interactor.hierarchy))





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
