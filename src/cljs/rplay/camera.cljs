(ns rplay.camera
  (:require
   [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]
   [thi.ng.geom.gl.arcball :as arc]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.quaternion :as q]
   [reagent.core :as r]))

(defn init-state! [camera-state vrect]
  (swap! camera-state assoc
         ::cam
         (-> (arc/arcball {:init (m/normalize (q/quat 0.0 0.707 0.707 0))})
             (arc/resize (g/width vrect) (g/height vrect)))))

(defn update-camera [cam f & args]
  (apply update cam ::cam f args))

(defn init-arcball!
  [el state vrect]
  (doto el
    (.addEventListener
     "wheel"
     (fn [e]
       (swap! state update-camera arc/zoom-delta (.-deltaY e))))
    (.addEventListener
     "mousedown"
     (fn [e]
       (doto state
         (swap! assoc :mouse-down true)
         (swap! update-camera arc/down (.-clientX e) (.-clientY e)))))
    (.addEventListener
     "mouseup"
     (fn [e] (swap! state assoc :mouse-down false)))
    (.addEventListener
     "mousemove"
     (fn [e]
       (when (:mouse-down @state)
         (swap! state update :cam arc/drag (.-clientX e) (.-clientY e)))))))

(defn arcball [{:keys [:webgl/gl-context ::camera-state]} props & children]
  (let [vrect (gl/get-viewport-rect gl-context)]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (init-state! camera-state vrect)
        (init-arcball! this camera-state vrect))
      :reagent-render
      (fn [_]
        [:div props children])})))

#_(comment
  [arcball {}
   [:div "Hello World"]]
  )
