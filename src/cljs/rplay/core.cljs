(ns rplay.core
  (:require-macros
   [thi.ng.math.macros :as mm])
  (:require
   [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]
   [thi.ng.color.core :as col]
   [thi.ng.typedarrays.core :as arrays]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.webgl.constants :as glc]
   [thi.ng.geom.gl.webgl.animator :as anim]
   [thi.ng.geom.gl.buffers :as buf]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.geom.gl.utils :as glu]
   [thi.ng.geom.gl.glmesh :as glm]
   [thi.ng.geom.gl.camera :as cam]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.gl.arcball :as arc]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :as mat :refer [M44]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.geom.gl.shaders.basic :as basic]
   [thi.ng.geom.quaternion :as q]
   [thi.ng.geom.utils :as gu]
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [goog.events.KeyCodes :as KeyCodes :refer [ALT]]
   [reagent.core :as r]))

(defn look-at
  "Returns a matrix that puts the camera at the eye position looking
  toward the target point with the given up direction."
  [eye target up]
  (let [dir (m/- eye target)]
    (if (m/delta= v/V3 dir)
      M44
      (let [[zx zy zz :as z] (m/normalize dir)
            [xx xy xz :as x] (gu/ortho-normal up z)
            [yx yy yz :as y] (gu/ortho-normal z x)]
        (mat/Matrix44.
         xx yx zx 0.0
         xy yy zy 0.0
         xz yz zz 0.0
         (- (m/dot x eye)) (- (m/dot y eye)) (- (m/dot z eye)) 1.0
         nil nil)))))

(defn update-view
  [{:keys [curr-rot] :as ab}]
  (let [q      (q/quat (:xyz curr-rot) (- (:w curr-rot)))
        offset (g/transform-vector q (vec3 0 0 (get ab :dist)))
        up     (g/transform-vector q v/V3Y)
        eye    (m/- offset)
        m (look-at eye (vec3) up)
        x (:view ab)]
    (assoc ab :view (g/scale (g/as-matrix curr-rot) (:dist ab)))))

(defn- sphere-position
  [{:keys [center radius]} x y]
  (let [v (vec3 (mm/subdiv x (v/x center) radius) (mm/subdiv y (v/y center) radius) 0)
        m (m/mag-squared v)]
    (if (> m 1.0)
      (m/normalize v)
      (assoc v :z (Math/sqrt (- 1.0 m))))))

(defn zoom-delta
  [{:keys [min-dist max-dist] :as ab} delta]
  (-> ab
      (assoc :dist
             (m/clamp
              (let [d (mm/madd delta (mm/subm max-dist min-dist 1e-3) (get ab :dist))]
                d)
              min-dist max-dist))
      update-view))

(defn down
  [ab x y]
  (-> ab
      (assoc :click-pos (sphere-position ab x y)
             :click-rot (:curr-rot ab))
      update-view))

(defn drag
  [{:keys [click-pos] :as ab} x y]
  (when click-pos
    (let [drag-pos (sphere-position ab x y)
          axis     (m/cross click-pos drag-pos)
          theta    (m/dot click-pos drag-pos)
          drag-rot (q/quat axis theta)]
      (-> ab
          (assoc :curr-rot (m/* drag-rot (:click-rot ab)))
          update-view))))

(defn translate-x [ab delta]
  (update ab :view g/translate (vec3 delta 0 0)))

(defn translate-y [ab delta]
  (update ab :view g/translate (vec3 0 delta 0)))

(def shader-spec
  {:vs (glsl/minified
        "void main() {
           vUV = uv;
           gl_Position = proj * view * model * vec4(position, 1.0);
         }")
   :fs (glsl/minified
        "void main() {
           gl_FragColor = texture2D(tex1, vUV);
         }")
   :uniforms {:model    [:mat4 M44]
              :view     :mat4
              :proj     :mat4
              :tex1     [:sampler2D 0] ;; bound to tex unit #0
              :fade     :float}
   :attribs  {:position :vec3
              :uv       :vec2}
   :varying  {:vUV      :vec2}
   :state    {:depth-test true}})

#_(def gl (gl/gl-context "main"))
(def state (atom {}))

(defn key-dispatch [state e] (.-keyCode e))
(defmulti key-handler key-dispatch)
(defmethod key-handler :default [state e] nil)

(defmethod key-handler KeyCodes/ALT [state e] (println "ALT"))

(defmethod key-handler KeyCodes/SHIFT [state e] (println "SHIFT"))

(defmethod key-handler KeyCodes/LEFT [state e]
  (swap! state update-in [:uniforms :view] g/translate (vec3 -0.1 0 0)))

(defmethod key-handler KeyCodes/RIGHT [state e]
  (swap! state update-in [:uniforms :view] g/translate (vec3 0.1 0 0)))

(defmethod key-handler KeyCodes/UP [state e]
  (let [[x y z] (:xyz (-> @state :cam :curr-rot))]
    (println x y z)
    (swap! state update-in [:uniforms :view] g/translate (vec3 0 z y))))

(defmethod key-handler KeyCodes/DOWN [state e]
  (let [[x y z] (:xyz (-> @state :cam :curr-rot))]
    (swap! state update-in [:uniforms :view] g/translate (vec3 0 (- z) (- y)))))

(defn init-arcball
  [el vrect]
  (gevents/removeAll el)
  (doto el
    (gevents/listen gevents/EventType.KEYDOWN
                    (fn [e]
                      (key-handler state e)))
    (.addEventListener gevents/EventType.WHEEL
                       (fn [e]
                         (swap! state update :cam zoom-delta (/ (.-deltaY e) 40))))
    (gevents/listen gevents/EventType.MOUSEDOWN
                    (fn [e]
                      (doto state
                        (swap! assoc :mouse-down true)
                        (swap! update :cam down (.-clientX e) (.-clientY e)))))
    (gevents/listen gevents/EventType.MOUSEUP
                    (fn [e] (swap! state assoc :mouse-down false)))
    (gevents/listen gevents/EventType.MOUSEMOVE
                    (fn [e]
                      (when (:mouse-down @state)
                        (swap! state update :cam drag (.-clientX e) (.-clientY e)))))))


(defn demo
  []
  (let [gl        (gl/gl-context "main")
        view-rect (gl/get-viewport-rect gl)
        tex-ready (atom false)
        tex1      (buf/load-texture
                   gl {:callback (fn [tex img]
                                   (reset! tex-ready true))
                       :src      "assets/lancellotti.jpg"
                       :flip     false})

        model    (-> (pl/plane-with-point (vec3 0 1 0) v/V3Z)
                     (g/as-mesh {:mesh (glm/indexed-gl-mesh 4 #{:uv})
                                 :attribs {:uv (attr/face-attribs (attr/uv-cube-map-v 256 false))}})
                     (gl/as-gl-buffer-spec {})
                     (cam/apply (cam/perspective-camera {:aspect view-rect}))
                     (assoc :shader (sh/make-shader-from-spec gl shader-spec)
                            :cam (-> (arc/arcball {:init (m/normalize (q/quat 0.0 1.0 1.0 0))
                                                   :dist 2.0
                                                   :max-dist (* 20 2.0)
                                                   :min-dist (/ 2.0 30)}) 
                                     (arc/resize (g/width view-rect) (g/height view-rect))))
                     (gl/make-buffers-in-spec gl glc/static-draw))]
    (reset! state model)
    (gl/set-viewport gl view-rect)
    (init-arcball (.getElementById js/document "main") view-rect)
    (anim/animate
     (fn [t frame]
       (when @tex-ready
         (gl/bind tex1 0)
         (doto gl
           (gl/clear-color-and-depth-buffer col/WHITE 1)
           (gl/enable glc/depth-test)
           (gl/draw-with-shader
            (swap! state
                   #(-> %
                        #_(update-in [:uniforms :view] g/rotate-x 0.01)
                        #_(update-in [:uniforms :view] g/rotate-z 0.01))))))
       true))))


#_(def _ (swap! state update-in [:uniforms :view] g/rotate-around-axis [0 0 1] (/ Math/PI 2)))

(demo)
