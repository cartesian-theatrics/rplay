(ns rplay.core
  (:import
   [com.jogamp.opengl GL3 GLAutoDrawable]
   [com.jogamp.newt.event MouseEvent KeyEvent])
  (:require
   [thi.ng.math.macros :as mm]
   [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]
   [thi.ng.color.core :as col]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.jogl.constants :as glc]
;;   [thi.ng.geom.gl.webgl.animator :as anim]
   [thi.ng.geom.gl.buffers :as buf]
   [thi.ng.geom.gl.shaders :as sh]
   ;;[thi.ng.geom.gl.utils :as glu]
   [thi.ng.geom.gl.glmesh :as glm]
   [thi.ng.geom.gl.jogl.core :as jogl]
   [thi.ng.geom.gl.jogl.buffers :as native]
   [thi.ng.geom.gl.camera :as cam]
   [thi.ng.geom.core :as g]
   [rplay.arcball :as arc]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :as mat :refer [M44]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.geom.gl.shaders.basic :as basic]
   [thi.ng.geom.quaternion :as q]
   [thi.ng.geom.utils :as gu]))

(defonce state (atom {}))
(defonce app (atom nil))

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
   :state    {:depth-test true}
   :version 330})

(defn line [gl dir]
  (let [vertices (native/float-buffer
                  (into [0 0 0] dir))
        colors (native/float-buffer
                [1 0 0 1 0 0.6 0])]
    (gl/make-buffers-in-spec
     {:attribs {:position {:data vertices :size 3}
                :color {:data colors :size 3}}
      :shader (sh/make-shader-from-spec gl shader-spec)
      :vs (glsl/minified
           "void main() {
           uCol = color;
           gl_Position = proj * view * model * orientation * vec4(position, 1.0);
         }")
      :fs (glsl/minified
           "void main() {
           gl_FragColor = vec4(uCol, 0.1);
         }")
      :uniforms {:orientation M44}
      :varying {:uCol :vec3}
      :mode 3
      :version 330
      :num-vertices 6}
     gl
     glc/static-draw)))

(defn init
  [^GLAutoDrawable drawable]
  (let [^GL3 gl   (.. drawable getGL getGL3)
        view-rect (gl/get-viewport-rect gl)
        tex-ready (atom false)
        tex1      (buf/load-texture
                   gl {:src      (java.io.File. "resources/public/assets/lancellotti.jpg") 
                       :flip     false})
        plane (-> (pl/plane-with-point (vec3 0 1 0) v/V3Z)
                  (g/as-mesh {:mesh (glm/indexed-gl-mesh 4 #{:uv})
                              :attribs {:uv (attr/face-attribs (attr/uv-cube-map-v 256 false))}})
                  (gl/as-gl-buffer-spec {})
                  (cam/apply (cam/perspective-camera {:aspect view-rect}))
                  (assoc :shader (sh/make-shader-from-spec gl shader-spec)
                         :tex1 tex1
                         :cam (-> (arc/arcball {:init (m/normalize (q/quat 0.0 1.0 1.0 0))
                                                :dist 2.0
                                                :max-dist (* 20 2.0)
                                                :min-dist (/ 2.0 30)})
                                  (arc/resize (g/width view-rect) (g/height view-rect))))
                  (gl/make-buffers-in-spec gl glc/static-draw)
                  (assoc-in [:uniforms :model] M44))
        line (-> (line gl [0 1 2]))]
    (reset! state {:plane plane
                   :line line})))

(defn display
  [^GLAutoDrawable drawable t]
  (let [^GL3 gl (.. drawable getGL getGL3)
        {:keys [cam tex1 plane] :as model} (:plane @state)
        view    (arc/get-view cam)]
    (gl/bind tex1 0)
    (doto gl
      (gl/clear-color-and-depth-buffer col/WHITE 1)
      (gl/enable glc/depth-test)
      (gl/draw-with-shader
       (swap! state
              #(->  %
                  #_(assoc-in [:uniforms :model] view)
                   #_(update-in [:uniforms :view] g/rotate-z 0.01)))))
    true))

(defn dispose [] (jogl/stop-animator (:anim @app)))

(defn tr [m v]
  (swap! state update-in [:uniforms m] g/translate v)
  nil)

(defn rot [m axis theta]
  (swap! state update-in [:uniforms m] g/rotate-around-axis axis theta)
  nil)

(defn key-pressed [^KeyEvent e]
  (let [[x y z] (map #(Math/sin %) (:xyz (q/quat-from-matrix (-> @state :uniforms :model))))]
    (println x y z)
    (condp = (.getKeyCode e)
      KeyEvent/VK_UP (tr :view (vec3 0 z y))
      KeyEvent/VK_DOWN (tr :view (vec3 0 (- z) (- y)))
      KeyEvent/VK_RIGHT (tr :proj (vec3 0.02 0.0 0.0))
      KeyEvent/VK_LEFT (tr :proj (vec3 -0.02 0.0 0.0))
      (println "Testing"))))

(defn quat->x-rot [quat]
  (let [[q0 q1 q2 q3] quat]
    (Math/atan (/ (* 2 (mm/madd q0 q3 q1 q2))
                  (- 1 (* 2 (mm/madd q2 q2 q3 q3)))))))

(defn quat->y-rot [quat]
  (let [[q0 q1 q2 q3] quat]
    (Math/asin (* 2 q0 (mm/msub q0 q2 q3 q1)))))

(defn quat->z-rot [quat]
  (let [[q0 q1 q2 q3] quat]
    (Math/atan2 (* 2 (mm/madd q0 q1 q2 q3))
                (- 1 (* 2 (mm/madd q1 q1 q2 q2))))
    #_(Math/atan (/ (* 2 (mm/madd q0 q1 q2 q3))
                    (- 1 (* 2 (mm/madd q1 q1 q2 q2)))))))

(defn orbit-pan [model dx dy]
  (let [model (g/rotate-z model dx)
        q (q/quat-from-matrix model)
        z-rot (quat->z-rot q)
        x-rot (quat->x-rot q)
        y-rot (quat->y-rot q)]
    (g/rotate-around-axis model (m/normalize (vec3 1 (- z-rot) 0) 1.0) dy)))

(defn mouse-pressed [^MouseEvent e] (swap! state update :cam arc/down (.getX e) (.getY e)))

(defn mouse-dragged [^MouseEvent e] (swap! state update :cam arc/drag (.getX e) (.getY e)))

(defn wheel-moved [^MouseEvent e deltas] (swap! state update :cam arc/zoom-delta (nth deltas 1)))

(defn -main
  [& args]
  (when-let [win @app]
    (jogl/destroy-window (:window win))
    (jogl/stop-animator (:anim win)))
  (reset!
   app
   (jogl/gl-window
    {:profile       :gl3
     :always-on-top true
     :samples       4
     :double-buffer true
     :fullscreen    false
     :events        {:init    init
                     :display (fn [& args]
                                (try
                                  (apply display args)
                                  (catch Exception e
                                    (println e)
                                    (dispose))))
                     :keys {:press #(key-pressed %)}
                     :mouse   {:press #(mouse-pressed %)
                               :drag  #(mouse-dragged %)
                               ;;:wheel wheel-moved
                               }}}))
  nil)



(-main)
