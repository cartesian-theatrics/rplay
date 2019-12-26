(ns rplay.stl-mesh
  (:import
   [com.jogamp.opengl GL3 GLAutoDrawable]
   [com.jogamp.newt.event MouseEvent KeyEvent])
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.math.macros :as mm]
   [thi.ng.color.core :as col]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.quaternion :as q]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.geom.gl.core :as gl]
   [rplay.arcball :as arcball]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.geom.gl.shaders.phong :as phong]
   [thi.ng.geom.gl.buffers :as buf]
   [thi.ng.geom.gl.camera :as cam]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.gl.glmesh :as glm]
   [thi.ng.geom.matrix :as mat :refer [M44]]
   [thi.ng.geom.gl.jogl.core :as jogl]
   [thi.ng.geom.gl.jogl.constants :as glc]
   [thi.ng.glsl.core :as glsl]
   [clojure.java.io :as io]))

(def app (atom nil))

(comment

  (defn parse-mesh [g3bj-vertices]
    (loop [mesh (glm/gl-mesh (/ (count g3bj-vertices) 10) {:vnorm :col})
           [p0 p1 p2 n0 n1 n2 c0 c1 c2 c3 & verts] g3bj-vertices]
      (if (empty? verts)
        mesh
        (recur (g/add-face mesh [[p0 p1 p2]
                                 {:vnorm  [n0 n1 n2]
                                  :col [c0 c1 c2]}])
               verts))))

  (do
    (require
     '[thi.ng.geom.core :as g]
     '[thi.ng.geom.utils :as gu]
     '[thi.ng.geom.vector :as v :refer [vec3]]
     '[thi.ng.geom.basicmesh :as bm]
     '[thi.ng.geom.triangle :as t]
     '[thi.ng.dstruct.streams :as streams]
     '[thi.ng.strf.core :as f]
     '[clojure.string :as str])


    (def mesh (parse-mesh (map float vertices)))

    (count (g/boun mesh))

    (def bounding-sphere (g/bounding-sphere mesh))

    (let [{:keys [p r]} (g/bounding-sphere mesh)]
      (def p p)
      (def r r))

    (/ (count vertices) 10 2)

    (def vertices (-> obj :meshes first :vertices))

    (glm/gl-mesh (/ ) {:fnorm :col})
    (def attrs (parse-attrs vertices))

    (defn- str->int [str]
      (if (str/blank? str) nil (Integer/parseInt str)))

    (defn- str->double [str] (Double/parseDouble str))

    (defn- f-element [item]
      (zipmap [:v :vt :vn] (map str->int (str/split item #"/"))))

    (defn- vertex-data [keys arr]
      (zipmap keys (map str->double arr)))

    (defn- create [type arr]
      (case type
        :f  (map f-element arr)
        :v  (vertex-data [:x :y :z] arr)
        :vt (vertex-data [:u :v :w] arr)
        :vn (vertex-data [:x :y :z] arr)
        :vp (vertex-data [:u :v :w] arr)))

    (defn- line-has-data? [line]
      (not (or (str/blank? line) (str/starts-with? line "#"))))

    (defn- lines-with-data [filename]
      (filter line-has-data? (line-seq (io/reader filename))))

    (defn- add-item [m [first & remaining]]
      (let [type (keyword first)]
        (if (contains? #{:f :v :vt :vn :vp} type)
          (update m type conj (create type remaining))
          m)))

    (defn parse [filename]
      (->>
       (lines-with-data filename)
       (map #(str/split % #"\s+"))
       (reduce add-item {:v '[] :f '[] :vt '[] :vn '[] :vp '[]})))

    )


  (def obj (parse "assets/t7.obj"))
  (count (obj :f))


  (def in (io/input-stream "resources/public/assets/t7.stl"))
  (def stl (mio/read-stl (mio/wrapped-input-stream in)
                         #(glm/gl-mesh % #{:fname})))

  (g/bounding-sphere stl)

  (def input-stream (mio/wrapped-input-stream in))
  (def line (streams/read-utf8-line))
  (streams/read-utf8-line input-stream)
  (streams/read-uint32-le input-stream)

  )


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

(defn load-mesh
  "Loads STL mesh from given path and fits it into centered bounding box."
  [path bounds]
  (with-open [in (io/input-stream path)]
    (->> (mio/read-stl (mio/wrapped-input-stream in) #(glm/gl-mesh % #{:fnorm}))
         vector
         (gu/fit-all-into-bounds (g/center bounds))
         first)))

(defn init
  [^GLAutoDrawable drawable]
  (let [^GL3 gl   (.. drawable getGL getGL3)
        view-rect (gl/get-viewport-rect gl)
        mesh      (load-mesh "resources/public/assets/t7.stl" (a/aabb 2))
        {:keys [p r]} (g/bounding-sphere mesh)
        shader    (sh/make-shader-from-spec gl (assoc phong/shader-spec :version 330))
        tex       (buf/load-texture
                   gl
                   {:src      (java.io.File. "resources/public/assets/lancellotti.jpg")
                    :flip     false})
        plane (-> (pl/plane-with-point (vec3 0 1 0) v/V3Z)
                  (g/as-mesh {:mesh (glm/indexed-gl-mesh 4 #{:uv})
                              :attribs {:uv (attr/face-attribs (attr/uv-cube-map-v 256 false))}})
                  (gl/as-gl-buffer-spec {})
                  (cam/apply (cam/perspective-camera {:aspect view-rect}))
                  (assoc :shader (sh/make-shader-from-spec gl shader-spec)
                         :tex tex
                         :cam (-> (arcball/arcball {:init (m/normalize (q/quat 0.0 1.0 1.0 0))
                                                :dist 2.0
                                                :max-dist (* 20 2.0)
                                                :min-dist (/ 2.0 30)})
                                  (arcball/resize (g/width view-rect) (g/height view-rect))))
                  (gl/make-buffers-in-spec gl glc/static-draw)
                  (assoc-in [:uniforms :model]
                            (-> M44
                                (g/scale 3))))

        model     (-> mesh
                      (gl/as-gl-buffer-spec {})
                      (update :uniforms merge
                              {:lightPos [0 2 2]
                               :view (mat/look-at (v/vec3 0 0 1) (v/vec3) v/V3Y)
                               :model (->  M44
                                           (g/rotate-x (/ Math/PI -2))
                                           (g/scale (/ 1 r 2.5))
                                           (g/translate (m/invert p)))
                               :shininess 50
                               :wrap 1
                               :ambientCol [0.0 0.1 0.4 0.0]
                               :diffuseCol [0.1 0.5 0.6]
                               :specularCol [0.8 0.3 0.3]})
                      (assoc :shader shader)
                      (gl/make-buffers-in-spec gl glc/static-draw))]
    (swap! app assoc
           :model     model
           :plane     plane
           :wireframe true
           :arcball   (arcball/arcball {:init (m/normalize (q/quat 0.0 0.707 0.707 0))}))))

(defn display
  [^GLAutoDrawable drawable t]
  (let [^GL3 gl (.. drawable getGL getGL3)
        {:keys [model plane wireframe arcball]} @app
        view    (arcball/get-view arcball)]
    (doto gl
      (gl/clear-color-and-depth-buffer col/GRAY 1)
      (.glPolygonMode glc/front-and-back (if wireframe glc/line glc/fill))
      (gl/draw-with-shader (update-in model [:uniforms :model] g/transform view))
      (.glPolygonMode glc/front-and-back glc/fill)
      (gl/draw-with-shader (update-in plane [:uniforms :model] g/transform view)))))

(defn resize
  [_ x y w h]
  (swap! app assoc-in [:model :uniforms :proj] (mat/perspective 45 (/ w h) 0.1 10))
  (swap! app update :arcball arcball/resize w h))

(defn dispose [_] (jogl/stop-animator (:anim @app)))

(defn key-pressed
  [^KeyEvent e]
  (condp = (.getKeyCode e)
    KeyEvent/VK_ESCAPE (jogl/destroy-window (:window @app))
    (case (.getKeyChar e)
      \w (swap! app update :wireframe not)
      nil)))

(defn mouse-pressed [^MouseEvent e] (swap! app update :arcball arcball/down (.getX e) (.getY e)))

(defn mouse-dragged [^MouseEvent e] (swap! app update :arcball arcball/drag (.getX e) (.getY e)))

(defn wheel-moved [^MouseEvent e deltas] (swap! app update :arcball arcball/zoom-delta (nth deltas 1)))

(defn -main
  [& args]
  (when-let [win @app]
    (jogl/destroy-window (:window win))
    (jogl/stop-animator (:anim win)))
  (reset!
   app
   (jogl/gl-window
    {:profile       :gl3
     :samples       4
     :double-buffer true
     :fullscreen    false
     :events        {:init    init
                     :display display
                     :dispose dispose
                     :resize  resize
                     :keys    {:press key-pressed}
                     :mouse   {:press mouse-pressed
                               :drag  mouse-dragged
                               :wheel wheel-moved}}}))
  nil)

#_(def _ (swap! app
              update-in
              [:model :uniforms :view]
              g/rotate-around-axis
              [0 0 1]
              (/ Math/PI 4)))

(defn quat-from-axis-angle
  [axis theta]
  (let [theta (/ theta 2.0)]
    (q/quat (m/normalize (v/vec3 axis) (Math/sin theta)) (Math/cos theta))))

#_(let [[x y z] [1 0 1]
      theta (* Math/PI 2.5)
      theta (Math/sin (/ theta 2))
      l (Math/sqrt (mm/madd x x y y z z))]
  (if (pos? l)
    (let [l (/ l theta)]
      (v/vec3 (/ x l) (/ y l) (/ z l)))))

(-main)
