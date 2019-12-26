(ns rplay.shaders
  (:require
   [thi.ng.geom.matrix :as mat :refer [M44]]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.glsl.vertex :as vertex]))

(def lidar-shader-spec
  {:vs
   (glsl/assemble
    (glsl/glsl-spec
     []
     "uniform mat4 matrix;
	 uniform vec3 customColor;

	 attribute float size;
   attribute float range;
   attribute float angle;

	 varying vec3 vColor;
	 void main() {
       float pos_x = cos(angle) * range;
       float pos_y = sin(angle) * range;
       vec4 pos = matrix * vec4(vec3(pos_x, pos_y, 0.0), 1.0);
	     vColor = customColor;
	     vec4 mvPosition = modelViewMatrix * pos;
	     gl_PointSize = size * ( 300.0 / -mvPosition.z );
	     gl_Position = projectionMatrix * mvPosition;
	 }"))

   :fs
   "void main(){
      vec3 L = normalize(vLightPos - vEyePos);
      vec3 N = normalize(vNormal);
      vec3 col = col0 * clamp(dot(N, L), 0.0, 1.0) + col1 * (1.0 - abs(dot(N, L))) + col2 * clamp(dot(-N,L), 0.0, 1.0);
      gl_FragColor=vec4(col, 1.0);
  }"
   :uniforms {:view      :mat4
              :proj      :mat4
              :model     [:mat4 M44]
              :normalMat [:mat4 (gl/auto-normal-matrix :model :view)]
              :col0      [:vec3 [0.3 0.3 0.3]]
              :col1      [:vec3 [1 1 1]]
              :col2      [:vec3 [0 0 0]]
              :lightPos  [:vec3 [0 1 2]]}
   :attribs {:position [:vec3 0]
             :normal   [:vec3 1]}
   :varying {:vNormal   :vec3
             :vEyePos   :vec3
             :vLightPos :vec3}
   :state    {:depth-test true}})
