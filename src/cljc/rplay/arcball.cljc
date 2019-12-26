(ns rplay.arcball
  #?(:cljs
     (:require-macros
      [thi.ng.math.macros :as mm]))
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3 V3Y]]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.quaternion :as q]
   #?(:clj [thi.ng.math.macros :as mm])))

(declare update-view)

;; Based on "ARCBALL: A User Interface for Specifying
;; Three-Dimensional Orientation Using a Mouse" by Ken Shoemake
;;
;; http://www.talisman.org/~erlkonig/misc/shoemake92-arcball.pdf

(defn arcball
  [{:keys [init dist min-dist max-dist radius center] :as ab}]
  (let [dist     (or dist 2.0)
        min-dist (or min-dist (/ dist 2.0))
        max-dist (or max-dist (* dist 2.0))
        curr-rot (if init (q/quat init) (q/quat-from-axis-angle V3Y m/PI))]
    (-> ab
        (merge
         {:dist      dist
          :min-dist  min-dist
          :max-dist  max-dist
          :radius    (or radius 300.0)
          :center    (or center (vec2 640 360))
          :curr-rot  curr-rot
          :click-rot curr-rot})
        update-view)))

;; Helpers

(defn- sphere-position
  [{:keys [center radius]} x y]
  (let [v (vec3 (mm/subdiv x (v/x center) radius) (mm/subdiv y (v/y center) radius) 0)
        m (m/mag-squared v)]
    (if (> m 1.0)
      (m/normalize v)
      (assoc v :z (Math/sqrt (- 1.0 m))))))

;; Event handlers & arcball operations

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

(defn up
  [ab] (assoc ab :click-pos nil))

(defn resize
  [ab w h]
  (let [ww (/ w 2)
        wh (/ h 2)]
    (assoc ab
           :radius (* (min ww wh) 2)
           :center (vec2 ww wh))))

(defn zoom-delta
  [{:keys [min-dist max-dist] :as ab} delta]
  (-> ab
      (assoc :dist
             (m/clamp
              (mm/madd delta (mm/subm max-dist min-dist 1e-3) (get ab :dist))
              min-dist max-dist))
      update-view))

(defn zoom-abs
  [ab x] (-> ab (assoc :dist (m/clamp x (:min-dist ab) (:max-dist ab))) update-view))

(defn update-view
  [{:keys [curr-rot] :as ab}]
  (assoc ab :view (g/as-matrix (q/quat (:xyz curr-rot) (- (:w curr-rot)))))
#_(let [q      (q/quat (:xyz curr-rot) (- (:w curr-rot)))
          offset (g/transform-vector q (vec3 0 0 (get ab :dist)))
          up     (g/transform-vector q V3Y)
          eye    (m/- offset)]
      (assoc ab :view (mat/look-at eye (vec3) up))))

(defn get-view
  [ab] (or (get ab :view) (get (update-view ab) :view)))

(defn get-rotation
  [ab] (get ab :curr-rot))

(defn set-rotation
  [ab q] (-> ab (assoc :curr-rot q) update-view))

(defn set-zoom-range
  [ab min max] (assoc ab :min-dist min :max-dist max :dist (m/clamp (get ab :dist) min max)))
