(ns yyy-data.core
  (:require [adl-support.utils :refer :all]
            [fastmath.core :refer [radians degrees sin cos sqrt atan2]]
            [clojure.core.matrix :as mx]
            [clojure.data.json :as json]
            [clojure.string :as s]))

(declare Cartesian Point)

;;; Coordinate system conversion cribbed from https://www.movable-type.co.uk/scripts/latlong-os-gridref.html

(def ellipsoids
  "Ellipsoid parameters; major axis (a), minor axis (b), and flattening (f) for each ellipsoid."
  {
    :WGS84         { :a 6378137     :b 6356752.314245 :f (/ 1 298.257223563) }
    :Airy1830      { :a 6377563.396 :b 6356256.909    :f (/ 1 299.3249646)   }
    :AiryModified  { :a 6377340.189 :b 6356034.448    :f (/ 1 299.3249646)   }
    :Bessel1841    { :a 6377397.155 :b 6356078.962818 :f (/ 1 299.1528128)   }
    :Clarke1866    { :a 6378206.4   :b 6356583.8      :f (/ 1 294.978698214) }
    :Clarke1880IGN { :a 6378249.2   :b 6356515.0      :f (/ 1 293.466021294) }
    :GRS80         { :a 6378137     :b 6356752.314140 :f (/ 1 298.257222101) }
    :Intl1924      { :a 6378388     :b 6356911.946    :f (/ 1 297)           } ;; aka Hayford
    :WGS72         { :a 6378135     :b 6356750.5      :f (/ 1 298.26)        }
    })


(def datums
  "Datums; with associated ellipsoid, and Helmert transform parameters to convert from WGS 84 into
  given datum.

  Note that precision of various datums will vary, and WGS-84 (original) is not defined to be
  accurate to better than ±1 metre. No transformation should be assumed to be accurate to better
  than a meter; for many datums somewhat less.

  Yes, I know that the plural of datums is data..."
  {
    ;; transforms: t in metres, s in ppm, r in arcseconds                               tx            ty           tz           s          rx          ry          rz
    :ED50        { :key :ED50       :ellipsoid (ellipsoids :Intl1924)      :transform { :tx   89.5,   :ty   93.8   :tz  123.1   :s -1.2    :rx  0.0    :ry  0.0    :rz  0.156  }}
    :Irl1975     { :key :Irl1975    :ellipsoid (ellipsoids :AiryModified)  :transform { :tx -482.530  :ty  130.596 :tz -564.557 :s -8.150  :rx -1.042  :ry -0.214  :rz -0.631  }}
    :NAD27       { :key :NAD27      :ellipsoid (ellipsoids :Clarke1866)    :transform { :tx    8      :ty -160     :tz -176     :s  0      :rx  0      :ry  0      :rz  0      }}
    :NAD83       { :key :NAD83      :ellipsoid (ellipsoids :GRS80)         :transform { :tx    1.004  :ty   -1.910 :tz   -0.515 :s -0.0015 :rx  0.0267 :ry  0.00034:rz  0.011  }}
    :NTF         { :key :NTF        :ellipsoid (ellipsoids :Clarke1880IGN) :transform { :tx  168      :ty   60     :tz -320     :s  0      :rx  0      :ry  0      :rz  0      }}
    :OSGB36      { :key :OSGB36     :ellipsoid (ellipsoids :Airy1830)      :transform { :tx -446.448  :ty  125.157 :tz -542.060 :s 20.4894 :rx -0.1502 :ry -0.2470 :rz -0.8421 }}
    :Potsdam     { :key :Potsdam    :ellipsoid (ellipsoids :Bessel1841)    :transform { :tx -582      :ty -105     :tz -414     :s -8.3    :rx  1.04   :ry  0.35   :rz -3.08   }}
    :TokyoJapan  { :key :TokyoJapan :ellipsoid (ellipsoids :Bessel1841)    :transform { :tx  148      :ty -507     :tz -685     :s  0      :rx  0      :ry  0      :rz  0      }}
    :WGS72       { :key :WGS72      :ellipsoid (ellipsoids :WGS72)         :transform { :tx    0      :ty    0     :tz   -4.5   :s -0.22   :rx  0      :ry  0      :rz  0.554  }}
    :WGS84       { :key :WGS84      :ellipsoid (ellipsoids :WGS84)         :transform { :tx    0.0    :ty    0.0   :tz    0.0   :s  0.0    :rx  0.0    :ry  0.0    :rz  0.0    }}
    })


(defprotocol Location
  "A location on the surface of the earth"
  (datum [l])
  (datum-key [l])
  (ellipsoid [l])
  (grid-x [l])
  (grid-y [l])
  (latitude [l])
  (longitude [l])
  (to-cartesian [l])
  (to-point [l])
  (to-point [l d]))


(defrecord Cartesian
  [x y z]
  Location
  ;; datum is a bit meaningless for a Cartesian; get the default.
  (datum [x] (datums :WGS84))
  (datum-key [x] :WGS84)
  (ellipsoid [x] (:ellipsoid (datum x)))
  ;; I already am cartesian; return myself
  (to-cartesian [x] x)
  (to-point [this datum] (let
                           [a (:a (:ellipsoid datum))
                            b (:b (:ellipsoid datum))
                            f (:f (:ellipsoid datum))
                            e2 (- (* 2 f) (* f f)) ;; first eccentricity squared
                            ε2 (/ e2 (- 1 e2)) ;; second eccentricity squared
                            p (sqrt (+ (* (:x this) (:x this)) (* (:y this) (:y this))))
                            ;; distance from minor radius
                            R (sqrt (+ (* p p) (* (:z this) (:z this))))
                            ;; polar radius
                            tanβ (* (/ (* b (:z this))(* a p)) (/ (* b (+ 1 ε2)) R))
                            sinβ (/ tanβ (sqrt (+ 1 (* tanβ tanβ))))
                            cosβ (/ sinβ tanβ)
                            φ (if
                                (Double/isNaN cosβ) 0
                                (atan2 (+ z (* ε2 b sinβ sinβ sinβ))
                                       (- p (* e2 a cosβ cosβ cosβ))))
                            λ (atan2 (:y this) (:x this))
                            v (/ a (sqrt (- 1 (* e2 (sin φ) (sin φ)))))]
                           (Point. (degrees φ) (degrees λ) datum)))
  )


(defn inverse-transform [t]
  "Return a transform which is the inverse of `t`. More generally,
  expects a map `t` whose values are numbers, and returns a map which has
  for each key in `t` a number which is the inverse of the value of the
  same key in `t`."
  (reduce
    merge
    {}
    (map
      #(if (number? (t %)) (hash-map % (- 0 (t %))))
      (keys t))))

;; (inverse-transform { :tx   89.5,   :ty   93.8   :tz  123.1   :s -1.2    :rx  0.0    :ry  0.0    :rz  0.156  })

(defrecord Point
  "A point with an `x` co-ordinate, a `y` co-ordinate, and a datum `d`. We're
  agnostic as to whether `d` is passed as a keyword or a map, but it must be
  taken from `datums`, q.v."
  [x y d]
  Location
  (datum [x]
         (cond
           (keyword? (:d x))
           (datums (:d x))
           (map? (:d x))
           (:d x)))
  (datum-key [x]
             (cond
               (keyword? (:d x))
               (:d x)
               (map? (:d x))
               (:key (:d x))))
  (ellipsoid [x]
             (:ellipsoid (datum [x])))
  (to-cartesian [x]
                (let [φ (radians (latitude x))
                      λ (radians (longitude x))
                      h 0
                      a (:a (:ellipsoid (:datum x)))
                      f (:f (:ellipsoid (:datum x)))
                      sinφ (sin φ)
                      cosφ (cos φ)
                      sinλ (sin λ)
                      cosλ (cos λ)
                      e2 (- (* 2 f) (* f f))
                      v (/ a (sqrt (- 1 (* e2 sinφ sinφ))))]
                  (Cartesian.
                    (* (+ v h) cosφ cosλ)
                    (* (+ v h) cosφ sinλ)
                    (* v (+ h (- 1 e2)) sinφ))))
  (to-point [x] x)
  (to-point [x new-datum]
            (let [od (datum x)
                  nd (if (keyword? new-datum) (datums new-datum) new-datum)
                  c (to-cartesian x)]
              (cond
                (= od nd) x
                (= (:key od) :WGS84) (to-point
                                       (apply-transform c (:transform nd)))
                (= (:key nd) :WGS84) (to-point
                                       (apply-transform
                                         c
                                         (inverse-transformation (:datum x))))
                true
                (to-datum (to-datum x :WGS84) nd)))))

  (latitude [x]
            (:y (to-datum
;; (defn os-grid-x-to-lat-long
;;   ([x y]
;;    (os-grid-x-to-lat-long {:x x :y y})
;;   ([point]
;;    (let [datum
;;   )

;; (defn os-grid-y-to-latitude
;;   [y]
;;   )

(defn

(defn post-code-data-to-addresses
  [filename]
  (map
    (fn [a]
      (s/join
        (list
          "insert into addresses (address:ellipsoid postcode}} latitude, longitude) values ('"
          (:address %) "', '" (:postcode %)
          "', " 0 "," 0 ");"))
      )
    (filter
      #(= (:classification_code_description %) "Dwelling")
      (:results (json/read-str (slurp filename) :key-fn #(keyword (.toLowerCase %)))))))
