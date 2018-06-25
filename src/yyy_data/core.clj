(ns yyy-data.core
  (:require [adl-support.utils :refer :all]
            [fastmath.complex :refer [pow]]
            [fastmath.core :refer [radians degrees sin cos tan sqrt atan2]]
            [clojure.core.matrix :as mx]
            [clojure.data.json :as json]
            [clojure.string :as s]))

(declare geopoint-to-osgrid vector3d-to-geopoint geopoint-to-vector3d osgrid-to-geopoint)

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

  Yes, I know that the plural of datum is data..."
  {
    ;; transforms: t in metres, s in ppm, r in arcseconds                               tx            ty           tz           s          rx          ry           rz
    :ED50        { :key :ED50       :ellipsoid (ellipsoids :Intl1924)      :transform { :tx   89.5,   :ty   93.8   :tz  123.1   :s -1.2    :rx  0.0    :ry  0.0     :rz  0.156  }}
    :Irl1975     { :key :Irl1975    :ellipsoid (ellipsoids :AiryModified)  :transform { :tx -482.530  :ty  130.596 :tz -564.557 :s -8.150  :rx -1.042  :ry -0.214   :rz -0.631  }}
    :NAD27       { :key :NAD27      :ellipsoid (ellipsoids :Clarke1866)    :transform { :tx    8      :ty -160     :tz -176     :s  0      :rx  0      :ry  0       :rz  0      }}
    :NAD83       { :key :NAD83      :ellipsoid (ellipsoids :GRS80)         :transform { :tx    1.004  :ty   -1.910 :tz   -0.515 :s -0.0015 :rx  0.0267 :ry  0.00034 :rz  0.011  }}
    :NTF         { :key :NTF        :ellipsoid (ellipsoids :Clarke1880IGN) :transform { :tx  168      :ty   60     :tz -320     :s  0      :rx  0      :ry  0       :rz  0      }}
    :OSGB36      { :key :OSGB36     :ellipsoid (ellipsoids :Airy1830)      :transform { :tx -446.448  :ty  125.157 :tz -542.060 :s 20.4894 :rx -0.1502 :ry -0.2470  :rz -0.8421 }}
    :Potsdam     { :key :Potsdam    :ellipsoid (ellipsoids :Bessel1841)    :transform { :tx -582      :ty -105     :tz -414     :s -8.3    :rx  1.04   :ry  0.35    :rz -3.08   }}
    :TokyoJapan  { :key :TokyoJapan :ellipsoid (ellipsoids :Bessel1841)    :transform { :tx  148      :ty -507     :tz -685     :s  0      :rx  0      :ry  0       :rz  0      }}
    :WGS72       { :key :WGS72      :ellipsoid (ellipsoids :WGS72)         :transform { :tx    0      :ty    0     :tz   -4.5   :s -0.22   :rx  0      :ry  0       :rz  0.554  }}
    :WGS84       { :key :WGS84      :ellipsoid (ellipsoids :WGS84)         :transform { :tx    0.0    :ty    0.0   :tz    0.0   :s  0.0    :rx  0.0    :ry  0.0     :rz  0.0    }}
    })


(defprotocol Location
  "A location on the surface of the earth"
  (datum [location])
  (datum-key [location])
  (ellipsoid [location])
  (geopoint [location datum])
  (grid-x [location])
  (grid-y [location])
  (latitude [location])
  (longitude [location])
  (osgrid [location datum])
  (vector3d [location])
  )


(defprotocol Transformable
  (apply-transform [this transform]))


(defrecord Vector3d
  [x y z]
  Location
  ;; datum is a bit meaningless for a Vector3d; get the default.
  (datum [x] (datums :WGS84))
  (datum-key [x] :WGS84)
  (ellipsoid [x] (:ellipsoid (datum x)))
  ;; I already am vector3d; return myself
  (vector3d [x] x)
  (geopoint [this datum] (vector3d-to-geopoint))
  Transformable
  (apply-transform [this transform]
                   (let
                     [s1 (+ (/ (:s transform) 1e6) 1)              ;; scale
                      rx (radians (/ (:rx transform) 3600))     ;; x-rotation: normalise arcseconds to radians
                      ry (radians (/ (:ry transform) 3600))     ;; y-rotation: normalise arcseconds to radians
                      rz (radians (/ (:rz transform) 3600))     ;; z-rotation: normalise arcseconds to radians
                      x2 (-
                           (+ (:tx transform)
                              (* (.x this) s1))
                           (+
                             (* (.y this) rz)
                             (* (.z this) ry)))
                      y2 (+
                           (:ty transform)
                           (* (.x this) rz)
                           (* (.y this) s1)
                           (- 0 (* (.z this) rx)))
                      z2 (+
                           (.z this)
                           (- 0 (* (.x this) ry))
                           (* (.y this) rx)
                           (* (.z this) s1))]
                     (Vector3d. x2 y2 z2))))


(defn inverse-transform
  "Return a transform which is the inverse of `t`. More generally,
  expects a map `t` whose values are numbers, and returns a map which has
  for each key in `t` a number which is the inverse of the value of the
  same key in `t`."
  [t]
  (reduce
    merge
    {}
    (map
      #(if (number? (t %)) (hash-map % (- 0 (t %)))) ;; (hash-map % (t %)))
      (keys t))))

(inverse-transform { :tx   89.5,   :ty   93.8   :tz  123.1   :s -1.2    :rx  0.0    :ry  0.0    :rz  0.156  })

(defn to-fixed
  "Round the number `n` to `p` places after the decimal point."
  [n p]
  (let [m (reduce *' 1 (repeat p 10))]
    (double (/ (int (*' n m)) m))))


(defn to-datum
  ;; TODO: this obviously doesn't work but I'm trying to debug a compilation problem
  [geopoint datum]
  geopoint)


;; (to-fixed 1234.56789 4)

(defrecord GeoPoint
  [latitude longitude datum]
  ;;   "A point with an `x` co-ordinate, a `y` co-ordinate, and a datum `d`. We're
  ;;   agnostic as to whether `d` is passed as a keyword or a map, but it must be
  ;;   taken from `datums`, q.v."
  Location
  (datum [location]
         (cond
           (keyword? (:datum location))
           (datums (:datum location))
           (map? (:datum location))
           (:datum location)))
  (datum-key [location]
             (cond
               (keyword? (:datum location))
               (:datum location)
               (map? (:datum location))
               (:key (:datum location))))
  (ellipsoid [location]
             (:ellipsoid (datum [location])))
  (vector3d [this] (geopoint-to-vector3d this))
  (geopoint [location new-datum]
            (let [od (datum location)
                  nd (if (keyword? new-datum) (datums new-datum) new-datum)
                  c (vector3d location)]
              (cond
                (= od nd) location
                (= (:key od) :WGS84) (geopoint
                                       (apply-transform c (:transform nd))
                                       (:datum location))
                (= (:key nd) :WGS84) (geopoint
                                       (apply-transform
                                         c
                                         (inverse-transform (datums (:datum location))))
                                       (:datum location))
                true
                (to-datum (to-datum location :WGS84) nd))))
  (latitude [location]
            (:latitude (to-datum location :WGS84)))
  (longitude [location]
             (:longitude (to-datum location :WGS84)))
  (grid-x [location]
          (:e (osgrid location (:datum location))))
  (grid-y [location]
          (:n (osgrid location (:datum location))))
  (osgrid [location datum]
          (geopoint-to-osgrid location (:datum location))))

(def location (GeoPoint. 54.8240911 -3.9170342 :WGS84))
(def od (datum location))
(def nd (datums :NTF))
(def c (vector3d location))

(apply-transform c (:transform nd))

(defrecord OsGrid
  [e n]
  Location
  ;;  "A location on the surface of the earth"
  (datum [location] (:WGS84 datums))
  (datum-key [location] :WGS84)
  (ellipsoid [location] (:ellipsoid :WGS84))
  (grid-x [location] e)
  (grid-y [location] n)
  (latitude [location] (latitude (geopoint location :WGS84)))
  (longitude [location] (longitude (geopoint location :WGS84)))
  (vector3d [location])
  (geopoint [location datum]
            (osgrid-to-geopoint location datum))
  (osgrid [location datum] location))


(defn vector3d-to-geopoint
  [this datum]
  (let
    [a (:a (:ellipsoid datum))
     b (:b (:ellipsoid datum))
     f (:f (:ellipsoid datum))
     e² (- (* 2 f) (* f f)) ;; first eccentricity squared
     ε² (/ e² (- 1 e²)) ;; second eccentricity squared
     p (sqrt (+ (* (.x this) (.x this)) (* (.y this) (.y this))))
     ;; distance from minor radius
     R (sqrt (+ (* p p) (* (:z this) (:z this))))
     ;; polar radius
     tanβ (* (/ (* b (:z this))(* a p)) (/ (* b (+ 1 ε²)) R))
     sinβ (/ tanβ (sqrt (+ 1 (* tanβ tanβ))))
     cosβ (/ sinβ tanβ)
     φ (if
         (Double/isNaN cosβ) 0
         (atan2 (+ (.z this) (* ε² b sinβ sinβ sinβ))
                (- p (* e² a cosβ cosβ cosβ))))
     λ (atan2 (:y this) (:x this))
     v (/ a (sqrt (- 1 (* e² (sin φ) (sin φ)))))]
    (GeoPoint. (degrees φ) (degrees λ) datum)))


(defn geopoint-to-osgrid
  [geopoint datum]
  ;; for bizarrely over-complicated trigonometry, look no further.
  (let [point (.geopoint geopoint (or datum :OSGB36))
        φ     (radians (latitude point))
        λ     (radians (longitude point))
        a     6377563.396       ;; Airy 1830 major & minor semi-axes
        b     6356256.909
        F0    0.9996012717      ;; OS Grid scale factor on central meridian
        φ0    (radians 49)   ;; OS Grid true origin latitude
        λ0    (radians -2)   ;; OS Grid true origin longitude
        Δφ    (- φ φ0)          ;; latitude offset from origin
        Δλ    (- λ λ0)          ;; longitude offset from origin
        Δλ²   (reduce * 1 (repeat 2 Δλ))
        Δλ³   (reduce * 1 (repeat 3 Δλ))
        Δλ⁴   (reduce * 1 (repeat 4 Δλ))
        Δλ⁵   (reduce * 1 (repeat 5 Δλ))
        Δλ⁶   (reduce * 1 (repeat 6 Δλ))
        N0    -100000           ;; northing of true origin, metres
        E0    400000            ;; easting of true origin, metres
        e²    (- 1 (/ (* b b) (* a a)))  ;; eccentricity squared
        n     (/ (- a b) (+ a b))
        n²    (* n n)
        n³    (* n n n)
        sinφ  (sin φ)
        sin²φ (* (sin φ) (sin φ))
        cosφ  (cos φ)
        v     (* a (/ F0 (sqrt (- 1 (* e² sin²φ)))))
        ;; nu = transverse radius of curvature
        ρ     (/ (* a F0 (- 1 e²)) (pow (- 1 (* e² sin²φ)) 1.5))
        ;; rho = meridional radius of curvature
        η2    (/ v (- ρ 1))     ;; beware confusing η2 with n²
        Ma    (* (+ 1 n (* (/ 5 4) n²) (* (/ 5 4) n³)) Δφ)
        Mb    (*
                (+ (* 3 n) (* 3 n²) (* (/ 21 8) n³))
                (sin Δφ) (cos (+ φ φ0)))
        Mc    (*
                (+ (* (/ 15 8) n²) (* (/ 15 8) n³))
                (sin (* 2 Δφ)) (cos (* 2 (+ φ φ0))))
        Md    (* (/ 35 24) n³ (sin (* 3 Δφ)) (cos (* 3 (+ φ φ0))))
        M     (* b F0 (+ (- Ma Mb) (- Mc Md)))
        ;; meridional arc
        cos³φ (reduce * 1 (repeat 3 cosφ))
        cos⁵φ (reduce * 1 (repeat 5 cosφ))
        tan²φ (reduce * 1 (repeat 2 (tan φ)))
        tan⁴φ (reduce * 1 (repeat 4 (tan φ)))
        I     (+ M N0)
        II    (* (/ v 2) sinφ cosφ)
        III   (* (/ v 24) sinφ cos³φ (- 5 (+ tan²φ (* 9 η2))))
        ;; var IIIA = (ν/720)*sinφ*cos5φ*(61-58*tan2φ+tan4φ);
        ;; TODO: CHECK JAVASCRIPT OPERATOR PRECEDENCE!
        IIIA  (* (/ v 720) sinφ cos⁵φ (- 61 (+ (* 58 tan²φ) tan⁴φ)))
        IV    (* v cosφ)
        V     (* (/ v 6) cos³φ (- (/ v ρ) tan²φ))
        ;; var VI = (ν/120) * cos5φ * (5 - 18*tan2φ + tan4φ + 14*η2 - 58*tan2φ*η2);
        VI    (*
                (/ v 120)
                cos⁵φ
                (- 5
                   (+ (* 18 tan²φ)
                      tan⁴φ
                      (* 14 η2) (- 0 (* 58 tan²φ η2)))))
        N     (to-fixed (+ I (* II Δλ²) (+ III Δλ⁴) (* IIIA Δλ⁶)) 3)
        E     (to-fixed (+ E0 (* IV Δλ) (* V Δλ³) (* VI Δλ⁵)) 3)]
    (OsGrid. E N)))


(defn geopoint-to-vector3d
  [geopoint]
  (let [datum (cond
               (keyword? (:datum geopoint))
               (datums (:datum geopoint))
               (map? (:datum geopoint))
               (:datum geopoint))
        φ (radians (latitude geopoint))
        λ (radians (longitude geopoint))
        h 0
        a (:a (:ellipsoid datum))
        f (:f (:ellipsoid datum))
        sinφ (sin φ)
        cosφ (cos φ)
        sinλ (sin λ)
        cosλ (cos λ)
        e² (- (* 2 f) (* f f))
        v (/ a (sqrt (- 1 (* e² sinφ sinφ))))]
    (Vector3d.
      (* (+ v h) cosφ cosλ)
      (* (+ v h) cosφ sinλ)
      (* v (+ h (- 1 e²)) sinφ))))


(defn osgrid-to-geopoint
  [osgrid datum]
  (let
    [d     (or datum :WGS84)
     E     (.e osgrid)
     N     (.n osgrid)
     a     6377563.396         ;; Airy 1830 major semi-axis
     b     6356256.909         ;; Airy 1830 minor semi-axis
     F0    0.9996012717        ;; national grid scale factor on central meridian
     φ0    (radians 49)     ;; national grid true origin latitude
     λ0    (radians -2)     ;; national grid true origin longitude
     N0    -100000             ;; northing of true origin, metres
     E0    400000              ;; easting of true origin, metres
     e²    (- 1
              (/
                (* b b)
                (* a a)))      ;; eccentricity squared
     n     (/ (- a b) (+ a b))
     n²    (* n n)
     n³    (* n n n)
     [M φ] (loop  [φ φ0 M 0]
             (let
               [φ₁ (+ φ (/ (- N N0 M) (* a F0)))
                Δφ    (- φ φ0)
                Ma (* (+ 1 n (* 5/4 n²) (* 5/4 n³)) Δφ)
                Mb (* (+ (* n 3) (* n² 3) (* n³ 21/8)) (sin Δφ) (cos Δφ))
                Mc (* (+ (* n² 15/8) (* n³ 15/8)) (sin (* 2 Δφ)) (cos (* 2 Δφ)))
                Md (* 35/24 n³ (sin (* 3 Δφ)) (cos (* 3 Δφ)))
                M₁ (* b F0 (+ (- 0 Ma) Mb Mc (- 0 Md)))]
               (if
                 (>= (- N N0 M₁) 0.00001)
                 (recur M₁ φ₁)
                 [M₁ φ₁])))
     sinφ  (sin φ)
     sin²φ (* (sin φ) (sin φ))
     cosφ  (cos φ)
     v     (* a (/ F0 (sqrt (- 1 (* e² sin²φ)))))
     v³    (reduce * 1 (repeat 3 v))
     v⁵    (reduce * 1 (repeat 5 v))
     v⁷    (reduce * 1 (repeat 7 v))
     ;; nu = transverse radius of curvature
     ρ     (/ (* a F0 (- 1 e²)) (pow (- 1 (* e² sin²φ)) 1.5))
     ;; rho = meridional radius of curvature
     η2    (/ v (- ρ 1))     ;; beware confusing η2 with n²
     tanφ  (tan φ)
     tan²φ (reduce * 1 (repeat 2 tanφ))
     tan⁴φ (reduce * 1 (repeat 4 tanφ))
     tan⁶φ (reduce * 1 (repeat 6 tanφ))
     secφ  (/ 1 cosφ)
     VII   (/ tanφ (* 2 ρ v))
     ;; TODO: check Javascript operator precedence!
     VIII  (* (/ tanφ (* 24 ρ v³) (+ 5 (* 3 tan²φ) η2 (- 0 (* 9 tan²φ η2)))))
     ;;     var VIII = tanφ/(24*ρ*ν3)*(5+3*tan2φ+η2-9*tan2φ*η2);
     IX    (* (/ tanφ (* 720 ρ v⁵)) (+ 61 (* 90 tan²φ) (* 45 tan⁴φ)))
     ;;     var IX = tanφ/(720*ρ*ν5)*(61+90*tan2φ+45*tan4φ);
     X     (/ secφ v)
     XI    (* (/ secφ (* 6 v³)) (+ (/ v ρ) (* 2 tan²φ)))
     ;;    var XI = secφ/(6*ν3)*(ν/ρ+2*tan2φ);
     XII   (* (/ secφ (* 120 v⁵)) (+ 5 (* 28 tan²φ) (* 24 tan⁴φ)))
     ;;    var XII = secφ/(120*ν5)*(5+28*tan2φ+24*tan4φ);
     XIIA  (* (/ secφ (* 5040 v⁷)) (+ 61 (* 622 tan²φ) (* 1322 tan⁴φ) (* 720 tan⁶φ)))
     ;;    var XIIA = secφ/(5040*ν7)*(61+662*tan2φ+1320*tan4φ+720*tan6φ);
     Δe    (- E E0)
     Δe²   (reduce * 1 (repeat 2 Δe))
     Δe³   (reduce * 1 (repeat 3 Δe))
     Δe⁴   (reduce * 1 (repeat 4 Δe))
     Δe⁵   (reduce * 1 (repeat 5 Δe))
     Δe⁶   (reduce * 1 (repeat 6 Δe))
     Δe⁷   (reduce * 1 (repeat 6 Δe))
     φ₁    (+ φ (- 0 (+ VII Δe²)) (* VIII Δe⁴) (- 0 (* IX Δe⁷)))
     λ₁    (+ λ0 + (* X Δe) (- 0 (* XI Δe³)) (* IX Δe⁶) (- 0 (* XIIA Δe⁷)))]
    (.geopoint (GeoPoint. (degrees φ₁) (degrees λ₁) :OSGB36) datum)))


(defn post-code-data-to-addresses
  [filename]
  (map
    (fn [a]
      (let [location (.geopoint (OsGrid. (:x_coordinate a) (:y_coordinate a)) :WGS84)]
        (s/join
          (list
            "insert into addresses (address:ellipsoid postcode}} latitude, longitude) values ('"
            (:address a) "', '" (:postcode a)
            "', " (.latitude location) "," (.longitude location) ");"))
        ))
    (filter
      #(= (:classification_code_description %) "Dwelling")
      (map
        :dpa
        (:results
          (json/read-str
            (slurp filename)
            :key-fn #(keyword (.toLowerCase %))))))))


;; (def home (GeoPoint. 54.8240911 -3.9170342 :WGS84))

;; (.osgrid home :WGS84)

;; (.datum home)
;; (datums (:datum home))
;; (:ellipsoid (datums (:datum home)))

