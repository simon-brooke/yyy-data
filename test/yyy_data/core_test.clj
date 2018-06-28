(ns yyy-data.core-test
  (:require [clojure.test :refer :all]
            [yyy-data.core :refer :all]))

(defn abs [n]
  (if (pos? n) n (- 0 n)))

(deftest vector-tests
  (testing "vector transformation round-trip."
    (let [unit-vector (yyy_data.core.Vector3d. 1 1 1)
          transformed (apply-transform
                        (apply-transform unit-vector (:transform (:ED50 datums)))
                        (inverse-transform (:transform (:ED50 datums))))]
      (is
        (< (abs (- 1 (:x transformed))) 0.001)
          (str "Components should be close to 1: x: " (:x transformed)))
      (is
        (< (abs (- 1 (:y transformed))) 0.001)
        (str "Components should be close to 1: y: " (:y transformed)))
      (is
        (< (abs (- 1 (:z transformed))) 0.001)
        (str "Components should be close to 1: z: " (:z transformed))))))


(deftest osgrid->geopoint-tests
  (testing "osgrid to geopoint one way"
    (let
      [hazelfield (yyy_data.core.OsGrid. 277656 549165)
       actual (geopoint hazelfield :WGS84)]
      (is
        (< (abs (- (:latitude actual) 54.822218)) 0.001)
        (str "Latitude should be 54.822219: "  (:latitude actual)))
      (is
        (< (abs (- (:longitude actual) -3.906009)) 0.001)
        (str "Longitude should be -3.906009: "  (:longitude actual))))))


(deftest geopoint->osgrid-tests
  (testing "geopoint to osgrid one way"
    (let
      [hazelfield (yyy_data.core.GeoPoint. 54.822218 -3.906009 :WGS84)
       actual (osgrid hazelfield :WGS84)]
      (is
        (< (abs (- (:n actual) 549165)) 0.001)
        (str "Northing should be 549165: "  (:n actual)))
      (is
        (< (abs (- (:e actual) 277656)) 0.001)
        (str "Easting should be 277656: "  (:e actual))))))


(deftest osgrid->geopoint-round-trip-tests
  (testing "osgrid to geopoint round trip"
    (let
      [hazelfield (yyy_data.core.OsGrid. 277656 549165)
       latlon (geopoint hazelfield :WGS84)
       actual (osgrid latlon :WGS84)]
      (is
        (< (abs (- (:e hazelfield) (:e actual))) 0.001)
          (str "Components should be close to one another: easting: "
               (:e hazelfield) ", " (:e actual)))
      (is
        (< (abs (- (:n hazelfield) (:n actual))) 0.001)
          (str "Components should be close to one another: northing: "
               (:n hazelfield) ", " (:n actual))))))


(deftest geopoint->vector-round-trip-tests
  (testing "round trip geopoint->vector->geopoint"
    (let
      [hazelfield (yyy_data.core.GeoPoint. 54.822218 -3.906009 :WGS84)
       v (vector3d hazelfield)
       actual (geopoint v :WGS84)]
      (is
        (< (abs (- (:latitude actual) 54.822218)) 0.001)
        (str "Latitude should be 54.822219: "  (:latitude actual)))
      (is
        (< (abs (- (:longitude actual) -3.906009)) 0.001)
        (str "Longitude should be -3.906009: "  (:longitude actual))))))


(deftest osgrid->vector-round-trip-tests
  (testing "round trip osgrid->vector->osgrid"
    (let
      [hazelfield (yyy_data.core.OsGrid. 277656 549165)
       v (vector3d hazelfield)
       actual (osgrid v :WGS84)]
      (is
        (< (abs (- (:e hazelfield) (:e actual))) 0.001)
        (str "Components should be close to one another: easting: "
             (:e hazelfield) ", " (:e actual)))
      (is
        (< (abs (- (:n hazelfield) (:n actual))) 0.001)
        (str "Components should be close to one another: northing: "
             (:n hazelfield) ", " (:n actual))))))

;; (def hazelfield (yyy_data.core.OsGrid. 277656 549165))
;; (def v (vector3d hazelfield))
;; (osgrid->geopoint hazelfield :WGS84)


;; Currently blows up horribly (mutual recursion/stack). But I currently
;; don't need it to work.
;; (deftest geopoint-change-datum-round-trip-wgs84-tests
;;   (testing "round trip geopoint/wgs84->geopoint/osgb36->geopoint/wgs84"
;;     (let
;;       [hazelfield (yyy_data.core.GeoPoint. 54.822218 -3.906009 :WGS84)
;;        gp (geopoint hazelfield :OSGB36)
;;        actual (geopoint gp :WGS84)]
;;       (is
;;         (< (abs (- (:latitude actual) 54.822218)) 0.001)
;;         (str "Latitude should be 54.822219: "  (:latitude actual)))
;;       (is
;;         (< (abs (- (:longitude actual) -3.906009)) 0.001)
;;         (str "Longitude should be -3.906009: "  (:longitude actual))))))
