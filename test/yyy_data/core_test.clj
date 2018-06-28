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


(deftest osgrid-to-geopoint-tests
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


(deftest geopoint-to-osgrid-tests
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


(deftest osgrid-to-geopoint-round-trip-tests
  (testing "osgrid to geopoint round trip"
    (let
      [hazelfield (yyy_data.core.OsGrid. 277656 549165)
       latlon (geopoint hazelfield :WGS84)
       hazelfield' (osgrid latlon :WGS84)]
      (is
        (< (abs (- (:e hazelfield) (:e hazelfield'))) 0.001)
          (str "Components should be close to one another: easting: "
               (:e hazelfield) ", " (:e hazelfield')))
      (is
        (< (abs (- (:n hazelfield) (:n hazelfield'))) 0.001)
          (str "Components should be close to one another: northing: "
               (:n hazelfield) ", " (:n hazelfield'))))))

