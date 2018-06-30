(ns yyy-data.core
  (:require [adl-support.utils :refer :all]
            [clojure.string :as s]
            ))

(def surnames
  [
    ;; top 20 from Wikipedia
    "Smith"     ;; (1.28%) (occupational name)
    "Brown"     ;; (0.94%) (nickname)
    "Wilson"    ;; (0.89%) (patronym)
    "Robertson" ;; (0.78%) (patronym)
    "Thomson"   ;; (0.78%) (patronym)
    "Campbell"  ;; (0.77%) (nickname)
    "Stewart"   ;; (0.73%) (occupational name)
    "Anderson"  ;; (0.70%) (patronym)
    "Scott"     ;; (0.55%) (ethnic name)
    "Murray"    ;; (0.53%) (territorial name)
    "MacDonald" ;; (0.52%) (patronym)
    "Reid"      ;; (0.52%) (nickname)
    "Taylor"    ;; (0.49%) (occupational name)
    "Clark"     ;; (0.47%) (occupational name)
    "Ross"      ;; (0.43%) (territorial name)
    "Young"     ;; (0.42%) (nickname)
    "Mitchell"  ;; (0.41%) (patronym; nickname)
    "Watson"    ;; (0.41%) (patronym)
    "Paterson"  ;; (0.40%) (patronym)
    "Morrison"  ;; (0.40%) (patronym)
    ;; others off the top of my head
    "Aitkenhead"
    "Baxter"
    "Cooper"
    "Douglas"

    "Frazer"
    "Gourlay"
    "Henry"
    "Keith"
    "Lang"
    "Mullen"
    "Nicholson"

    "Patterson"

    "Riddoch"
    "Sutherland"
    "Thomson"

    ])

(def forenames
  {:unknown ["Alex"
             "Charlie"
             "Jack"
             "Jude"
             "Leslie"]
   :non-binary [] ;; non-binary people may have any personal name.
   :female ["Alice"
            "Belinda"
            "Catriona"
            "Dorothy"
            "Elfrida"
            "Fiona"
            "Geraldine"
            "Henrietta"
            "Iris"
            "Joan"
            "Karen"
            "Lucy"
            "Mharidh"
            "Noreen"
            "Olivia"
            "Pauline"
            "Queenie"
            "Ruth"
            "Shirley"
            ;; T?
            "Ursula"
            "Vonnie"
            "Wanda"
            ;; X?
            "Ysabel"
            "Zara"]
   :male ["Alistair"
          "Bruce"
          "Calum"
          "Douglas"
          "Edward"
          "Fergus"
          "Gary"
          "Hamish"
          "Iain"
          "James"
          "Keith"
          "Lachlan"
          "Michael"
          "Nicholas"
          "Oliver"
          "Patrick"
          "Quentin"
          "Robert"
          "Simon"
          "Torquil"
          "Uchtred"
          "Victor"
          "William"
          ;; X?
          ;; Y?
          ;; Z?
          ]})

(def genders
  ;; assume a population 40% female, 40% male, 10% non-binary and 10% unknown gender
  [:female :female :female :female :male :male :male :male :unknown :non-binary])

(defn make-elector
  ([dwelling]
   (make-elector dwelling (rand-nth surnames)))
  ([dwelling s]
   (let
     [gender (rand-nth genders)
      surname (if (nil? s) (rand-nth surnames) s)
      forename (rand-nth
                 (case
                   gender
                   :male (concat (:male forenames) (:unknown forenames))
                   :female (concat (:female forenames) (:unknown forenames))
                   :unknown (:unknown forenames)
                   (reduce concat [] (vals forenames))))]
     {:gender (capitalise (name gender))
      :name (s/join " " (list forename surname))
      :dwelling dwelling})))

(defn make-electors
  ([]
   (for [d (range 100)]
     (make-electors d)))
  ([dwelling]
  (let [n (+ 1 (rand-int 5)) ;; between 1 and 6 electors in a household
        s (if (< (rand-int 3) 2) (rand-nth surnames))]
    (make-electors dwelling s n)))
  ([dwelling s n]
   (if
     (zero? n) '()
     (cons (make-elector dwelling s)
           (make-electors dwelling s (- n 1))))))



(defn generate-insert-query
  [elector]
  (str "INSERT INTO electors (name, gender, dwelling_id) values ('"
       (:name elector)
       "', '"
       (:gender elector)
       "', "
       (:dwelling elector)
       ");"))

(spit
  "insert-electors.sql"
  (s/join "\n\n" (map generate-insert-query (flatten (make-electors)))))


