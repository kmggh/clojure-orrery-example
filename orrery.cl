;; Copyright 2021 by Ken Guyton.  All rights reserved.

"Orrery in Clojure

This is a solar system orrery, a model of the solar system based on 
circular orbits and planets moving at constant speed based on Kepler's
third law and their approximate orbital radii in astronomical units.

This is more LISP-like and probably not idiomatic Clojure.  I'm not sure
if I'm sacrificing any performance but this is a short program.  I will say
I prefer writing a LISP-like language like LISP more than like another
language, like Python.  For all of the touting of the functional programming
aspects of Clojure, I find it's 'impure' aspects, e.g., mutable variables
and do sequences of statements a bit distasteful.  I say that as a fully
dedicated object-oriented programmer who only writes in Python these days.
However, I did capitulate and use do in order to handle printing.

One more thing.  Though they're archaic, and though first, second, and
rest are readable, I miss car and cdr.
"


;; The planets, sans Holst.
(def mercury (list "Mercury" 0.39 0))
(def venus (list "Venus" 0.72 0))
(def earth (list "Earth" 1.0 0.0))
(def mars (list "Mars" 1.5 0))
(def ceres (list "Ceres" 2.77 0))
(def jupiter (list "Jupiter" 5.2 0.0))
(def saturn (list "Saturn" 9.5 0.0))
(def uranus (list "Uranus" 19. 0.0))
(def neptune (list "Neptune" 30. 0.0))
(def pluto (list "Pluto" 39.4 0.0))


;; Access various properties of a planet.
;;
;; The planet's name, it's orbit semi-major axis in A.U., and it's position
;; angle in it's orbit given in degrees.  Note that A.U. are astronomical
;; units, defined such that the Earth's orbit has a semi-major axis of
;; 1 A.U., to a fairly good approximation.  The current formal definition is
;; modified to a more precise standard than the actual orbit of the Earth.
(defn planet-name [x] (first x))
(defn au [x] (second x))
(defn pos [x] (first (rest (rest x))))


;; Return a planet's orbital period in years given its radius in A.U.
;;
;; This is Kepler's third law, really the only astronomy in this program. 
(defn period [x] (Math/pow (au x) 1.5))


;; Return a delta angle for a delta time.
(defn advance-for-time [period dt] (* (/ dt period) 360.0))


;; Normalize any angle in degrees to the range 0 <= angle < 360.0.
(defn normalize-angle [angle] (cond (>= angle 360)
                                    (normalize-angle (- angle 360))
				    :else angle))


;; Print a planet's name and current angle in it's orbit.
(defn print-planet [planet delta-yr] 
    (println (planet-name planet)
        (format "%.2f" (normalize-angle
	    (advance-for-time (period planet) delta-yr)))))


;; Convert months to fractional years.
(defn delta-yr-months [months] (/ months 12.0))


;; Compute the next year, possibly fractional, in the sequence.
(defn next-delta-yr [a-delta-yr step-yr] (+ a-delta-yr step-yr))


;; Print the planets and the current year since starting the orrery.
(defn print-planets [planets delta-yr]
    (do
        (println "\nTime delta (yr): " (format "%.2f" delta-yr) "\n")
        (doseq [a-planet planets] (print-planet a-planet delta-yr))))


;; Run the orrery with a step in years and a max number of years to run.
(defn run-orrery [planets delta-yr step-yr max-yr]
    (cond (< delta-yr max-yr) (do
        (print-planets planets delta-yr)
	(run-orrery planets (next-delta-yr delta-yr step-yr) step-yr max-yr))))


;; The solar system, short list.
(def planets (list mercury earth mars jupiter))

;; The solar system, long list.
(def planets-all (list mercury venus earth mars ceres jupiter saturn
                  uranus neptune pluto))

;; The solar system inner planets.
(def planets-inner (list mercury venus earth mars))

;; The solar system outer planets.
(def planets-outer (list earth jupiter saturn uranus neptune))

;; The starting time set to 0 years.
(def delta-yr 0.0)


;; Run the orrery for the inner planets with a shorter time step.
(defn run-orrery-inner []
    (let [step-yr (delta-yr-months 0.5)
        max-yr 1.0]
	(run-orrery planets-inner delta-yr step-yr max-yr)))


;; Run the orrery for the outer planets with a longer time step.
(defn run-orrery-outer []
    (let [step-yr 1.0 max-yr 10.0]
	(run-orrery planets-outer delta-yr step-yr max-yr)))


;; Run the orrery for all of the planets.
(defn run-orrery-all []
    (let [step-yr 1.0 max-yr 10.0]
	(run-orrery planets-all delta-yr step-yr max-yr)))


(run-orrery-all)
;; (run-orrery-inner)
;; (run-orrery-outer)
