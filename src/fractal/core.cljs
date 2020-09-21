(ns fractal.core)

(enable-console-print!)

;;a complex number is represented as a vector [a b] where a is the real part and b is the imaginary part

(defn add [a b]
  "a and b can be regular numbers or complext numbers. returns a vector representing a complex number"
  (cond
    (and (number? a)
         (number? b)) [(+ a b) 0]

    (and (number? a)
         (vector? b)) [(+ a (first b)) (second b)]

    (and (vector? a)
         (vector? b)) [(+ (first a) (first b))
                       (+ (second a) (second b))]

    (and (vector? a)
         (number? b)) [(+ (first a) b) (second a)]))

(defn multiply [a b]
  (cond
    (and (number? a)
         (number? b)) [(* a b) 0]

    (and (number? a)
         (vector? b)) [(* a (first b)) (* a (second b))]

    (and (vector? a)
         (vector? b)) [(+ (* (first a) (first b))
                          (* (second a) (second b)))
                       (+ (* (second a) (first b))
                          (* (first a) (second b)))]
    (and (vector? a)
         (number? b)) [(* (first a) b) (second a)]))

(defn multiply-i
  "multiply complex number a and b"
  [a b]
  (if (and (vector? a)
           (vector? b))
    [(+ (* (first a) (first b))
        (* -1 (second a) (second b)))
     (+ (* (second a) (first b))
        (* (first a) (second b)))]
    (multiply a b))) 

(defn subtract [a b]
  (add a (multiply -1 b)))

(defn square [a]
  (multiply a a))

(defn square-i [a]
  (multiply-i a a))

(defn distance2
  "use the pythagorean theorem to find distance between two points.
  actually square of the distance because taking square root isnt necessary"
  [[a b] [c d]]
  (let [[real imaginary] (add (square (subtract [c 0] [a 0]))
                              (square (subtract [0 d] [0 b])))]
    real))

(defn escape? [[real imaginary :as c]]
  (let [max-iteration 25]
    (loop [zn c
           n 0]
      (let [d (distance2 zn [0 0])]
        (cond
          (> n max-iteration) false
          (< d 4)  (recur (add (square-i zn) c)
                          (inc n))
          (>= d 4) true)))))

(defn plot [ctx width height]
  (let [delta-x (atom (/ 4 width))
        delta-y (atom (/ 2 height))
        x-start -2
        y-start 1]
    (doseq [i (range width)
            :let [x (+ x-start (* i @delta-x))]]
      (doseq [j (range height)
              :let [y (- y-start (* j @delta-y))]]
        (if (escape? [x y])
          (set! (.-fillStyle ctx) "rgb(155,0,0)")
          (set! (.-fillStyle ctx) "rgb(18,161,56)"))
        (.fillRect ctx i j 1 1)))))

(defn pr-num
  "prints out complex number for debugging purposes"
  [c]
  (cond
    (number? c) (prn (str c))
    (vector? c) (let [[real imaginary] c]
                  (cond
                    (and real imaginary) (prn (str real "+" imaginary "i"))
                    (-> real nil? not) (prn (str real))
                    (-> imaginary nil? not) (prn (str imaginary "i"))))))

(defn init []
  (prn "init")
  (let [canvas (js/document.getElementById "canvas")
        ctx (.. canvas (getContext "2d"))
        width js/window.innerWidth
        height js/window.innerHeight]
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)
    (plot ctx width height)))


(init)



