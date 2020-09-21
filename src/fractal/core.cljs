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

(defn square-i [a]
  (multiply-i a a))

(defn square [a]
  (multiply a a))

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
  (let [delta-x (atom (/ 2 width))
        delta-y (atom (/ 1 height))
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

(defn pr-num [c]
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



(comment
  (escape? [0 0])
  
  (add 1 1)
  (pr-num (add [1] [1 1]))

  (pr-num [1 20])
  
  (subtract (square [1 1])
            (square [1 1]))

  (subtract [5 0] [5 0])
  (multiply [5 1] [5 1])
  (square [5 1])

  (let [c [3 2]
        d [5 -1]]
    (add c d)
    )
  
  (distance [3 2] 
            [5 -1])

  (add (square (subtract -1 2))
       (square (subtract 5 3)))

  (square (subtract [5 0] [3 0]))
  (square (subtract [0 -1] [0 2]))

  (multiply-i [0 -3] [0 -3])
  (multiply [0 -3] [0 -3])
  
  (square [0 -3])
  (square-i [0 -3])

  (multiply [0 -3] [0 -3])
  
  (subtract [5 1] [3 10])

  (subtract [3 2] [1 5])
  
  (multiply 2 [3 4])
  (multiply [-1 0] [1 1])
  (multiply -1  [1 1])

  (multiply-i [1 2] [3 4])
  (multiply-i 2 [5 -4])
  
  (init)

  (multiply 3 4)

  
  )
