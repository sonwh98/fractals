(ns fractal.core)

(enable-console-print!)

;;a complex number is represented as a vector [a b] where a is the real part and b is the imaginary part

(defn add-i [a b]
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

(defn multiply-i [a b]
  (if (and (vector? a)
           (vector? b))
    [(+ (* (first a) (first b))
        (* -1 (second a) (second b)))
     (+ (* (second a) (first b))
        (* (first a) (second b)))]
     (multiply a b)))

(defn subtract-i [a b]
  (add-i a (multiply-i -1 b)))

(defn square-i [a]
  (multiply-i a a))

(defn square [a]
  (multiply a a))

(defn distance
  "use the pythagorean theorem to find distance between two points"
  [[a b] [c d]]
  (let [[real imaginary] (add-i (square (subtract-i [c 0] [a 0]))
                                (square (subtract-i [0 d] [0 b])))]
    (Math/sqrt real)))

(defn generate-grid
  "The coordinate system of a canvas, origin (0,0) is top left with x,y growing positive down left,
  but the normal cartesian coordinate system the origin (0,0) is bottom left with x,y growing positive left, up.
  Normalize the width and height of the canvas to a unit of 1 which is where the mandebrot set lives which mean
  each pixel has a width and hiehght of 1/width and 1/height"
  [width height]
  (let [delta-x (/ 1 width)
        delta-y (/ 1 height)
        x (atom -1.5)
        y (atom 1)]
    (for [i (range width)
          :let [_ (swap! x (fn [x] (+ x delta-x)))]]
      (for [j (range height)
            :let [_ (swap! y (fn [y] (- y delta-y)))]]
        [i j [@x @y]]))))

(defn escape? [[real imaginary :as c]]
  (let [max-iteration 30]
    (loop [zn c
           n 0]
      (prn zn)
      (if (< n max-iteration)
        (recur (add-i (square zn) c)
               (inc n))
        (if (< (distance zn [0 0]) 1) 
          false
          true)))))

(defn plot-mandelbrot [ctx grid]
  (doseq [row grid]
    (doseq [ [row col [x y]] row
            :let [escaped? (escape? [x y])]]
      (prn [row col] [x y] " escaped?=" escaped?)
      (if escaped?
        (set! (.-fillStyle ctx) "rgb(155,0,0)")
        (set! (.-fillStyle ctx) "rgb(18,161,56)"))
      
      (.fillRect ctx row col 1 1))))

(defn pr-num [c]
  (cond
    (number? c) (prn (str c))
    (vector? c) (let [[real imaginary] c]
                  (cond
                    (and real imaginary) (prn (str real "+" imaginary "i"))
                    (-> real nil? not) (prn (str real))
                    (-> imaginary nil? not) (prn (str imaginary "i"))))))

(defn init []
  (let [canvas (js/document.getElementById "canvas")
        ctx (.. canvas (getContext "2d"))
        
        width 200 ;;js/window.innerWidth
        height 200 ;;js/window.innerHeight
        grid (generate-grid width height)]
    (prn "width=" width)
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)

    ;;(set! (.-fillStyle ctx) "rgb(155,0,0)")
    (plot-mandelbrot ctx grid)
    ;;(.beginPath ctx)


    ;;(.fill ctx)


    ))


;;(init)



(comment
  
  (def g (generate-grid 100 100))

  (def foo (filter (fn [[r c [x y]]]
                     (or (> x 1) (> y 1)))
                   g))
  
  (doseq [r g]
    (doseq [[row col [x y]] r]
        (when (or (>= x 1)
                (>= y 1))
        (prn r))))
  
  (def r0 (first g))
  (def rn (last g))

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
