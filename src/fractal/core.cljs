(ns fractal.core)

(enable-console-print!)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(defn add [a b]
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

(defn subtract [a b]
  (add a (multiply-i -1 b)))

(defn square-i [a]
  (multiply-i a a))

(defn square [a]
  (multiply a a))

(defn distance [[a b] [c d]]
  (let [[real imaginary] (add (square (subtract [c 0] [a 0]))
                              (square (subtract [0 d] [0 b])))]
    (Math/sqrt real)))

(defn generate-grid [width height]
  (let [delta-x (/ 1 width)
        delta-y (/ 1 height)
        x (atom -1.5)
        y (atom 1)]
    (for [i (range width)
          :let [_ (swap! x (fn [x] (+ x delta-x)))]]
      (for [j (range height)
            :let [_ (swap! y (fn [y] (- y delta-y)))]]
        [i j [@x @y]]))))

(defn plot-mandelbrot [ctx grid]
  (doseq [row grid]
    (doseq [ [r c [x y]] row]
      (.fillRect ctx r c 1 1))))

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
        
        width js/window.innerWidth
        height js/window.innerHeight
        grid (generate-grid width height)]
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)

    (set! (.-fillStyle ctx) "rgb(255,0,0)")
    (plot-mandelbrot ctx grid)
    ;;(.beginPath ctx)


    ;;(.fill ctx)


    ))


(init)
(comment
  
  (def g (generate-grid 3 3))

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
