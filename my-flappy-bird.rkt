#lang racket

(require (except-in 2htdp/universe space)
         2htdp/image
         2htdp/planetcute
         (only-in pict pict->bitmap)
         pict/face
         math/base
         sweet-exp-utils/def
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants:

(define flappy-bird-image
  (bitmap/file "Flappy-Bird.png")
  ;(freeze (pict->bitmap (face 'happy "yellow")))
  )

(define v-flap -20) ; in pixels/tick

(define g 2.2) ; in pixels/tick^2

(define scene.width 800) ; in pixels
(define scene.height 600) ; in pixels

(define scene.ctr-x (* 1/2 scene.width))
(define scene.ctr-y (* 1/2 scene.height))

(define speed -10) ; the speed in pixels/tick of the pipes relative to the screen

(define space-between-pipes 200)

(define space.height 200) ; in pixels

(define pipe.width 75) ; in pixels

(define transparent
  (color 255 255 255 0))

(define transparent-scene
  (rectangle scene.width scene.height
             0 transparent))

(define sky
  (rectangle scene.width scene.height
             "solid" "DeepSkyBlue"))

(define grass
  (local [(define grass-block.width (image-width grass-block))
          (define grass-block.height (image-height grass-block))
          (define grass.new-height (- grass-block.height 40))
          (define grass.new-width (- grass-block.width 10))
          (define grass-img (crop 5 0
                                  grass.new-width grass.new-height
                                  grass-block))
          (define n
            (exact-ceiling (/ scene.width grass.new-width)))]
    (apply beside (make-list n grass-img))))

(define scene
  (underlay/align
   "middle" "bottom"
   sky grass))

(define space
  (rectangle pipe.width space.height
             0 transparent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions:

;; World is (new world%)
(define world%
  (class object% (super-new) (inspect #f)
    (init-field [flappy-bird (new flappy-bird%)]
                [pipes '()]
                [score 0]
                [game-over? #f])
    (public tick! maybe-fly! inc-score! collision? game-over! reset! draw draw-scene get-flappy-bird get-game-over? get-score maybe-add-new-pipe! maybe-remove-last-pipe! add-new-pipe! remove-last-pipe!) 
    
    (define (tick!)
      (for ([pipe (in-list pipes)])
        (send pipe tick-x!))
      (maybe-add-new-pipe!)
      (maybe-fly!))
    
    (define (maybe-fly!)
      (cond ;[(not (send flappy-bird above-ground?)) (send flappy-bird fly!)]
            [(collision?) (game-over!)]
            [else (send flappy-bird fly!)]))
    
    (define (inc-score!)
      (set! score += 1))
    
    (define (collision?)
      (let* ([flappy-bird.x (send flappy-bird get-x)]
             [flappy-bird.y (send flappy-bird get-y)]
             [flappy-bird.img (send flappy-bird get-img)]
             [flappy-bird.height (image-height flappy-bird.img)]
             [flappy-bird.width (image-width flappy-bird.img)])
        (for/or ([pipe (in-list pipes)])
          (let ([pipe.x (send pipe get-x)]
                [pipe.y (send pipe get-y)])
            (and (<= (- pipe.x (* 1/2 pipe.width) (* 1/3 flappy-bird.width))
                     flappy-bird.x
                     (+ pipe.x (* 1/2 pipe.width) (* 1/3 flappy-bird.width)))
                 (not (<= (- pipe.y (* 1/2 space.height) (* -1/3 flappy-bird.height))
                          flappy-bird.y
                          (+ pipe.y (* 1/2 space.height) (* -1/3 flappy-bird.height)))))))))
    
    (define (game-over!)
      (set! game-over? = #t))
    
    (define (reset!)
      (set! flappy-bird = (new flappy-bird%))
      (set! pipes = '())
      (set! score = 0)
      (set! game-over? = #f))
    
    (define (draw)
      (overlay/align
       "left" "top"
       (text (~v score) 36 "yellow")
       (apply underlay
              (draw-scene)
              (send flappy-bird draw)
              (for/list ([pipe (in-list pipes)])
                (send pipe draw)
                ))))
    
    (define (draw-scene)
      (let ([grass.x (cond [(empty? pipes) scene.width]
                           [else (let ([first-pipe (first pipes)])
                                   (send first-pipe get-x))])])
        (place-image/align (beside grass grass grass grass)
                           grass.x scene.height
                           "middle" "bottom"
                           sky
                           )))
    
    (define (get-flappy-bird) flappy-bird)
    
    (define (get-game-over?) game-over?)
    
    (define (get-score) score)
    
    (define (maybe-add-new-pipe!)
      (when (not (empty? pipes))
        (let* ([first-pipe (first pipes)] [first-pipe.x (send first-pipe get-x)])
          (when (<= (+ first-pipe.x space-between-pipes pipe.width -400) scene.width)
            (add-new-pipe!))))
      (when (empty? pipes)
        (add-new-pipe!)))
    
    (define (maybe-remove-last-pipe!)
      (when (not (empty? pipes))
        (let* ([last-pipe (last pipes)] [last-pipe.x (send last-pipe get-x)])
          (when (<= last-pipe.x 0)
            (remove-last-pipe!)))))
    
    (define (add-new-pipe!)
      (set! pipes = (cons (new pipe%) pipes))
      (maybe-remove-last-pipe!))
    
    (define (remove-last-pipe!)
      (set! pipes = (drop-right pipes 1)))
    ))
(define world? (is-a?/c world%))

;; FlappyBird is (new flappy-bird%)
;; a bird, with an x-position x, a y-position y, a y-velocity v, and an image img.
(define flappy-bird%
  (class object% (super-new) (inspect #f)
    (init-field [x scene.ctr-x]
                [y scene.ctr-y]
                [v 0]
                [img flappy-bird-image])
    
    (public fly! flap! above-ground? draw get-x get-y get-img)
    
    (define (fly!)
      (when (above-ground?)
        (local [(define ∆t 1)]
          (set! v += (* g ∆t))
          (set! y += (* v ∆t)))))
    
    (define (flap!)
      (local [(define ∆t 1)]
        (set! v = v-flap)
        (set! y += (* v ∆t))))
    
    (define (above-ground?)
      (let* ([img.height (image-height img)]
             [ground.height scene.height]
             [max-y (+ ground.height (* -1/2 img.height))])
        (< y max-y)))
    
    (define (draw)
      (place-image img
                   x y
                   transparent-scene))
    
    (define (get-x) x)
    (define (get-y) y)
    (define (get-img) img)
    ))
(define flappy-bird? (is-a?/c flappy-bird%))

;; Pipe is (new pipe%)
(define pipe%
  (class object% (super-new) (inspect #f)
    (init-field [x (+ scene.width pipe.width 400)]
                [y (random-integer space.height (- scene.height space.height))]
                [img (make-pipe-image #:y y)])
    (public tick-x! draw get-x get-y get-img)
    
    (define (tick-x!)
      (local [(define ∆t 1)]
        (when (and (< (+ scene.ctr-x (* speed ∆t)) x) (<= x scene.ctr-x))
          (send world inc-score!))
        (set! x += speed)))
    
    (define (draw)
      (place-image img
                   x scene.ctr-y
                   transparent-scene))
    
    (define (get-x) x)
    (define (get-y) y)
    (define (get-img) img)
    ))

(define world (new world%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions:

(define (main world)
  (big-bang world
            [on-tick tick]
            [on-key handle-key]
            [to-draw draw]
            ))

(define (tick world)
  (when (not (game-over? world))
    (send world tick!))
  world)

(define (handle-key world key)
  (let ([flappy-bird (send world get-flappy-bird)])
    (when (not (game-over? world))
      (send flappy-bird flap!))
    (when (and (game-over? world) (or (key=? key "r") (key=? key "R")))
      (send world reset!))
    world))

(define (draw world)
  (if (game-over? world)
      (draw-last-scene world)
      (send world draw)))

(define (game-over? world)
  (send world get-game-over?))

(define (draw-last-scene world)
  (underlay (send world draw)
            (above (text "Game Over" 36 "black")
                   (beside (text "score: " 36 "black") (text (~v (send world get-score)) 36 "black"))
                   (text "press R to restart" 36 "black"))))

(define (make-pipe-image #:y y)
      (local [(define upper-pipe
                (rectangle pipe.width (max 0 (+ y (* -1/2 space.height)))
                           "solid" "Green"))
              (define lower-pipe
                (rectangle pipe.width (max 0 (- scene.height y (* 1/2 space.height)))
                           "solid" "Green"))]
        (above upper-pipe
               space
               lower-pipe)
        ))

(void (main world))