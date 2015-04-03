#lang sweet-exp racket

require (except-in 2htdp/universe space)
        2htdp/image
        2htdp/planetcute
        (only-in pict pict->bitmap)
        pict/face
        math/base
        sweet-exp-utils/def
        my-cond/iffy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants:

def flappy-bird-image =
  bitmap/file("Flappy-Bird.png")
  ;freeze(pict->bitmap(face('happy "yellow")))

def v-flap = -20 ; in pixels/tick

def g = 2.2 ; in pixels/tick^2

def scene.width = 800 ; in pixels
def scene.height = 600 ; in pixels

def scene.ctr-x = {1/2 * scene.width}
def scene.ctr-y = {1/2 * scene.height}

def speed = -10 ; the speed in pixels/tick of the pipes relative to the screen

def space-between-pipes = 200

def space.height = 200 ; in pixels

def pipe.width = 75 ; in pixels

def transparent =
  color(255 255 255 0)

def transparent-scene =
  rectangle(scene.width scene.height 0 transparent)

def sky =
  rectangle(scene.width scene.height "solid" "DeepSkyBlue")

def grass =
  begin
    def grass-block.width = image-width(grass-block)
    def grass-block.height = image-height(grass-block)
    def grass.new-height = {grass-block.height - 40}
    def grass.new-width = {grass-block.width - 10}
    def grass-img =
      crop 5 0 grass.new-width grass.new-height
           grass-block
    def n =
      exact-ceiling{scene.width / grass.new-width}
  apply(beside make-list(n grass-img))

def scene =
  underlay/align "middle" "bottom"
    sky
    grass

def much-grass =
  apply beside make-list(4 grass)

def space =
  rectangle(pipe.width space.height 0 transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions:

;; World is new(world%)
def world% =
  class object% super-new() inspect(#f)
    init-field [flappy-bird new(flappy-bird%)]
               [pipes '()]
               [score 0]
               [game-over? #f]
    public tick! maybe-fly! inc-score! collision? game-over! reset! draw draw-scene get-flappy-bird get-game-over? get-score maybe-add-new-pipe! maybe-remove-last-pipe! add-new-pipe! remove-last-pipe! 
    ;
    def tick!() =
      for ([pipe in-list(pipes)])
        send* pipe tick-x!()
      maybe-add-new-pipe!()
      maybe-fly!()
    ;
    def maybe-fly!() =
      my-cond
        if collision?()
          game-over!()
        else
          send* flappy-bird fly!()
    ;
    def inc-score!() =
      set! score += 1
    ;
    def collision?() =
      def flappy-bird.x = (send* flappy-bird get-x())
      def flappy-bird.y = (send* flappy-bird get-y())
      def flappy-bird.img = (send* flappy-bird get-img())
      def flappy-bird.height = image-height(flappy-bird.img)
      def flappy-bird.width = image-width(flappy-bird.img)
      for/or ([pipe in-list(pipes)])
        def pipe.x = (send* pipe get-x())
        def pipe.y = (send* pipe get-y())
        {{{pipe.x - {1/2 * pipe.width} - {1/3 * flappy-bird.width}}
          <= flappy-bird.x
          <= {pipe.x + {1/2 * pipe.width} + {1/3 * flappy-bird.width}}}
         and
         not{{pipe.y - {1/2 * space.height} - {-1/3 * flappy-bird.height}}
             <= flappy-bird.y
             <= {pipe.y + {1/2 * space.height} + {-1/3 * flappy-bird.height}}}}
    ;
    def game-over!() =
      set! game-over? = #t
    ;
    def reset!() =
      set! flappy-bird = new(flappy-bird%)
      set! pipes = '()
      set! score = 0
      set! game-over? = #f
    ;
    def draw() =
      overlay/align "left" "top"
        text( ~v(score) 36 "yellow")
        apply underlay
          draw-scene()
          send* flappy-bird draw()
          for/list ([pipe in-list(pipes)])
            send* pipe draw()
    ;
    def draw-scene() =
      def grass.x =
        my-cond
          if empty?(pipes)
            scene.width
          else
            def first-pipe = first(pipes)
            send* first-pipe get-x()
      place-image/align(much-grass
                        grass.x scene.height
                        "middle" "bottom"
                        sky)
    ;
    def get-flappy-bird() = flappy-bird
    ;
    def get-game-over?() = game-over?
    ;
    def get-score() = score
    ;
    def maybe-add-new-pipe!() =
      when not(empty?(pipes))
        def first-pipe = first(pipes)
        def first-pipe.x = (send* first-pipe get-x())
        when {{first-pipe.x + space-between-pipes + pipe.width + -400} <= scene.width}
          add-new-pipe!()
      when empty?(pipes)
        add-new-pipe!()
    ;
    def maybe-remove-last-pipe!() =
      when not(empty?(pipes))
        def last-pipe = last(pipes)
        def last-pipe.x = (send* last-pipe get-x())
        when {last-pipe.x <= 0}
          remove-last-pipe!()
    ;
    def add-new-pipe!() =
      set! pipes = cons(new(pipe%) pipes)
      maybe-remove-last-pipe!()
    ;
    def remove-last-pipe!() =
      set! pipes = drop-right(pipes 1)

def world? = is-a?/c(world%)

;; FlappyBird is new(flappy-bird%)
;; a bird, with an x-position x, a y-position y, a y-velocity v, and an image img.
def flappy-bird% =
  class object% super-new() inspect(#f)
    init-field [x scene.ctr-x]
               [y scene.ctr-y]
               [v 0]
               [img flappy-bird-image]
    ;
    public fly! flap! above-ground? draw get-x get-y get-img
    ;
    def fly!() =
      when above-ground?()
        def ∆t = 1
        set! v += {g * ∆t}
        set! y += {v * ∆t}
    ;
    def flap!() =
      def ∆t = 1
      set! v = v-flap
      set! y += {v * ∆t}
    ;
    def above-ground?() =
      def img.height = image-height(img)
      def ground.height = scene.height
      def max-y = {ground.height + {-1/2 * img.height}}
      {y < max-y}
    ;
    def draw() =
      place-image(img x y transparent-scene)
    ;
    def get-x() = x
    def get-y() = y
    def get-img() = img

def flappy-bird? = is-a?/c(flappy-bird%)

;; Pipe is new(pipe%)
def pipe% =
  class object% super-new() inspect(#f)
    init-field [x {scene.width + pipe.width + 400}]
               [y random-integer(space.height {scene.height - space.height})]
               [img make-pipe-image(#:y y)]
    public tick-x! draw get-x get-y get-img
    ;
    def tick-x!() =
      def ∆t = 1
      when {{{scene.ctr-x + {speed * ∆t}} < x} and {x <= scene.ctr-x}}
        send* world inc-score!()
      set! x += speed
    ;
    def draw() =
      place-image(img x scene.ctr-y transparent-scene)
    ;
    def get-x() = x
    def get-y() = y
    def get-img() = img


def world = new(world%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions:

def main(world) =
  big-bang world
    [on-tick tick]
    [on-key handle-key]
    [to-draw draw]

def tick(world) =
  when not(game-over?(world))
    send* world tick!()
  world

define handle-key(world key)
  def flappy-bird = (send* world get-flappy-bird())
  when not(game-over?(world))
    send* flappy-bird flap!()
  when {game-over?(world) and {key=?(key "r") or key=?(key "R")}}
    send* world reset!()
  world

def draw(world) =
  my-cond
    if game-over?(world)
      draw-last-scene(world)
    else
      send* world draw()

def game-over?(world) =
  send* world get-game-over?()

def draw-last-scene(world) =
  underlay
    send* world draw()
    above
      text("Game Over" 36 "black")
      beside
        text("score: " 36 "black")
        text( (~v (send* world get-score())) 36 "black")
      text("press R to restart" 36 "black")

def make-pipe-image(#:y y) =
  def upper-pipe =
    rectangle(pipe.width max(0 {y + {-1/2 * space.height}})
              "solid" "Green")
  def lower-pipe =
    rectangle(pipe.width max(0 {scene.height - y - {1/2 * space.height}})
              "solid" "Green")
  above
    upper-pipe
    space
    lower-pipe

void main(world)
