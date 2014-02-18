;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem set 8 - missile defense|) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp")))))
(require 2htdp/image)
(require 2htdp/universe)

; Missile Defense


(define SCENE-WIDTH 500) ; width of scene in pixels
(define SCENE-HEIGHT 500) ; height of scene in pixels
(define TICK-RATE .01) ; .2 seconds/tick
(define SCENE (overlay
               (square (- SCENE-WIDTH 10) 'solid 'black)
               (square SCENE-WIDTH 'solid 'red)))
(define HEALTH 10) ; Initial health starting the game
(define (HEALTH-IMAGE w)
  (text (string-append "Health: "
                       (number->string(world-health w)))
        15 "red")) ; Image health
(define B-COLOR "green") ; Bullet Color
(define M-COLOR "blue") ; Missile Color

(define B-SIZE 5) ; Bullet size
(define M-SIZE 10) ; Missile size
(define SILO-SIZE 12) ; Silo Size
(define SILO-POSN (make-posn 250 (- 500 (/ SILO-SIZE 2))))
(define SILO (square SILO-SIZE "solid" B-COLOR)) ; image silo
(define RANDOM-TOP
  (make-posn (random 500) 0)) ; random start coordinate for missile
(define RANDOM-BOTTOM
  (make-posn (random 500) 500)) ; random bottom coordinate for missile
(define counter 0)

(define EASY 100) ; Easy setting for probability of missile launch
(define MEDIUM 50) ; Medium slightly more frequent probability of missile launch
(define HARD 25) ; Hard frequent possibility of missile launch




;;; A Sprite is a (make-sprite Posn Posn Number String)
;;; where
;;; - LOC is the sprite's location
;;; - VEL is its velocity
;;; - SIZE is the radius of the sprite
;;; - COLOR is its color.
;;; Location, velocity and size are in computer-graphic/pixel coordinates.
;;; A sprite represents either an attacker's missile or a defender's
;;; anti-missile bullet.
(define-struct sprite (loc vel size color))


(define sprite1 (make-sprite (make-posn 250 250)
                             (make-posn .70710 .70710) B-SIZE B-COLOR))
(define sprite2 (make-sprite (make-posn 0 250)
                             (make-posn .70710 .70710) B-SIZE B-COLOR))
(define sprite3 (make-sprite (make-posn 250 0)
                             (make-posn .70710 .70710) B-SIZE B-COLOR))
(define sprite4 (make-sprite (make-posn 50 400)
                             (make-posn .70710 .70710) B-SIZE B-COLOR))
(define sprite5 (make-sprite (make-posn 200 305)
                             (make-posn .70710 .70710) B-SIZE B-COLOR))
(define sprite6 (make-sprite (make-posn 200 -250)
                             (make-posn .70710 .70710) M-SIZE M-COLOR))
(define sprite7 (make-sprite (make-posn 800 200)
                             (make-posn .70710 .70710) M-SIZE M-COLOR))
(define sprite8 (make-sprite (make-posn 100 400)
                             (make-posn .70710 .70710) M-SIZE M-COLOR))
(define sprite9 (make-sprite (make-posn 400 100)
                             (make-posn .70710 .70710) M-SIZE M-COLOR))
(define sprite10 (make-sprite (make-posn 200 300)
                              (make-posn .70710 .70710) M-SIZE M-COLOR))
(define sprite11 (make-sprite (make-posn 250 500)
                              (make-posn .3511 -.9363) B-SIZE B-COLOR))
(define sprite12 (make-sprite (make-posn 200 300)
                              (make-posn .3714 .9285) M-SIZE M-COLOR))

; sprite-image: Sprite -> Image
; Consumes a size and color and returns image of sprite
(check-expect (sprite-image sprite1) (circle B-SIZE "solid" B-COLOR))

(define (sprite-image s)
  (circle (sprite-size s) "solid" (sprite-color s)))

;;; A LOS (list of sprites) is one of:
;;; - empty
;;; - (cons Sprite LOS)

(define l-sprite1 (list sprite1 sprite2 sprite3 sprite4 sprite5 sprite6
                        sprite7 sprite8 sprite9 sprite10))
(define l-sprite2 (list sprite1 sprite3 sprite5
                        sprite7  sprite9 ))
(define l-sprite3 (list sprite1 sprite2 sprite3 sprite4 sprite5))
(define l-sprite4 (list sprite6 sprite7 sprite8 sprite9 sprite10))
(define l-sprite5 (list sprite1 sprite2))
(define l-spriteoff (list sprite1 sprite6))

;;; A world structure is a (make-world LOS LOS Number)
;;; - missiles: the missiles attacking the player
;;; - bullets: the missiles launched by the player
;;; - health: current health of the player -- game-over if health <= 0

(define-struct world (missiles bullets health))
(define world0 (make-world empty empty HEALTH))
(define world1 (make-world l-sprite4 empty HEALTH))
(define world2 (make-world (list sprite12) empty HEALTH))


;; place-start-image: World -> Image
;; displays the bullet-silo at the bottom of the screen
(check-expect (place-start-image world0)
              (place-image SILO 250 494
                           (place-image
                            (text (string-append "Health: "
                                                 (number->string
                                                  (world-health world0)))
                                  15 "red")
                            450 485
                            SCENE)))
(define (place-start-image w)
  (place-image
   SILO
   (/ SCENE-WIDTH 2)
   (- SCENE-HEIGHT (/ SILO-SIZE 2))
   (place-image
    (HEALTH-IMAGE w)
    450 485
    (image-help (append (world-missiles w)
                        (world-bullets w))))))

;; image-help: list -> image
;; Cosume a list of blocks and create an image
(check-expect (image-help empty) SCENE)
(check-expect (image-help l-sprite5)
              (place-image
               (circle B-SIZE "solid" B-COLOR)
               250 250
               (place-image
                (circle B-SIZE "solid" B-COLOR)
                0 250
                SCENE)))

(define (image-help l)
  (cond
    [(empty? l) SCENE]
    [else
     (place-image
      (sprite-image (first l))
      (posn-x (sprite-loc (first l)))
      (posn-y (sprite-loc (first l)))
      (image-help (rest l)))]))

;;; move-sprites: LOS -> World
;;; Move the sprites one time step and remove any that have gone off screen.
(check-expect (move-sprites l-sprite3)
              (list (make-sprite (make-posn 250.70710 250.70710)
                                 (make-posn .70710 .70710)
                                 B-SIZE B-COLOR)
                    (make-sprite (make-posn .70710 250.70710)
                                 (make-posn .70710 .70710)
                                 B-SIZE B-COLOR)
                    (make-sprite (make-posn 250.70710 .70710)
                                 (make-posn .70710 .70710)
                                 B-SIZE B-COLOR)
                    (make-sprite (make-posn 50.70710 400.70710)
                                 (make-posn .70710 .70710)
                                 B-SIZE B-COLOR)
                    (make-sprite (make-posn 200.70710 305.70710)
                                 (make-posn .70710 .70710)
                                 B-SIZE B-COLOR)))
(define (move-sprites l)
  (map move-sprite l))

;;; move-sprite-missile: Sprite -> Sprite
;;; Move the missiles one time step.
(check-expect (move-sprite sprite1)
              (make-sprite (make-posn 250.70710 250.70710)
                           (make-posn .70710 .70710) B-SIZE B-COLOR))
(define (move-sprite s)
  (local [(define loc (sprite-loc s))
          (define vel (sprite-vel s))]
    (make-sprite (make-posn
                  (+ (posn-x loc)
                     (posn-x vel))
                  (+ (posn-y loc)
                     (posn-y vel)))
                 vel
                 (sprite-size s)
                 (sprite-color s))))

;; generate-missile: World Number -> World
;; Consumes a world and then returns a world with a new missile in it
;; base on the level of difficulty

(define (generate-missile l x)
  (if (= (random x) 3)
      (cons (r-missile (make-sprite (make-posn (random 500) 0)
                                    (make-posn (random 500) 0)
                                    M-SIZE
                                    M-COLOR)) l)
      l))


;; r-missile: Sprite -> Sprite
;; Consumes a random sprite and returns sprite with correct velocity posn
;; can't perform check-expect because values constantly change because of random

(define (r-missile s)
  (make-sprite (sprite-loc s)
               (velocity (sprite-loc s) (make-posn (random 500) 500))
               (sprite-size s)
               (sprite-color s)))

;; velocity: Posn Posn -> Posn
;; Consumes a location posn and a destination posn and gives 1 pixel step
;; posn to get there
(check-expect (velocity (sprite-loc sprite1) SILO-POSN) (make-posn 0 1))

(define (velocity start end)
  (make-posn
   (/ (round (* (/ (- (posn-x end) (posn-x start)) (distance start end)) 100))
      100)
   (/ (round (* (/ (- (posn-y end) (posn-y start)) (distance start end)) 100))
      100)))

;; distance: Posn Posn -> Number
;; calculates the distance between two positions
(check-expect (distance (sprite-loc sprite1) SILO-POSN) 244)

(define (distance start end)
  (/ (round (* (inexact->exact
                (sqrt (+ (sqr (- (posn-x end) (posn-x start)))
                         (sqr (- (posn-y end) (posn-y start))))))
               100))
     100))


;; generate-bullet: World Number Number -> World
;; Consumes a world and then returns a world with a new bullet in it
(check-expect (generate-bullet l-sprite5 270 280)
              (list
               (make-sprite
                (make-posn 250 494)
                (make-posn 9/100 -1)
                5
                "green")
               (make-sprite
                (make-posn 250 250)
                (make-posn 7071/10000 7071/10000)
                5
                "green")
               (make-sprite
                (make-posn 0 250)
                (make-posn 7071/10000 7071/10000)
                5
                "green")))
(define (generate-bullet l x y)
  (if (< (length l) 10)
      (cons (make-sprite SILO-POSN
                         (velocity SILO-POSN (make-posn x y))
                         B-SIZE
                         B-COLOR) l)
      l))


;; collision?: Sprite LOS -> Boolean
;; are the bullets and  missiles colliding?
(check-expect (collision? sprite1 l-sprite1) true)
(check-expect (collision? sprite1 empty) false)

(define (collision? s l)
  (ormap (lambda (list-s) (< (distance (sprite-loc s) (sprite-loc list-s))
                             (+ B-SIZE M-SIZE)))
         l))

;;; remove-dead-sprites: World -> World
;;; Remove every missile that is touching some bullet.
(check-expect (remove-dead-sprites l-sprite1 l-sprite2)
              (list
               (make-sprite
                (make-posn 0 250)
                (make-posn 7071/10000 7071/10000)
                5
                "green")
               (make-sprite
                (make-posn 50 400)
                (make-posn 7071/10000 7071/10000)
                5
                "green")
               (make-sprite
                (make-posn 200 -250)
                (make-posn 7071/10000 7071/10000)
                10
                "blue")
               (make-sprite
                (make-posn 100 400)
                (make-posn 7071/10000 7071/10000)
                10
                "blue")))
(define (remove-dead-sprites l1 l2)
  (filter (lambda (s) (not (collision? s l2))) l1))

;; off-screen? s -> boolean
;; Returns true or false if sprite is off-screen
(check-expect (off-screen? sprite1) false)
(check-expect (off-screen? sprite6) true)

(define (off-screen? s)
  (or (< (posn-x (sprite-loc s)) 0)
      (> (posn-x (sprite-loc s)) SCENE-WIDTH)
      (< (posn-y (sprite-loc s)) 0)
      (> (posn-y (sprite-loc s)) SCENE-HEIGHT)))

;; remove-off: List -> List
;; checks list for offscreen
(check-expect (remove-off l-spriteoff)
              (list
               (make-sprite
                (make-posn 250 250)
                (make-posn 7071/10000 7071/10000)
                5
                "green")))

(define (remove-off l)
  (filter  (lambda (s) (not (off-screen? s))) l))

;; world->scene w : World -> Image
;; consumes world and returns image of the world and
;; display "GAME OVER" is run out of health

(define (world->scene w)
  (if (game-over (world-health w))
      (place-image (text "GAME OVER" 75 "red") 250 250 SCENE)
      (place-start-image  w)))

; handle-mouse: World Number Number String
; Creates world based on mouse click
(check-expect (handle-mouse world0 270 280 "button-down")
              (make-world
               empty
               (list
                (make-sprite
                 (make-posn 250 494)
                 (make-posn 9/100 -1)
                 5
                 "green"))
               10))
(check-expect (handle-mouse world1 270 280 "release") world1)

(define (handle-mouse w x y me)
  (cond [(string=? me "button-down")
         (make-world
          (world-missiles w)
          (generate-bullet (world-bullets w) x y)
          (world-health w))]
        [else w]))

; remove-health: LOS Number -> Number
; retunrs the number of lives left
(check-expect (remove-health l-sprite1 5) 4)
(check-expect (remove-health l-sprite5 5) 5)

(define (remove-health l h)
  (cond [(ormap off-screen?  l) 
         (sub1 h)]
        [else h]))

;; game-over: Number -> Boolean
;; does the player run out of lives?
(check-expect (game-over 0) true)
(check-expect (game-over (remove-health l-sprite1 5)) false)

(define (game-over h)
  (<= h 0))

;; update-world: World -> World
;; Move the bullets and remove offscreen ones.
;; Move the missiles.
;; Kill any destroyed missiles.
;; Detonate the missiles that landed.
;; Maybe launch another missile.

(define (update-world w)
  (make-world
   (move-sprites (remove-dead-sprites
                  (remove-off (generate-missile (world-missiles w) EASY))
                  (world-bullets w)))
   (move-sprites (remove-dead-sprites
                  (remove-off (world-bullets w))
                  (world-missiles w)))
   (remove-health (world-missiles w) (world-health w))))



(big-bang world0
          (on-tick update-world TICK-RATE)
          (to-draw world->scene)
          (on-mouse handle-mouse))