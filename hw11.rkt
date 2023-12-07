;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; To run the program, uncomment the last line of the file.

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIPES

(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.

(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-CR (make-pipe #true #true #true #true))

(define STARTING-PIPE-T (make-pipe #true #false #false #false))
(define STARTING-PIPE-B (make-pipe #false #true #false #false))
(define STARTING-PIPE-L (make-pipe #false #false #true #false))
(define STARTING-PIPE-R (make-pipe #false #false #false #true))

(define (pipe-template p)
  (... (pipe-top p)
   ... (pipe-bot p)
   ... (pipe-left p)
   ... (pipe-right p) ...))

;; A StartingPipe is one of:
;; - STARTING-PIPE-T
;; - STARTING-PIPE-B
;; - STARTING-PIPE-L
;; - STARTING-PIPE-R

;; pipe list
(define ALL-PIPES (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TB PIPE-LR PIPE-CR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRECTIONS

;; A Direction is one of:
;; - "right"
;; - "down"
;; - "left"
;; - "up"
 
(define RIGHT "right")
(define DOWN "down")
(define LEFT "left")
(define UP "up")

(define (direction-template d)
  (cond
    [(string=? "right") RIGHT]
    [(string=? "down") DOWN]
    [(string=? "left") LEFT]
    [(string=? "up") UP]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIPE->IMAGE FUNCTION

;; pipe->image: Pipe Integer Integer Boolean Direction Boolean -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.
;; If filled? then draw the pipe with goo.

(define (pipe->image pipe tile-side-length pipe-width filled? dir cr-color?)
  (local (;; top pipe color for cross pipes
          [define PIPE-COLOR
            (if filled? "light green" "red")]
          ;; bottom pipe color for cross pipes
          [define PIPE-COLOR-2
            (if cr-color? "light green" "red")]
          ;; background tile for pipe images
          [define TILE (square tile-side-length "solid" "pink")]
          ;; cross-pipe : Integer Integer -> Image
          ;; produces an image of a cross pipe
          [define (cross-pipe w1 w2)
            (overlay
             (rectangle w1 w2 "solid" PIPE-COLOR)
             (rectangle w2 w1 "solid" PIPE-COLOR-2)
             TILE)]
          ;; straight-pipe : Integer Integer -> Image
          ;; produces an image of a straight pipe
          [define (straight-pipe w1 w2)
            (overlay
             (rectangle w1 w2 "solid" PIPE-COLOR)
             TILE)]
          ;; corner-pipe : String String -> Image
          ;; produces an image of a corner pipe
          [define (corner-pipe d1 d2)
            (overlay/align d1 "middle"
                    (rectangle (/ tile-side-length 2) pipe-width "solid" PIPE-COLOR)
                    (overlay/align "middle" d2
                                   (rectangle pipe-width (+ (/ tile-side-length 2)
                                                            (/ pipe-width 2))
                                              "solid"
                                              PIPE-COLOR)
                                   TILE))]
          ;; starting-pipe : String String Integer Integer -> Image
          ;; produces an image of a starting pipe
          [define (starting-pipe d1 d2 w1 w2)
            (overlay/align d1 d2
                    (rectangle w1 w2 "solid" PIPE-COLOR)
                    TILE)])
  (cond
    [(and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe))
     (if (or (string=? dir RIGHT) (string=? dir LEFT))
     (cross-pipe tile-side-length pipe-width)
     (cross-pipe pipe-width tile-side-length))] 
    [(and (pipe-top pipe) (pipe-bot pipe)) (straight-pipe pipe-width tile-side-length)]
    [(and (pipe-left pipe) (pipe-right pipe)) (straight-pipe tile-side-length pipe-width)]
    [(and (pipe-left pipe) (pipe-top pipe)) (corner-pipe "left" "top")]
    [(and (pipe-right pipe) (pipe-top pipe)) (corner-pipe "right" "top")]
    [(and (pipe-left pipe) (pipe-bot pipe)) (corner-pipe "left" "bottom")]
    [(and (pipe-right pipe) (pipe-bot pipe)) (corner-pipe "right" "bottom")]
    [(pipe-top pipe) (starting-pipe "middle" "top" pipe-width (/ tile-side-length 2))]
    [(pipe-bot pipe) (starting-pipe "middle" "bottom" pipe-width (/ tile-side-length 2))]
    [(pipe-left pipe) (starting-pipe "left" "middle" (/ tile-side-length 2) pipe-width)]
    [(pipe-right pipe) (starting-pipe "right" "middle" (/ tile-side-length 2) pipe-width)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRIDPIPE DEFINITION

(define-struct gp [pipe row col])
;; A GridPipe is a (make-gp Pipe Number Number) where
;; - pipe is the type of pipe
;; - row is the row that the pipe is on
;; - col is the column that the pipe is on

(define GRID-PIPE-EX-0 (make-gp STARTING-PIPE-R 0 0))
(define GRID-PIPE-EX-1 (make-gp PIPE-CR 5 6))
(define GRID-PIPE-EX-2 (make-gp PIPE-BL 3 4))

(define (gp-template g)
  (... (pipe-template (gp-pipe g)) ...
       (gp-row g) ...
       (gp-col g) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRID DEFINITION

(define-struct grid [row col grid-pipes])
;; A Grid is a (make-grid Number Number [List-of GridPipe]) where
;; - row is the number of rows in the grid
;; - col is the number of columns in the grid
;; - grid-pipes is the list of all pipes being represented on the grid

(define STARTING-GRID (make-grid 7 7 '()))
(define GRID-EX-0 (make-grid 3 4 (list GRID-PIPE-EX-0)))
(define GRID-EX-1 (make-grid 7 7 (list GRID-PIPE-EX-0 GRID-PIPE-EX-1 GRID-PIPE-EX-2)))

(define (grid-template g)
  (... (grid-row g) ...
       (grid-col g) ...
       (cond
         [(empty? (grid-grid-pipes g)) ...]
         [(cons? (grid-grid-pipes g)) (... (gp-template g) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GOOFLOW DEFINITION

(define-struct gooflow [goo-path])
;; A GooFlow is a (make-gooflow [NE-List-of [Any-of #false [List-of GridPipe Direction]])
;; goo-path is a list of the GridPipes that have goo gone through them
;; and the Direction of the goo through each GridPipe

(define GOOFLOW-EX-0 (make-gooflow (list (list (make-gp STARTING-PIPE-R 0 0) RIGHT))))
(define GOOFLOW-EX-1 (make-gooflow (list (list (make-gp PIPE-BL 0 1) DOWN)
                                         (list (make-gp STARTING-PIPE-R 0 0) RIGHT))))
(define GOOFLOW-EX-2 (make-gooflow (list (list (make-gp PIPE-TR 1 1) RIGHT)
                                         (list (make-gp PIPE-BL 0 1) DOWN)
                                         (list (make-gp STARTING-PIPE-R 0 0) RIGHT))))

(define (gooflow-template gf)
  (cond
    [(empty? (gooflow-goo-path gf)) ...]
    [(cons? (gooflow-goo-path gf)) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIPE/GRID FUNCTIONS

;; place-pipe: Grid Pipe Integer Integer GooFlow -> [Optional Grid]
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.

(check-expect (place-pipe STARTING-GRID PIPE-TB 3 4 GOOFLOW-EX-0)
              (make-grid 7 7 (list (make-gp PIPE-TB 3 4))))
(check-expect (place-pipe GRID-EX-0 PIPE-CR 1 2 GOOFLOW-EX-0)
              (make-grid 3 4 (list (make-gp PIPE-CR 1 2) GRID-PIPE-EX-0)))
(check-expect (place-pipe GRID-EX-1 PIPE-LR 4 2 GOOFLOW-EX-0)
              (make-grid 7 7 (list (make-gp PIPE-LR 4 2)
                                   GRID-PIPE-EX-0
                                   GRID-PIPE-EX-1
                                   GRID-PIPE-EX-2)))

(define (place-pipe grid pipe row col gf)
  (if (ormap (λ (g) (and (= row (gp-row g))
                         (= col (gp-col g))))
             (map first (filter cons? (gooflow-goo-path gf))))
             #f
             (make-grid (grid-row grid)
                        (grid-col grid)
                        (cons (make-gp pipe row col)
                              (grid-grid-pipes grid)))))

;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.

(check-expect (pipe-at STARTING-GRID 0 0) #false)
(check-expect (pipe-at GRID-EX-0 0 0) STARTING-PIPE-R)
(check-expect (pipe-at GRID-EX-1 5 6) PIPE-CR)

(define (pipe-at grid row col)
  (cond
    [(empty? (grid-grid-pipes grid)) #false]
    [(and (cons? (grid-grid-pipes grid)) (= (gp-row (first (grid-grid-pipes grid))) row)
                                         (= (gp-col (first (grid-grid-pipes grid))) col))
     (gp-pipe (first (grid-grid-pipes grid)))]
    [(cons? (grid-grid-pipes grid))
     (pipe-at (make-grid (grid-row grid)
                         (grid-col grid)
                         (rest (grid-grid-pipes grid))) row col)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GOOFLOW FUNCTIONS

;; in-goo-flow? : Integer Integer GooFlow -> Boolean
;; checks if the coordinates are on the goo's path

(check-expect (in-goo-flow? 0 0 GOOFLOW-EX-0) #t)
(check-expect (in-goo-flow? 0 1 GOOFLOW-EX-1) #t)
(check-expect (in-goo-flow? 4 5 GOOFLOW-EX-2) #f)

(define (in-goo-flow? row col gf)
  (local
    ;; list of all gridpipes in the gooflow's path
    [(define gfp (map (λ (elem) (list (gp-row elem) (gp-col elem)))
                      (filter gp? (map first (filter (λ (l) (cons? l)) (gooflow-goo-path gf))))))]
    (ormap (λ (l) (and (= row (first l)) (= col (second l)))) gfp)))

;; num-pipe : GooFlow Pipe Number Number -> Number
;; Counts the number of the pipes with the given coordinates
;; and pipe type in the goo's path

(check-expect (num-pipe GOOFLOW-EX-0 STARTING-PIPE-R 0 0) 1)
(check-expect (num-pipe GOOFLOW-EX-0 PIPE-CR 0 0) 0)
(check-expect (num-pipe GOOFLOW-EX-2 PIPE-BL 0 1) 1)

(define (num-pipe gf pipe row col)
  (length (filter (λ (g) (and (= row (gp-row g))
                              (= col (gp-col g))))
                  (filter (λ (g) (and (boolean=? (pipe-top pipe) (pipe-top (gp-pipe g)))
                                      (boolean=? (pipe-bot pipe) (pipe-bot (gp-pipe g)))
                                      (boolean=? (pipe-left pipe) (pipe-left (gp-pipe g)))
                                      (boolean=? (pipe-right pipe) (pipe-right (gp-pipe g)))))
                          (map first (filter (λ (l) (cons? l)) (gooflow-goo-path gf)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRID->IMAGE FUNCTIONS

;; More Grid examples
(define GRID-EX-2 (place-pipe GRID-EX-1 PIPE-LR 6 6 GOOFLOW-EX-1))
(define GRID-EX-3 (place-pipe GRID-EX-1 PIPE-LR 6 0 GOOFLOW-EX-1))

;; draw-row : Grid Integer Integer GooFlow -> Image
;; This function draws one row of the grid with the given tile side length
;; and pipe width and checks if there is a pipe in each tile of the row

(define (draw-row grid tile-side-length pipe-width gooflow)
  (local
    [(define curr-row (- (grid-row grid) 1))
     (define curr-col (- (grid-col grid) 1))
     ;; finds all pipes in gooflow that have the same coordinates
     ;; as possible pipe being drawn
     (define dir-list (filter (λ (k) (and (= curr-row (gp-row (first k)))
                                          (= curr-col (gp-col (first k)))))
                              (filter cons? (gooflow-goo-path gooflow))))
     ;; finds the direction of the gooflow, otherwise sets a default
     ;; direction
     (define curr-dir (cond
                        [(empty? dir-list) RIGHT]
                        [(cons? dir-list) (second (first dir-list))]))
     (define PIPE (pipe-at grid curr-row curr-col))
     ;; determines if pipe is filled
     (define filled (in-goo-flow? curr-row curr-col gooflow))
     ;; for cross pipes, determines if goo has flown through it
     ;; more than once
     (define CR-COLOR (> (num-pipe gooflow PIPE-CR curr-row curr-col) 1))]
  (cond
    [(pipe? PIPE)
     (if (not (= curr-col 0))
         (beside (draw-row (make-grid (grid-row grid) (- (grid-col grid) 1) (grid-grid-pipes grid)) tile-side-length pipe-width gooflow)
                 (pipe->image PIPE tile-side-length pipe-width filled curr-dir CR-COLOR))
         (pipe->image PIPE tile-side-length pipe-width filled curr-dir CR-COLOR))]
    [(boolean? PIPE)
     (if (not (= curr-col 0))
         (beside (draw-row (make-grid (grid-row grid) (- (grid-col grid) 1) (grid-grid-pipes grid)) tile-side-length pipe-width gooflow)
                 (square tile-side-length "outline" "pink"))
         (square tile-side-length "outline" "pink"))])))

;; grid->image: Grid Integer Integer GooFlow -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.

(define (grid->image grid tile-side-length pipe-width gooflow)
  (if (not (= (- (grid-row grid) 1) 0))
      (above (grid->image (make-grid (- (grid-row grid) 1) (grid-col grid) (grid-grid-pipes grid)) tile-side-length pipe-width gooflow)
             (draw-row grid tile-side-length pipe-width gooflow))
      (draw-row grid tile-side-length pipe-width gooflow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GOO PROPAGATE FUNCTIONS

;; goo-propagate? : GooFlow Grid -> Boolean
;; Determines if goo can propagate on the grid

(check-expect (goo-propagate? GOOFLOW-EX-1 (make-grid 2 2
                                                      (list (make-gp STARTING-PIPE-R 0 0)
                                                           (make-gp PIPE-BL 0 1))))
              #f)
(check-expect (goo-propagate? GOOFLOW-EX-2 (make-grid 4 4
                                                      (list (make-gp STARTING-PIPE-R 0 0)
                                                            (make-gp PIPE-BL 0 1)
                                                            (make-gp PIPE-TR 1 1)
                                                            (make-gp PIPE-CR 1 2))))
              #t)
(check-expect (goo-propagate? GOOFLOW-EX-2 (make-grid 4 4
                                                      (list (make-gp STARTING-PIPE-R 0 0)
                                                            (make-gp PIPE-BL 0 1)
                                                            (make-gp PIPE-TR 1 1)
                                                            (make-gp PIPE-CR 1 3))))
              #f)

(define (goo-propagate? gf grid)
  (local
     ;; current direction of the gooflow
    [(define DIR (second (first (gooflow-goo-path gf))))
     (define ROW (gp-row (first (first (gooflow-goo-path gf)))))
     (define COL (gp-col (first (first (gooflow-goo-path gf)))))
     ;; find-pipe : Integer Integer Function -> Boolean
     ;; returns if there is a next pipe
     (define (find-pipe r c f)
       (if (pipe? (pipe-at grid r c))
           (f (pipe-at grid r c))
           #f))]
  (cond
    [(boolean? (first (gooflow-goo-path gf))) #f]
    [(string=? DIR RIGHT) (find-pipe ROW (add1 COL) pipe-left)]
    [(string=? DIR LEFT) (find-pipe ROW (sub1 COL) pipe-right)]
    [(string=? DIR UP) (find-pipe (sub1 ROW) COL pipe-bot)]
    [(string=? DIR DOWN) (find-pipe (add1 ROW) COL pipe-top)])))
              
;; next-direction : Direction Pipe -> Direction
;; Gives the next direction based on the given pipe and old direction

(check-expect (next-direction RIGHT PIPE-BL) DOWN)
(check-expect (next-direction LEFT PIPE-CR) LEFT)
(check-expect (next-direction UP PIPE-TB) UP)

(define (next-direction d p)
  (cond
    [(and (pipe-top p) (pipe-bot p) (pipe-left p) (pipe-right p)) d]
    [(and (pipe-top p) (pipe-left p) (string=? d RIGHT)) UP]
    [(and (pipe-top p) (pipe-left p) (string=? d DOWN)) LEFT]
    [(and (pipe-top p) (pipe-right p) (string=? d LEFT)) UP]
    [(and (pipe-top p) (pipe-right p) (string=? d DOWN)) RIGHT]
    [(and (pipe-bot p) (pipe-left p) (string=? d RIGHT)) DOWN]
    [(and (pipe-bot p) (pipe-left p) (string=? d UP)) LEFT]
    [(and (pipe-bot p) (pipe-right p) (string=? d LEFT)) DOWN]
    [(and (pipe-bot p) (pipe-right p) (string=? d UP)) RIGHT]
    [(and (pipe-left p) (pipe-right p) (string=? d RIGHT)) RIGHT]
    [(and (pipe-left p) (pipe-right p) (string=? d LEFT)) LEFT]
    [(and (pipe-top p) (pipe-bot p) (string=? d UP)) UP]
    [(and (pipe-top p) (pipe-bot p) (string=? d DOWN)) DOWN]))

;; grid-goo-propagate : GooFlow Grid -> GooFlow
;; Propagates the goo on the grid

(check-expect (grid-goo-propagate GOOFLOW-EX-1 (make-grid 2 2
                                                      (list (make-gp STARTING-PIPE-R 0 0)
                                                            (make-gp PIPE-BL 0 1))))
              (make-gooflow (cons #false (gooflow-goo-path GOOFLOW-EX-1))))
(check-expect (grid-goo-propagate GOOFLOW-EX-2 (make-grid 4 4
                                                      (list (make-gp STARTING-PIPE-R 0 0)
                                                            (make-gp PIPE-BL 0 1)
                                                            (make-gp PIPE-TR 1 1)
                                                            (make-gp PIPE-CR 1 2))))
              (make-gooflow (list (list (make-gp PIPE-CR 1 2) RIGHT)
                                  (list (make-gp PIPE-TR 1 1) RIGHT)
                                  (list (make-gp PIPE-BL 0 1) DOWN)
                                  (list (make-gp STARTING-PIPE-R 0 0) RIGHT))))
(check-expect (grid-goo-propagate GOOFLOW-EX-2 (make-grid 4 4
                                                      (list (make-gp STARTING-PIPE-R 0 0)
                                                            (make-gp PIPE-BL 0 1)
                                                            (make-gp PIPE-TR 1 1)
                                                            (make-gp PIPE-TR 1 2))))
              (make-gooflow (cons #false (gooflow-goo-path GOOFLOW-EX-2))))

(define (grid-goo-propagate gf grid)
  (local
     ;; current direction of gooflow
    [(define DIR (if (boolean? (first (gooflow-goo-path gf)))
                     ""
                     (second (first (gooflow-goo-path gf)))))
     ;; new-gf : Direction Pipe Integer Integer -> GooFlow
     ;; makes a new GooFlow based on given parameters
     (define (new-gf d p r c)
       (make-gooflow (cons (list (make-gp p r c)
                                 (next-direction d p))
                           (gooflow-goo-path gf))))]
  (cond
    [(boolean? (first (gooflow-goo-path gf))) gf]
    [(and (string=? DIR RIGHT) (goo-propagate? gf grid))
     (local
       [(define ROW (gp-row (first (first (gooflow-goo-path gf)))))
        (define COL (+ (gp-col (first (first (gooflow-goo-path gf)))) 1))
        (define PIPE (pipe-at grid ROW COL))]
     (new-gf DIR PIPE ROW COL))]
    [(and (string=? DIR LEFT) (goo-propagate? gf grid))
     (local
       [(define ROW (gp-row (first (first (gooflow-goo-path gf)))))
        (define COL (- (gp-col (first (first (gooflow-goo-path gf)))) 1))
        (define PIPE (pipe-at grid ROW COL))]
     (new-gf DIR PIPE ROW COL))]
    [(and (string=? DIR UP) (goo-propagate? gf grid))
     (local
       [(define ROW (- (gp-row (first (first (gooflow-goo-path gf)))) 1))
        (define COL (gp-col (first (first (gooflow-goo-path gf)))))
        (define PIPE (pipe-at grid ROW COL))]
     (new-gf DIR PIPE ROW COL))]
    [(and (string=? DIR DOWN) (goo-propagate? gf grid))
     (local
       [(define ROW (+ (gp-row (first (first (gooflow-goo-path gf)))) 1))
        (define COL (gp-col (first (first (gooflow-goo-path gf)))))
        (define PIPE (pipe-at grid ROW COL))]
     (new-gf DIR PIPE ROW COL))]
    [else (make-gooflow (cons #f (gooflow-goo-path gf)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAMESTATE DEFINITION

(define-struct gamestate [grid incoming-pipes tile-side-length
                               pipe-width starting-pipe gooflow
                               num-replaced-pipes flow-time])

;; A GameState is a (make-gamestate Grid [List-of Pipe] Number
;; Number GridPipe GooFlow Number Number) where:
;; - grid is the grid of the game being displayed
;; - incoming-pipes is the list of pipes to be placed on the grid
;; - tile-side-length is the length of each tile
;; - pipe-width is the width of the pipe
;; - starting-pipe is the starting pipe on the grid
;; - gooflow is the flow of goo
;; - num-replaced-pipes is the number of placed pipes
;; - flow-time is the amount of time until the next step of goo propagation

;; incoming pipe lists
(define INCOMING-PIPES (list PIPE-CR PIPE-TB PIPE-LR PIPE-BL PIPE-TL PIPE-BR PIPE-TR))
(define DOUBLE-PIPES (append INCOMING-PIPES INCOMING-PIPES))

(define GAMESTATE-EX-0 (make-gamestate GRID-EX-0 INCOMING-PIPES 50 5
                                       STARTING-PIPE-R GOOFLOW-EX-0 0 140))
(define GAMESTATE-EX-1 (make-gamestate GRID-EX-0 DOUBLE-PIPES 75 8
                                       STARTING-PIPE-R
                                       GOOFLOW-EX-0
                                       2 28))
(define GAMESTATE-EX-2 (make-gamestate GRID-EX-1 INCOMING-PIPES 50 5
                                       STARTING-PIPE-R
                                       GOOFLOW-EX-0
                                       1 80))

(define (gamestate-template gs)
  (... (gamestate-grid gs) ... (gamestate-incoming-pipes gs) ...
       (gamestate-tile-side-length gs) ... (gamestate-pipe-width gs) ...
       (pipe-template (gamestate-starting-pipe gs)) ... (gooflow-template (gamestate-gooflow gs))
   ... (gamestate-num-replaced-pipes gs) ... (gamestate-flow-time gs) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAMESTATE/BIG BANG FUNCTIONS

;; gamestate-init : Integer Integer Integer Integer Direction
;; [List-of Pipe] Integer Integer -> GameState
;; Initializes a gamestate given n rows and m columns for the grid,
;; the row, column, and direction of the starting pipe, the list of
;; incoming pipes, the tile side length, and the pipe width

(check-expect (gamestate-init 7 7 0 0 RIGHT INCOMING-PIPES 50 5)
              (make-gamestate (make-grid 7 7 (list (make-gp STARTING-PIPE-R 0 0)))
                              INCOMING-PIPES
                              50 5
                              (make-gp STARTING-PIPE-R 0 0)
                              (make-gooflow (list (list (make-gp STARTING-PIPE-R 0 0) RIGHT)))
                              0 140))
(check-expect (gamestate-init 3 4 1 2 DOWN DOUBLE-PIPES 75 8)
              (make-gamestate (make-grid 3 4 (list (make-gp STARTING-PIPE-B 1 2)))
                              DOUBLE-PIPES
                              75 8
                              (make-gp STARTING-PIPE-B 1 2)
                              (make-gooflow (list (list (make-gp STARTING-PIPE-B 1 2) DOWN)))
                              0 140))
(check-expect (gamestate-init 6 6 5 5 LEFT INCOMING-PIPES 100 10)
              (make-gamestate (make-grid 6 6 (list (make-gp STARTING-PIPE-L 5 5)))
                              INCOMING-PIPES
                              100 10
                              (make-gp STARTING-PIPE-L 5 5)
                              (make-gooflow (list (list (make-gp STARTING-PIPE-L 5 5) LEFT)))
                              0 140))
              
(define (gamestate-init row col sp-row sp-col sp-dir incoming-pipes tile-sl pipe-width)
  (local [;; determines which starting pipe to put into the grid based on its direction
          (define STARTING-PIPE (make-gp 
                                 (cond
                                   [(string=? sp-dir RIGHT) STARTING-PIPE-R]
                                   [(string=? sp-dir LEFT) STARTING-PIPE-L]
                                   [(string=? sp-dir UP) STARTING-PIPE-T]
                                   [(string=? sp-dir DOWN) STARTING-PIPE-B])
                                 sp-row
                                 sp-col))]
  (make-gamestate (make-grid row col (list STARTING-PIPE)) incoming-pipes tile-sl pipe-width
                  STARTING-PIPE
                  (make-gooflow (list (list STARTING-PIPE sp-dir))) 0 140)))

;; draw-game : GameState -> Image
;; This function draws the current grid of the game

(define (draw-grid gs)
  (grid->image (gamestate-grid gs) (gamestate-tile-side-length gs)
               (gamestate-pipe-width gs) (gamestate-gooflow gs)))

;; draw-incoming-pipes : GameState -> Image
;; This function draws the incoming pipes

(define (draw-incoming-pipes count gs)
  (local
    [;; new-gs : [List-of Pipe] -> GameState
     ;; creates a new GameState with the new list
     ;; of incoming pipes
     (define (new-gs ip)
       (make-gamestate (gamestate-grid gs)
                       ip
                       (gamestate-tile-side-length gs)
                       (gamestate-pipe-width gs)
                       (gamestate-starting-pipe gs)
                       (gamestate-gooflow gs)
                       (gamestate-num-replaced-pipes gs)
                       (gamestate-flow-time gs)))]    
  (cond
    [(or (> (+ count 1) 4) (empty? (gamestate-incoming-pipes gs))) (square 0.1 "outline" "white")]
    [(and (<= (+ count 1) 4) (cons? (gamestate-incoming-pipes gs)))
      (beside (pipe->image (first (gamestate-incoming-pipes gs))
                           (gamestate-tile-side-length gs)
                           (gamestate-pipe-width gs)
                           #f
                           RIGHT
                           #f)
              (draw-incoming-pipes
               (+ count 1)
               (new-gs (rest (gamestate-incoming-pipes gs)))))]
    [else (square 0.1 "outline" "white")])))

;; get-score : GameState -> Integer
;; Computes the current score of the game

(check-expect (get-score GAMESTATE-EX-0) 50)
(check-expect (get-score GAMESTATE-EX-1) -50)
(check-expect (get-score GAMESTATE-EX-2) 0)

(define (get-score gs)
  (* 50 (- (length (gooflow-goo-path (gamestate-gooflow gs)))
           (gamestate-num-replaced-pipes gs))))

;; draw-game : GameState -> Image
;; This function draws the whole game

(define (draw-game gs)
  (above (text (string-append "Current score: "
                              (number->string (get-score gs)))
               25
               "pink")
         (draw-grid gs)
         (square 50 "outline" "white")
         (text "NEXT PIPE" 20 "pink")
         (draw-incoming-pipes 0 gs)))

;; gamestate-propagate-goo : GameState -> GameState
;; propagates the goo in the gamestate

(check-expect (gamestate-propagate-goo GAMESTATE-EX-0)
              (make-gamestate (gamestate-grid GAMESTATE-EX-0)
                              (gamestate-incoming-pipes GAMESTATE-EX-0)
                              (gamestate-tile-side-length GAMESTATE-EX-0)
                              (gamestate-pipe-width GAMESTATE-EX-0)
                              (gamestate-starting-pipe GAMESTATE-EX-0)
                              (make-gooflow (cons #f (gooflow-goo-path (gamestate-gooflow GAMESTATE-EX-0))))
                              (gamestate-num-replaced-pipes GAMESTATE-EX-0)
                              28))
(check-expect (gamestate-propagate-goo GAMESTATE-EX-1)
              (make-gamestate (gamestate-grid GAMESTATE-EX-1)
                              (gamestate-incoming-pipes GAMESTATE-EX-1)
                              (gamestate-tile-side-length GAMESTATE-EX-1)
                              (gamestate-pipe-width GAMESTATE-EX-1)
                              (gamestate-starting-pipe GAMESTATE-EX-1)
                              (make-gooflow (cons #f (gooflow-goo-path (gamestate-gooflow GAMESTATE-EX-1))))
                              (gamestate-num-replaced-pipes GAMESTATE-EX-1)
                              28))
(check-expect (gamestate-propagate-goo GAMESTATE-EX-2)
              (make-gamestate (gamestate-grid GAMESTATE-EX-2)
                              (gamestate-incoming-pipes GAMESTATE-EX-2)
                              (gamestate-tile-side-length GAMESTATE-EX-2)
                              (gamestate-pipe-width GAMESTATE-EX-2)
                              (gamestate-starting-pipe GAMESTATE-EX-2)
                              (make-gooflow (cons #f (gooflow-goo-path (gamestate-gooflow GAMESTATE-EX-2))))
                              (gamestate-num-replaced-pipes GAMESTATE-EX-2)
                              28))

(define (gamestate-propagate-goo gs)
  (make-gamestate (gamestate-grid gs)
                  (gamestate-incoming-pipes gs)
                  (gamestate-tile-side-length gs)
                  (gamestate-pipe-width gs)
                  (gamestate-starting-pipe gs)
                  (grid-goo-propagate
                   (gamestate-gooflow gs)
                   (gamestate-grid gs))
                  (gamestate-num-replaced-pipes gs)
                  28))

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.

(check-expect (place-pipe-on-click GAMESTATE-EX-0 100 200 "button-down")
              (make-gamestate (make-grid 3 4
                                         (cons (make-gp
                                                (first (gamestate-incoming-pipes GAMESTATE-EX-0)) 3 1)
                                               (grid-grid-pipes (gamestate-grid GAMESTATE-EX-0))))
                              (rest (gamestate-incoming-pipes GAMESTATE-EX-0))
                              50
                              5
                              STARTING-PIPE-R
                              (gamestate-gooflow GAMESTATE-EX-0)
                              0
                              140))
(check-expect (place-pipe-on-click GAMESTATE-EX-1 345 498 "button-down")
              (make-gamestate (make-grid 3 4
                                         (cons (make-gp
                                                (first (gamestate-incoming-pipes GAMESTATE-EX-1)) 6 4)
                                               (grid-grid-pipes (gamestate-grid GAMESTATE-EX-1))))
                              (rest (gamestate-incoming-pipes GAMESTATE-EX-1))
                              75
                              8
                              STARTING-PIPE-R
                              (gamestate-gooflow GAMESTATE-EX-0)
                              2
                              28))
(check-expect (place-pipe-on-click GAMESTATE-EX-2 34 28 "button-down")
              GAMESTATE-EX-2)

(define (place-pipe-on-click gs x y mouse-event)
  (local
    [;; all fields of the given gamestate
     (define ip (gamestate-incoming-pipes gs))
     (define griddy (gamestate-grid gs))
     (define tsl (gamestate-tile-side-length gs))
     (define pw (gamestate-pipe-width gs))
     (define sp (gamestate-starting-pipe gs))
     (define gf (gamestate-gooflow gs))
     (define nrp (gamestate-num-replaced-pipes gs))
     (define ft (gamestate-flow-time gs))]
  (cond
    [(and (mouse=? "button-down" mouse-event)
          (cons? ip))
     (local
        [;; new grid based on whether the pipe can be placed
         ;; (depends on if the pipe already has goo in it)
         (define new-griddy (place-pipe griddy
                                  (first ip)
                                  (- (ceiling (/ (- y 25) tsl)) 1)
                                  (- (ceiling (/ x tsl)) 1)
                                  gf))
         ;; new-rp : Grid -> Integer
         ;; Returns the number of pipes that have been replaced on the grid
         (define (new-rp g)
           (if (> (length (filter (λ (g) (and (= (- (ceiling (/ (- y 25) tsl)) 1) (gp-row g))
                                              (= (- (ceiling (/ x tsl)) 1) (gp-col g))))
                                 (grid-grid-pipes g))) 1)
              (+ nrp 1)
              nrp))]
     (make-gamestate
          (if (boolean? new-griddy) griddy new-griddy)
          (if (boolean? new-griddy) ip (rest ip))
          tsl pw sp gf
          (if (boolean? new-griddy) (new-rp griddy) (new-rp new-griddy))
          ft))]
    [else gs])))

;; countdown-timer : GameState -> GameState
;; Timer for the game

(check-expect (countdown-timer GAMESTATE-EX-0) GAMESTATE-EX-0) 
(check-expect (countdown-timer GAMESTATE-EX-1) GAMESTATE-EX-1)
(check-expect (countdown-timer GAMESTATE-EX-2) GAMESTATE-EX-2)

(define (countdown-timer gs)
  (cond
    [(and (goo-propagate? (gamestate-gooflow gs) (gamestate-grid gs))
          (> (gamestate-flow-time gs) 0))
     (make-gamestate (gamestate-grid gs)
                     (gamestate-incoming-pipes gs)
                     (gamestate-tile-side-length gs)
                     (gamestate-pipe-width gs)
                     (gamestate-starting-pipe gs)
                     (gamestate-gooflow gs)
                     (gamestate-num-replaced-pipes gs)
                     (- (gamestate-flow-time gs) 1))]
    [(and (goo-propagate? (gamestate-gooflow gs) (gamestate-grid gs))
          (= (gamestate-flow-time gs) 0))
     (gamestate-propagate-goo gs)]
    [else gs]))
    
;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
    (to-draw draw-game)
    (on-mouse place-pipe-on-click)
    (on-tick countdown-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TASK 7 GAMESTATE
(define T7GRID (make-grid 6 6 (list (make-gp PIPE-TL 2 4)
                                    (make-gp PIPE-LR 2 3)
                                    (make-gp PIPE-BR 2 1)
                                    (make-gp PIPE-CR 3 1)
                                    (make-gp PIPE-TR 4 1)
                                    (make-gp PIPE-TL 4 2)
                                    (make-gp PIPE-TB 3 2)
                                    (make-gp PIPE-CR 2 2)
                                    (make-gp PIPE-BL 1 2)
                                    (make-gp STARTING-PIPE-R 1 1))))
(define T7INCPIPES (list PIPE-BR PIPE-TB PIPE-LR PIPE-CR PIPE-TL))
(define T7GOOFLOW (make-gooflow (list (list (make-gp STARTING-PIPE-R 1 1) RIGHT))))
(define T7GAMESTATE (make-gamestate T7GRID
                                    T7INCPIPES
                                    50
                                    5
                                    STARTING-PIPE-R
                                    T7GOOFLOW
                                    0 140))
;; (pipe-fantasy T7GAMESTATE)