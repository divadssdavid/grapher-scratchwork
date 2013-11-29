#lang racket

(require 2htdp/universe 2htdp/image)
(require racket/format)
(provide graph)

" Hello! I see that you are using my graphing program." 
" In order to graph some function, x^2 say, from x = -5 " 
" to x = 5 say. You would first have to define the square" 
" function, (define (square x) (* x x)) for example"
" and then type (graph square -5 5) and press enter" 

;; Constants 

(define GRAPH-WIDTH 700)
(define GRAPH-HEIGHT 700)
(define WINDOW-WIDTH 1000)
(define WINDOW-HEIGHT 800) 
(define GRAPH-X-OFFSET (- 100))
(define GRAPH-Y-OFFSET (- 5))
(define GRAPH (rectangle GRAPH-WIDTH GRAPH-HEIGHT "solid" "gray"))
(define T-GRAPH (rectangle GRAPH-WIDTH GRAPH-HEIGHT 0 "gray")) ;; this is going to be completely transparent
(define WINDOW (rectangle WINDOW-WIDTH WINDOW-HEIGHT "outline" "white"))
(define T-WINDOW (rectangle WINDOW-WIDTH WINDOW-HEIGHT 0 "white"))
(define SCENE (overlay/offset GRAPH GRAPH-X-OFFSET GRAPH-Y-OFFSET WINDOW))
(define SCENE1-GRAPH-X-OFFSET 600)
(define SCENE1-GRAPH-Y-OFFSET 400)
(define SCENE2XOFFSET 280)
(define SCENE2YOFFSET 20)
(define POINT (circle 1 "solid" "blue"))
(define TRACE-CROSS  
  (overlay 
   (rectangle 20 1 "solid" "black")
   (overlay 
    (rectangle 1 20 "solid" "black")
    (circle 6 "outline" "black"))))

(define HORIZONTAL (rectangle 700 1 "solid" "black"))
(define VERTICAL (rectangle 1 700 "solid" "black"))

;; 
;; I am going to use SCENE1 as the background for my graphing program. 
;; I am going to build it incrementally.(of course) 
#;
(define SCENE1 
  (place-image GRAPH SCENE1-GRAPH-X-OFFSET SCENE1-GRAPH-Y-OFFSET 
               (empty-scene WINDOW-WIDTH WINDOW-HEIGHT "beige")))

(define SCENE2
  (underlay/xy (empty-scene WINDOW-WIDTH WINDOW-HEIGHT "beige") 
               SCENE2XOFFSET SCENE2YOFFSET GRAPH ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; STRUCTS 
;; 
;; This struct called a point will simply represent 
;; a point in the normal algebra sense
(struct point (x-coordinate y-coordinate) #:transparent)

;; I am now going to compose a struct to represent 
;; the state of the graphing program.
;;
;; Most of the things in this struct are basically 
;; self-explanatory. 

;; list-of-points - this will be the list of points that will actually be plotted
;; function - this is the function which is used to generate the y-value of the points
;; leftmost-x, rightmost-x - these are the left and right x-endpoints of the graphing window
;; current-x, current-y - these are the x or y values where a vertical or horizontal would be drawn
;; current-point - this is the point where a TRACE-CROSS will be drawn while in trace mode
;; vertical? - true to indicate a vertical to be shown at the current-x location, false otherwise
;; horizontal? - true to indicate a horizontal to be shown at the current-y location, false otherwise
;; trace? - true if in trace mode, false otherwise. 

(struct graph-state (list-of-points function leftmost-x rightmost-x current-x current-y 
                                    current-point vertical? horizontal? trace?) #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     ;;;;;
;;; BIG BANG MECHANISMS ;;;;;
;;;                     ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph function lx rx)
  (local [(define STATE (initialize-state function lx rx))]
    (big-bang STATE
              (on-draw render)
              (on-key key-handler))))


;; stub for render
(define (render state) 
  (local [(define MIN-X (text (~a (min-x (graph-state-list-of-points state)) #:max-width 5) 12 "black"))
          (define MAX-X (text (~a (graph-state-rightmost-x state)#:max-width 5) 12 "black"))
          (define MIN-Y (text (~a (* 1.0 (min-y (graph-state-list-of-points state)))#:max-width 5) 12 "black"))
          (define MAX-Y (text (~a (* 1.0 (max-y (graph-state-list-of-points state)))#:max-width 5) 12 "black"))
          (define X-AXES 
            (underlay/xy (underlay/xy T-WINDOW 
                                      285
                                      725
                                      MIN-X)
                         940 
                         725
                         MAX-X))
          (define Y-AXES
            (underlay/xy (underlay/xy T-WINDOW 
                                      240
                                      30
                                      MAX-Y)
                         240 
                         710
                         MIN-Y))
          (define AXES (overlay X-AXES Y-AXES))

          
          (define PLAIN 
            (underlay/xy SCENE2  SCENE2XOFFSET SCENE2YOFFSET 
                         (plot-points (graph-state-list-of-points state))))
          (define COORDINATES
            (underlay/xy (underlay/xy T-WINDOW 
                                      285
                                      775
                                      (text (string-append "x: " (~a (graph-state-current-x state))) 12 "black"))
                         800 
                         775
                         (text (string-append "y: " (~a ((graph-state-function state) (graph-state-current-x state)))) 12 "black")))
          (define TRACE-IMAGE (overlay COORDINATES (underlay/xy PLAIN  SCENE2XOFFSET SCENE2YOFFSET (place-trace state))))
          (define HORIZONTAL-IMAGE (underlay/xy T-WINDOW  SCENE2XOFFSET SCENE2YOFFSET (place-horizontal state)))
          (define VERTICAL-IMAGE (underlay/xy T-WINDOW  SCENE2XOFFSET SCENE2YOFFSET (place-vertical state)))
          (define TRACE-STATE? (graph-state-trace? state)) 
          (define VERTICAL-STATE? (graph-state-vertical? state))
          (define HORIZONTAL-STATE? (graph-state-horizontal? state))]
    (cond [(and TRACE-STATE? VERTICAL-STATE? HORIZONTAL-STATE?) (underlay TRACE-IMAGE (overlay HORIZONTAL-IMAGE VERTICAL-IMAGE AXES))]
          [(and TRACE-STATE? VERTICAL-STATE?) (underlay TRACE-IMAGE VERTICAL-IMAGE AXES)]
          [(and TRACE-STATE? HORIZONTAL-STATE?) (underlay TRACE-IMAGE HORIZONTAL-IMAGE AXES)]
          [(and VERTICAL-STATE? HORIZONTAL-STATE?) (underlay PLAIN (overlay HORIZONTAL-IMAGE VERTICAL-IMAGE AXES))]
          [TRACE-STATE? (overlay AXES TRACE-IMAGE)]
          [VERTICAL-STATE? (underlay PLAIN VERTICAL-IMAGE AXES)]
          [HORIZONTAL-STATE?  (underlay PLAIN HORIZONTAL-IMAGE AXES)]
          [else (overlay AXES PLAIN)])))



;; I am now going to write a function that will take a list of points, 
;; and then plot them onto a graph window. 
;; I am going to make the y values of the function fit within the y = 25 
;; all the way to y =  675 in the plotting window. I made an equation to 
;; map the y values to the plotting window

;; y to plot =  675 + ((y - y-min)/(y-max - y-min) * 650)

;; Signature 
;; list-of-points -> image 


(define (plot-points list-of-points)
  (local [(define MIN-Y (min-y list-of-points))
          (define MAX-Y (max-y list-of-points))
          (define VERTICAL-RANGE (- MAX-Y MIN-Y))
          (define (plotting-y y) (+ 675 (* 650 (/ (-  MIN-Y y) VERTICAL-RANGE)))) ;; scaling y values to fit window
          (define MIN-X (min-x list-of-points))
          (define MAX-X (max-x list-of-points))
          (define HORIZONTAL-RANGE (- MAX-X MIN-X))
          (define (plotting-x x) (+ 10  (* 680 (/ (- x MIN-X) HORIZONTAL-RANGE)))) ;; scaling x values to fully fit window
          (define (place-point point)
            (underlay/xy T-GRAPH 
                         (plotting-x (point-x-coordinate point))
                         (plotting-y (point-y-coordinate point))
                         POINT))
          (define (pp-iter list-of-points)
            (if (empty? list-of-points)
                T-GRAPH
                (overlay (place-point (first list-of-points))
                         (pp-iter (rest list-of-points)))))]
    (pp-iter list-of-points)))

;; need a function to place the trace-cross 
(define (place-trace state)
  (local [(define MIN-Y (min-y (graph-state-list-of-points state)))
          (define MAX-Y (max-y (graph-state-list-of-points state)))
          (define VERTICAL-RANGE (- MAX-Y MIN-Y))
          (define (plotting-y y) (+ 675 (* 650 (/ (-  MIN-Y y) VERTICAL-RANGE)))) ;; scaling y values to fit window
          (define MIN-X (min-x (graph-state-list-of-points state)))
          (define MAX-X (max-x (graph-state-list-of-points state)))
          (define HORIZONTAL-RANGE (- MAX-X MIN-X))
          (define (plotting-x x) (+ 10  (* 680 (/ (- x MIN-X) HORIZONTAL-RANGE)))) ;; scaling x values to fully fit window
          (define (place-cross point)
            (underlay/xy T-GRAPH 
                         (- (plotting-x (point-x-coordinate point)) (* (/ 1 2) (image-width TRACE-CROSS)))
                         (- (plotting-y (point-y-coordinate point)) (* (/ 1 2) (image-height TRACE-CROSS)))
                         TRACE-CROSS))]
    (place-cross (create-point (graph-state-function state) (graph-state-current-x state)))))

(define (place-vertical state)
  (local [(define MIN-Y (min-y (graph-state-list-of-points state)))
          (define MAX-Y (max-y (graph-state-list-of-points state)))
          (define VERTICAL-RANGE (- MAX-Y MIN-Y))
          (define (plotting-y y) (+ 675 (* 650 (/ (-  MIN-Y y) VERTICAL-RANGE)))) ;; scaling y values to fit window
          (define MIN-X (min-x (graph-state-list-of-points state)))
          (define MAX-X (max-x (graph-state-list-of-points state)))
          (define HORIZONTAL-RANGE (- MAX-X MIN-X))
          (define (plotting-x x) (+ 10  (* 680 (/ (- x MIN-X) HORIZONTAL-RANGE)))) ;; scaling x values to fully fit window
          (define (place-cross point)
            (underlay/xy T-GRAPH 
                         (plotting-x (point-x-coordinate point))
                         0
                         VERTICAL))]
    (place-cross (create-point (graph-state-function state) (graph-state-current-x state)))))

(define (place-horizontal state)
  (local [(define MIN-Y (min-y (graph-state-list-of-points state)))
          (define MAX-Y (max-y (graph-state-list-of-points state)))
          (define VERTICAL-RANGE (- MAX-Y MIN-Y))
          (define (plotting-y y) (+ 675 (* 650 (/ (-  MIN-Y y) VERTICAL-RANGE)))) ;; scaling y values to fit window
          (define MIN-X (min-x (graph-state-list-of-points state)))
          (define MAX-X (max-x (graph-state-list-of-points state)))
          (define HORIZONTAL-RANGE (- MAX-X MIN-X))
          (define (plotting-x x) (+ 10  (* 680 (/ (- x MIN-X) HORIZONTAL-RANGE)))) ;; scaling x values to fully fit window
          (define (place-cross state)
            (underlay/xy T-GRAPH 
                         0 
                         (plotting-y (point-y-coordinate (create-point (graph-state-function state) (graph-state-current-x state))))
                         HORIZONTAL))]
    (place-cross state)))

(define (key-handler state key)
  (cond [(key=? key "left")        (left-click state)]       ; make the cursor move one place to the left
        [(key=? key "right")       (right-click state)]      ; make the cursor move one place to the right
        [(key=? key "up")          (up-click state)]         ; make the cursor move to start of line
        [(key=? key "down")        (down-click state)]       ; make the cursor move to the end of the line
        [(key=? key "t")          (toggle-trace state)]  ; delete key immediately before the cursor      
        [(key=? key "h")          (toggle-horizontal state)]  ; delete key immediately before the cursor      
        [(key=? key "v")          (toggle-vertical state)]  ; delete key immediately before the cursor      
        [(key=? key "i")          (zoom-in state)]
        [(key=? key "o")          (zoom-out state)]
        [(key=? key "escape")      (save-image-fun state)] ; save the graph in a file with a name that will always be unique
        [else state]))                                       ; cause nothing to happen to the editor state when another key is pressed

;; I need to make a function called left-click that will 
;; take a graph state, and change the current-x variable 
;; a little to the left

(define (save-image-fun state)
  (local [(define THING (save-svg-image (render state) (~a (current-seconds))))]
    state))


(define (zoom-in state) 
  (local [(define lx (graph-state-leftmost-x state))
          (define rx (graph-state-rightmost-x state))
          (define WIDTH (- rx lx) )
          (define CENTER-X (graph-state-current-x state))
          (define FUNCTION (graph-state-function state))
          (define NLX (- CENTER-X (* (/ 1 2) WIDTH)))
          (define NRX (+ CENTER-X (* (/ 1 2) WIDTH)))
          (define STATE (initialize-state FUNCTION NLX NRX))]
    (change-current-x STATE CENTER-X)))

(define (zoom-out state) 
  (local [(define lx (graph-state-leftmost-x state))
          (define rx (graph-state-rightmost-x state))
          (define WIDTH (- rx lx) )
          (define CENTER-X (graph-state-current-x state))
          (define FUNCTION (graph-state-function state))
          (define NLX (- CENTER-X (* 2 WIDTH)))
          (define NRX (+ CENTER-X (* 2 WIDTH)))
          (define STATE (initialize-state FUNCTION NLX NRX))]
    (change-current-x STATE CENTER-X)))



(define (change-current-x state desired-x)
  (graph-state (graph-state-list-of-points state)
               (graph-state-function state)
               (graph-state-leftmost-x state)
               (graph-state-rightmost-x state)
               desired-x 
               (graph-state-current-y state)
               (graph-state-current-point state)
               (graph-state-vertical? state)
               (graph-state-horizontal? state)
               (graph-state-trace? state)))

(define (left-click state) 
  (local [(define lx (graph-state-leftmost-x state))
          (define rx (graph-state-rightmost-x state))
          (define STEP-SIZE (* 10 (/ (- rx lx) GRAPH-WIDTH)))]
    (graph-state (graph-state-list-of-points state)
                 (graph-state-function state)
                 lx
                 rx
                 (- (graph-state-current-x state) STEP-SIZE)
                 (graph-state-current-y state)
                 (graph-state-current-point state)
                 (graph-state-vertical? state)
                 (graph-state-horizontal? state)
                 (graph-state-trace? state))))


(define (right-click state) 
  (local [(define lx (graph-state-leftmost-x state))
          (define rx (graph-state-rightmost-x state))
          (define STEP-SIZE (* 10 (/ (- rx lx) GRAPH-WIDTH)))]
    (graph-state (graph-state-list-of-points state)
                 (graph-state-function state)
                 lx
                 rx
                 (+ (graph-state-current-x state) STEP-SIZE)
                 (graph-state-current-y state)
                 (graph-state-current-point state)
                 (graph-state-vertical? state)
                 (graph-state-horizontal? state)
                 (graph-state-trace? state))))

(define (up-click state) 
  (local [(define lx (graph-state-leftmost-x state))
          (define rx (graph-state-rightmost-x state))
          (define STEP-SIZE (* 10 (/ (- rx lx) GRAPH-WIDTH)))]
    (graph-state (graph-state-list-of-points state)
                 (graph-state-function state)
                 lx
                 rx
                 (graph-state-current-x state) 
                 (+ (graph-state-current-y state) STEP-SIZE)
                 (graph-state-current-point state)
                 (graph-state-vertical? state)
                 (graph-state-horizontal? state)
                 (graph-state-trace? state))))

(define (down-click state) 
  (local [(define lx (graph-state-leftmost-x state))
          (define rx (graph-state-rightmost-x state))
          (define STEP-SIZE (* 10 (/ (- rx lx) GRAPH-WIDTH)))]
    (graph-state (graph-state-list-of-points state)
                 (graph-state-function state)
                 lx
                 rx
                 (graph-state-current-x state) 
                 (- (graph-state-current-y state) STEP-SIZE)
                 (graph-state-current-point state)
                 (graph-state-vertical? state)
                 (graph-state-horizontal? state)
                 (graph-state-trace? state))))

(define (toggle-trace state) 
  (local [(define lx (graph-state-leftmost-x state))
          (define rx (graph-state-rightmost-x state))
          (define STEP-SIZE (/ (- rx lx) GRAPH-WIDTH))]
    (graph-state (graph-state-list-of-points state)
                 (graph-state-function state)
                 lx
                 rx
                 (graph-state-current-x state)
                 (graph-state-current-y state)
                 (graph-state-current-point state)
                 (graph-state-vertical? state)
                 (graph-state-horizontal? state)
                 (not (graph-state-trace? state)))))

(define (toggle-horizontal state) 
  (local [(define lx (graph-state-leftmost-x state))
          (define rx (graph-state-rightmost-x state))
          (define STEP-SIZE (/ (- rx lx) GRAPH-WIDTH))]
    (graph-state (graph-state-list-of-points state)
                 (graph-state-function state)
                 lx
                 rx
                 (graph-state-current-x state) 
                 (graph-state-current-y state)
                 (graph-state-current-point state)
                 (graph-state-vertical? state)
                 (not (graph-state-horizontal? state))
                 (graph-state-trace? state))))

(define (toggle-vertical state) 
  (local [(define lx (graph-state-leftmost-x state))
          (define rx (graph-state-rightmost-x state))
          (define STEP-SIZE (/ (- rx lx) GRAPH-WIDTH))]
    (graph-state (graph-state-list-of-points state)
                 (graph-state-function state)
                 lx
                 rx
                 (graph-state-current-x state) 
                 (graph-state-current-y state)
                 (graph-state-current-point state)
                 (not (graph-state-vertical? state))
                 (graph-state-horizontal? state)
                 (graph-state-trace? state))))


;; I need to make a function that will initialize the state of the graphing 
;; program. 
;; Signature 
;; (Float -> Float), Float, Float -> graph-state

;; Purpose 
;; this function will initialize the graph state that will be passed to the big bang 
;; mechanism. 
(define (initialize-state function lx rx)
  (local [(define list-of-points (list-points function lx rx))]
    (graph-state list-of-points            ;; this is of course the list of points 
                 function                  ;; this is the function that we are graphing
                 lx                        ;; leftmost-x 
                 rx                        ;; rightmost-x
                 lx                        ;; current-x 
                 (function lx)             ;; current-y 
                 (first list-of-points)    ;; current-point 
                 false                     ;; vertical? 
                 false                     ;; horizontal? 
                 false)))                  ;; trace-mode? 

                    

;; This function will take in a function and the leftmost and rightmost 
;; x coordinates

(define (list-points function lx rx) 
  (local [(define (create-points x) (create-point function x))]
    (if (= lx rx) ;; if the left and right x-points are the same, then  there will be no points
        (empty)
        (list-apply create-points (create-domain lx rx)))))

;; Given a left-x point and a right x point, I need to make a list of the x values 
;; that I am going to use. My graph window has a width given by the constatn GRAPH-WIDTH
;; Thus we need a total of GRAPH-WIDTH pixels to graph the function within the window
;; Signature
;; Float, Float -> list-of-Floats
;; Purpose
;; We input the two x endpoints of the region we want to graph and we outut a list 
;; of x values which we will then evaluate via f to form the points that we will plot
(define (create-domain lx rx)
  (local [(define STEP-SIZE  (/ (- rx lx) GRAPH-WIDTH))
          (define (create-domain-iter lx rx step-size)
            (if (>=  (- lx rx) 0) 
                empty
                (cons lx (create-domain-iter (+ lx step-size) rx step-size))))]
    (create-domain-iter lx rx STEP-SIZE)))

;; I now need to make a function that given an x-coordinate will output 
;; a point. I am going to use this in the list points function above. 
;; Signature 
;; (Float -> Float), Float -> point struct 

;; Purpose
;; this function will take in a function f, and an input x, and will 
;; output a point (x, f(x))

(define (create-point function x)
  (point x  (function x)))


;; I need a function that takes a list and a function, and produces a new list
;; by applying the function passed in to each member of the list

(define (list-apply function list-on-which-to-apply-function)
  (if (empty? list-on-which-to-apply-function)
      empty
      (cons (function (first list-on-which-to-apply-function))
            (list-apply function (rest list-on-which-to-apply-function)))))


;; In order to properly place the graph vertically, I need a function to determine the 
;; maximum and minimum y values that the function achieves on the interval that is entered
(define (max-y list-of-points)
  (if (empty? list-of-points)
      0
      (local [(define MAX (point-y-coordinate (first list-of-points)))
              (define (max-y-iter list-of-points guess-of-max)
                (if (empty? list-of-points) 
                    guess-of-max
                    (if (> (point-y-coordinate (first list-of-points)) guess-of-max)
                        (max-y-iter (rest list-of-points) (point-y-coordinate (first list-of-points)))
                        (max-y-iter (rest list-of-points) guess-of-max))))]
        (max-y-iter list-of-points MAX))))


(define (min-y list-of-points)
  (if (empty? list-of-points)
      0
      (local [(define MAX (point-y-coordinate (first list-of-points)))
              (define (max-y-iter list-of-points guess-of-max)
                (if (empty? list-of-points) 
                    guess-of-max
                    (if (< (point-y-coordinate (first list-of-points)) guess-of-max)
                        (max-y-iter (rest list-of-points) (point-y-coordinate (first list-of-points)))
                        (max-y-iter (rest list-of-points) guess-of-max))))]
        (max-y-iter list-of-points MAX))))


(define (max-x list-of-points)
  (if (empty? list-of-points)
      0
      (local [(define MAX (point-x-coordinate (first list-of-points)))
              (define (max-y-iter list-of-points guess-of-max)
                (if (empty? list-of-points) 
                    guess-of-max
                    (if (> (point-x-coordinate (first list-of-points)) guess-of-max)
                        (max-y-iter (rest list-of-points) (point-x-coordinate (first list-of-points)))
                        (max-y-iter (rest list-of-points) guess-of-max))))]
        (max-y-iter list-of-points MAX))))


(define (min-x list-of-points)
  (if (empty? list-of-points)
      0
      (local [(define MAX (point-x-coordinate (first list-of-points)))
              (define (max-y-iter list-of-points guess-of-max)
                (if (empty? list-of-points) 
                    guess-of-max
                    (if (< (point-x-coordinate (first list-of-points)) guess-of-max)
                        (max-y-iter (rest list-of-points) (point-x-coordinate (first list-of-points)))
                        (max-y-iter (rest list-of-points) guess-of-max))))]
        (max-y-iter list-of-points MAX))))



(module+ test 
  (require rackunit)
  (define (square x) (* x x))
  (check-equal? (list-apply square '( 1 2 3 4)) '(1 4 9 16))
  (check-equal? (max-y (cons (point 0 1) (cons (point 0 2) (cons (point 0 3) empty)))) 3)
  (check-equal? (max-y (cons (point 0 3) (cons (point 0 1) (cons (point 0 2) empty)))) 3)
  (check-equal? (min-y (cons (point 0 1) (cons (point 0 2) (cons (point 0 3) empty)))) 1)
  (check-equal? (min-y (cons (point 0 3) (cons (point 0 1) (cons (point 0 2) empty)))) 1)
  
  (check-equal? (max-x (cons (point 1 1) (cons (point 2 2) (cons (point 3 3) empty)))) 3)
  (check-equal? (max-x (cons (point 2 3) (cons (point 3 1) (cons (point 1 2) empty)))) 3)
  (check-equal? (min-x (cons (point 2 1) (cons (point 0 2) (cons (point 1 3) empty)))) 0)
  (check-equal? (min-x (cons (point 3 3) (cons (point 0 1) (cons (point 0 2) empty)))) 0)
  "all tests passed")
