#|
  (pslide
   #:go (coord 1/2 0.1 'ct)
   (t "Interposition Points")
   #:go (coord 1/2 0.3 'ct)
   (pin-arrow-line
    15
    (ht-append
     rmod
     (blank 200)
     rpmod)
    rmod (λ (a b)
           (let-values ([(x y) (rt-find a b)])
             (values (- x 5) (+ y 150))))
    rpmod (λ (a b)
            (let-values ([(x y) (lt-find a b)])
              (values (+ x 45) (+ y 150))))
    #:line-width 5
    #:label (t "elaborates"))))



  (pslide
   #:go (coord 1/2 1/2 'cc)
   (pin-arrow-line
    15
    (ht-append
     v-code
     (blank 230)
     r-code)
    v-code (λ (a b)
             (let-values ([(x y) (rt-find a b)])
               (values (- x 50) (+ y 150))))
    r-code (λ (a b)
             (let-values ([(x y) (lt-find a b)])
               (values x (+ y 150))))
    #:line-width 5
    #:label (t "elaborates*")))



(slide
 (mst "Plugins")

 (codeblock-pict @~a{
bl_info = {"name": "Plugin Name",
           "category": "Object"}
def register():
    ...
def unregister():
    ...}))

(slide
 (mst "Macros")
 (codeblock-pict @~a{
tell application "iMovie"
  activate
end tell

-- Start recording
tell application "System Events"
  tell process "iMovie"
    tell window 1
      ...
    end tell
  end tell
end tell}))

(slide
 (mst "Shell Scripts")
 (codeblock-pict @~a{
ffmpeg -f lavfi -i testsrc \
-f lavfi -i testsrc ... \
-filter_complex \
"[1:v]negate[a]; \
 [2:v]hflip[b]; \
 [3:v]edgedetect[c]; \
 [0:v][a]hstack=inputs=2[top]; \
 [b][c]hstack=inputs=2[bottom]; \
 [top][bottom]vstack=inputs=2[out]" \
-map "[out]" -c:v ffv1 -t 5 output.avi}))

(slide
 (mst "Functional eDSL")
 (bitmap
  (pict->bitmap (scale the-logo 0.45)
                #:make-bitmap make-monochrome-bitmap)))


(slide
 (t* "Linguistic Interposition Points:")
 (scale (code #%app) 1.8)
 (scale (code #%lambda-begin) 1.8)
 (scale (code #%module-begin) 1.8))

(let ()
  (define blk (scale (codeblock-pict @~a{
 #lang racket
 (playlist (clip "demo.mp4")
           (clip "logo.png"))}) 1.1))
  (define step-to (scale (tt "⇒") 1.1))
  (define new-blk (scale (codeblock-pict @~a{
 #lang video
 (clip "demo.mp4")
 (clip logo.png")}) 1.1))
  (define really-new-blk (scale (codeblock-pict @~a{
 #lang video
 the-talk
 (clip "logo")
 ;; Where
 (define the-talk
   (sepia-filter (clip "demo.mp4")))})
                                1.1))
  (staged [block new-block really-new-block]
          (slide
           (vl-append
            25
            blk
            (if (at/after new-block) step-to (ghost step-to))
            (if (at/after new-block) new-blk (ghost new-blk))
            (if (at/after really-new-block) step-to (ghost step-to))
            (if (at/after really-new-block) really-new-blk (ghost really-new-blk))))))

(slide
 (codeblock-pict @~a{
 #lang video #:width 640 #:height 480
 the-talk
 (clip "logo")
 ;; Where
 (define the-talk
   (sepia-filter (clip "demo.mp4")))})
 sepia-demo)

(slide
 (scale (tt "external-video : String -> Producer") 1.2)
 (scale (code (render (external-video "movie.vid"))) 1.2))

(slide
 (mk-video-tower #:render-ffi #f
                 #:render-video #f
                 #:render-ts #f
                 #:render-tv #f
                 #:render-scribble #f
                 #:render-viddoc #f
                 #:render-top #f))

(slide
 (mt "Syntax Parse")
 (t "A DSL for making DSLs"))

(play-n
 #:steps 20
 #:delay 0.025
 (λ (n1)
   (fade-around-pict
    n1
    (scale (code define-mlt) (- 2 n1))
    (λ (x)
      (code (#,x mlt-factory-init
                 (_fun [p : _path]
                       -> [ret : _mlt-repository/null]
                       -> (or ret
                              (error 'mlt-factory-init
                                     "Could not find ~a"
                                     p)))))))))
(slide
 (code
  (define-syntax (~module-begin stx)
    (syntax-parse stx
      #,elided))))

(slide
 (code
  (define-syntax (~module-begin stx)
    (syntax-parse stx
      [(_ code ...)
       #,elided]))))

(slide
 (code
  (define-syntax (~module-begin stx)
    (syntax-parse stx
      [(_ code ...)
       (#%module-begin
        (video-begin vid (provide vid) ()
                     code ...))]))))

(slide
 (mk-video-tower #:render-video #f
                 #:render-ts #f
                 #:render-tv #f
                 #:render-scribble #f
                 #:render-viddoc #f
                 #:render-top #f))
(slide
 (mk-video-tower #:render-ts #f
                 #:render-tv #f
                 #:render-scribble #f
                 #:render-viddoc #f
                 #:render-top #f))

(pslide #:go (coord 1/4 1/4 'cc)
        (rotate (t "Producers") (* pi 1/4))
        #:go (coord 3/4 1/4 'cc)
        (rotate (t "Transitions") (* pi -1/4))
        #:go (coord 1/4 3/4 'cc)
        (rotate (t "Playlists") (* pi -1/4))
        #:go (coord 3/4 3/4 'cc)
        (rotate (t "Filters") (* pi 1/4))
        #:go (coord 1/2 4/5 'cc)
        (rotate (t "Multitracks") (* pi 1/8))
        #:go (coord 1/2 4/10 'cc)
        the-lambda)

(let ()
  (define a (bitmap "res/prod-type.png"))
  (define b (scale (bitmap "res/clip-type.png") 0.6))
  (define c (scale (bitmap "res/playlist-type.png") 0.55))
  (define d (scale (bitmap "res/kind-rule.png") 0.6))
  (staged [rule]
          (slide
           a))
  (staged [example1 example2]
          (slide
           (if (at/after example1) b (ghost b))
           (if (at/after example2) c (ghost c))))
  (staged [kind]
          (slide
           (if (at/after kind) d (ghost d)))))
(let ()
  (define m (mt "Multitracks"))
  (define mtype (scale (vc-append (tt "multitrack : (∪ Producer Transition) ...")
                                  (tt "→ Producer"))
                       1))
  (define ex (scale (code (render
                           (multitrack (clip "demo.mp4")
                                       (overlay-transition 0 0 100 100)
                                       (clip "logo.mp4"))))
                    1))
  (define r (hc-append (tt "⇒ ") multitrack-demo))
  (staged [title type example res]
          (slide
           m
           (if (at/after type) mtype (ghost mtype))
           (if (at/after example) ex (ghost ex))
           (if (at/after res) r (ghost r)))))
(slide
 (cc-superimpose
  (mk-video-tower #:render-racket #f
                  #:render-sp #f
                  #:render-ffi #f
                  #:render-video #f
                  #:render-ts #f
                  #:render-tv #f
                  #:render-scribble #f
                  #:render-viddoc #f
                  #:render-top #f)
  (vc-append
   25
   (blank 25)
   (colorize (lt "Video:") "white")
   (vc-append
    (colorize (mst "A Better") "white")
    (colorize (mst "Approach") "white")))))

(live-demo video 'vertical #:split-location 1/4 @~a{
 #lang video
 })

(slide
 (ht-append
  25
  (vc-append
   0
   (t* "Playlists")
   (st "Composes producers sequentially")
   (blank 15)
   (code (playlist
          (clip "demo.mp4")
          (fade-transition 3)
          (clip "logo.png")))
   (blank 90)
   playlist-fade-demo)
  (vc-append
   (t* "Multitracks")
   (st "Composes producers concurrently")
   (blank 15)
   (code (multitrack
          (clip "demo.mp4")
          (overlay-transition
           #:x 0   #:y 0
           #:w 100 #:h 100)
          (clip "logo.png")))
   (blank 15)
   multitrack-demo)))

(live-demo logoed-video 'horizontal #:split-location 2/3 @~a{
 #lang video
 (clip "logo.png" #:length 5)
 (fade-transition 1)
 (clip "talk.mp4")
 })

(live-demo full-video 'horizontal #:split-location 2/3 @~a{
 #lang video
 (cut the-logo #:length 5)
 (define the-logo
   (clip "logo.png"))

 (multitrack
   (clip "talk.mp4")
   (overlay 0 0 100 100)
   the-logo)})


(slide
 (code
  (define-syntax lazy-modbeg
    (make-wrapping-module-begin
     #'force #'#%module-begin))))

(slide
 (code
  (define-syntax lazy-modbeg
    (λ (stx) ... body ...))))

(make-phase-slide (n1 n2 n3)
                  (n4 0))

(staged [norm def]
        (define modw
          (code
           (begin-for-syntax
             (define-syntax make-wrapping-module-begin
               (λ (stx) ...)))))
        (slide
         (vl-append
          100
          (if (at/after def) modw (ghost modw))
          (code
           (define-syntax lazy-modbeg
             (make-wrapping-module-begin
              #'force #'#%module-begin))))))

(make-phase-slide (n4)
                  (n1 1)
                  (n2 1)
                  (n3 1))

(slide
 (scale
 (codeblock-pict @~a{
 #lang racket/base
 (provide make-wrapping-module-begin)
 ...
 (define-syntax
   make-wrapping-module-begin ...)})
 1.3))

(make-repl-only-slide
 '(module foo racket
    (require (for-syntax syntax/parse)
             syntax/parse/define
             syntax/wrap-modbeg)
    (define-syntax #%lazy-module-begin
      (make-wrapping-module-begin
       #'force #'#%module-begin))
    (define-syntax-rule (~app a b ...)
      (lazy (#%app a (lazy b) ...)))
    (define-syntax-rule (#%lazy-top-interaction . form)
      (#%top-interaction . (force form)))
    (provide (rename-out [#%lazy-module-begin #%module-begin]
                         [#%lazy-top-interaction #%top-interaction]
                         [~app #%app])))
 '(require 'foo)
 #:init #'(+ 1 2))
 
(slide
 (freeze (scale (bitmap "res/want-it-when.png") 0.45)))

(staged [f b]
  (define bomb (bitmap (bomb-icon #:height 200
                                  #:bomb-color "red")))
  (define ev (hc-append (t "⇒") (st "evaluates")))
  (slide
   (vc-append
    25
    (scale (code (+ (delay 1) (delay 2))) 1.2)
    (hc-append (t "⇒") (st "evaluates"))
    (scale (codeblock-pict #:keep-lang-line? #f @~a{
 #lang racket
 (+ #<promise> (delay 2)}) 1.2)
    (if (at/after b) ev (ghost ev))
    (if (at/after b) bomb (ghost bomb)))))

(let ()
  (define strictify
    (code
     (define (strictify f)
       (lambda args
         (apply f (map force args))))))
  (define str+
    (code
     (define lazy-+ (strictify +))))
  (staged [s+]
          (slide
           (scale
            (vl-append
             25
             strictify
             (if (at/after s+) str+ (ghost str+)))
            1.3))))

(let ()
  (define v (send video-block draw 600 100))
  (define m (send mlt-block draw 250 100))
  (define f (send ffmpeg-block draw 250 100))
  (define t
    (freeze
     (ppict-do (blank 900 700)
               #:go (coord 0.5 0.65 'cc)
               (scale ffi-cloud 1.7)
               #:go (coord 0.3 0.35 'cc)
               (scale (rotate doc-cloud (* pi 1/6)) 1.2)
               #:go (coord 0.75 0.4 'cc)
               (scale type-cloud 1.55))))
  (play-n
   #:steps 20
   #:delay 0.01
   #:skip-first? #t
   (λ (n1 n2)
     (ppict-do (blank 600 600)
               #:go (coord 3/4 n1 'cb)
               (if (= n1 0) (ghost f) f)
               #:go (coord 1/4 n1 'cb)
               (if (= n1 0) (ghost m) m)
               #:go (coord 1/2 (* n1 1/6) 'cb)
               (if (= n1 0) (ghost v) v)
               #:go (coord 1/2 1/2)
               (cellophane t n2)))))

(make-repl-slides
 #'(define-macro (or a b)
     (define tmp (gensym))
     `(let ([,tmp ,a])
        (if ,tmp ,tmp ,b)))
  #'(let ([tmp #t])
      (or #f tmp))
 #'(begin
     (define-macro (let asn body)
       body)
     (or 42 "puppy")))

(staged [def use]
        (define the-use
          (code (first 42
                       (let loop ()
                         (loop)))
                (code:comment "=> Infinite Loop")))
        (slide
         (scale
          (vl-append
           (codeblock-pict "#lang racket")
           (code
            (define (first x y) x)
            code:blank
            #,(if (at/after use) the-use (ghost the-use))))
          1.4)))

(slide
 (scale
  (vl-append
   (rectify-pict (codeblock-pict "#lang lazy") "yellow")
   (code
    (define (first x y) x)
    code:blank
    (first 42
           (let loop ()
             (loop)))
    (code:comment "=> 42")))
  1.4))

(let ()
  (define rapp
    (cc-superimpose (colorize (filled-rectangle 100 30) color-3)
                    (code #%app)))
  (define lapp
    (cc-superimpose (colorize (filled-rectangle 150 30) color-2)
                    (code lazy-app)))
  (define napp
    (cc-superimpose (colorize (filled-rectangle 100 30) color-1)
                    (code #%app)))
  (define lazy-app
    (code
     (define-syntax-rule (#,lapp rator rand ...)
       (delay (#,rapp rator (delay rand) ...)))))
  (define renamer
    (code
     (provide
      (except-out (all-from-out racket/base) #,rapp)
      (rename-out [#,lapp #,napp]))))
  (staged [app prov]
          (slide
           (vl-append
            25
            (codeblock-pict "#lang racket")
            (if (at/after prov) renamer (ghost renamer))
            lazy-app))))


(staged [f b]
  (define bomb (bitmap (bomb-icon #:height 200
                                  #:bomb-color "red")))
  (define ev (hc-append (t "⇒") (st "evaluates")))
  (slide
   (vc-append
    25
    (scale (code (+ (delay 1) (delay 2))) 1.2)
    (hc-append (t "⇒") (st "evaluates"))
    (scale (codeblock-pict #:keep-lang-line? #f @~a{
 #lang racket
 (+ #<promise> (delay 2)}) 1.2)
    (if (at/after b) ev (ghost ev))
    (if (at/after b) bomb (ghost bomb)))))

(let ()
  (define strictify
    (code
     (define (strictify f)
       (lambda args
         (apply f (map force args))))))
  (define str+
    (code
     (define lazy-+ (strictify +))))
  (staged [s+]
          (slide
           (scale
            (vl-append
             25
             strictify
             (if (at/after s+) str+ (ghost str+)))
            1.3))))

(make-repl-only-slide
 '(module foo racket
    (define-syntax-rule (~app a b ...)
      (lazy (#%app a (lazy b) ...)))
    (provide (rename-out [~app #%app])))
 '(require 'foo)
 #:init #'(+ 1 2))

(play-n
 #:steps 20
 #:delay 0.025
 (λ (n)
   (vc-append
    25
    (scale (code (+ 1 2)) 1.2)
    (hc-append (t "⇒") (st "elaborates"))
    (fade-around-pict
     n
     (scale
      (cc-superimpose
       (colorize (filled-rectangle 580 35) (light "orange"))
       (code (delay (+ (delay 1) (delay 2)))))
      1.2)
     (λ (p)
       (scale (code (force #,(scale p (/ 1 1.2)))) 1.2)))
    (hc-append (t "⇒") (st "evaluates"))
    (scale
     (fade-pict n
                (tt "#<promise>")
                (code 3))
     1.2))))

(make-repl-only-slide
 '(module foo racket
    (require (for-syntax syntax/parse)
             syntax/parse/define
             syntax/wrap-modbeg)
    (define-syntax #%lazy-module-begin
      (make-wrapping-module-begin
       #'force #'#%module-begin))
    (define-syntax-rule (~app a b ...)
      (lazy (#%app a (lazy b) ...)))
    (define-syntax-rule (#%lazy-top-interaction . form)
      (#%top-interaction . (force form)))
    (define (lazy-+ . args)
      (apply + (map force args)))
    (provide (rename-out [lazy-+ +]
                         [#%lazy-module-begin #%module-begin]
                         [#%lazy-top-interaction #%top-interaction]
                         [~app #%app])))
 '(require 'foo)
 #:init #'(+ 1 2))

|#

