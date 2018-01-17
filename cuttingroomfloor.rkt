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


|#
