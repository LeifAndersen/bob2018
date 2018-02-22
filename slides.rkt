#lang at-exp slideshow

(require (for-syntax syntax/parse)
         (prefix-in video: video/base)
         video/private/editor
         pict/color
         racket/gui/base
         slideshow/staged-slide
         slideshow/play
         slideshow/code
         slideshow/repl
         pict/shadow
         ppict/pict
         (only-in ppict/slideshow pslide-base-pict)
         ppict/slideshow2
         (prefix-in v: video/base)
         video/player
         images/icons/misc
         "assets.rkt"
         "logo.rkt"
         "demo.rkt"
         "elabdemo.rkt"
         "utils.rkt"
         "block.rkt"
         "character.rkt"
         "tower.rkt")

(set-page-numbers-visible! #f)
;(current-page-number-font (make-object font% 42 'default))
#;
(set-spotlight-style! #:size 100
                      #:color (make-object color% 64 64 0 0.66))

(define video-block (new block%
                         [label "Video"]
                         [shape (draw-colored-rect (light "blue"))]))

(define small-logo (scale-to-fit the-plain-logo 300 300))
(define tiny-logo (scale-to-fit the-plain-logo 150 150))
(define leif (new person%))

(define (bg-slide-assembler title sep content)
  (inset content (- margin) (- margin) 0 0))

(define ben (new person%
                 [hair-color "black"]
                 [hair-type 'short]
                 [skin-color "SaddleBrown"]
                 [outfit 'pants]
                 [bottom-color "gray"]
                 [top-color "red"]))

(define (make-a-dsl-slide [overlay #f]
                          #:cite [cite #f]
                          #:lower [lower 60]
                          #:sep [sep 10]
                          #:text-func [text-func t*]
                          #:carrot-offset [carrot-offset -35]
                          #:slogan [slogan 'dsl])
  (define prob (text-func "We have a problem..."))
  (define terms (vc-append (text-func "We want to solve it in the")
                           (text-func "problem domain's own language...")))
  (define solve*
    (match slogan
      ['dsl (lt "Make a DSL!")]
      ['spark make-a-dsl]
      ['tower tower-of-dsls]
      ['syntax-parse (vc-append
                      (blank 90)
                      syntax-parse-dsl
                      (blank 95))]
      ['linguistic-huh (vc-append
                        (blank 90)
                        (hc-append (mlt "? ")
                                   (scale linguistic-inheritance 1.4)
                                   (mlt " ?"))
                        (blank 100))]
      ['linguistic (vc-append
                    (blank 90)
                    (scale linguistic-inheritance 1.8)
                    (blank 95))]))
  (staged [start mid end]
          (pslide
           #:go (coord 1/2 1/2 'cc)
           (vc-append
            10
            (blank lower)
            (if (at/after start) prob (ghost prob))
            (blank sep)
            (if (at/after mid) terms (ghost terms))
            (if (at/after end) solve* (ghost solve*)))
           #:go (coord 0.505 0.14 'cc)
           (if overlay
               (vc-append
                carrot-offset
                (cond [(string? overlay)
                       (colorize (text overlay scribbly-font 42) "red")]
                      [else overlay])
                the-^)
               (blank 0))
           #:go (coord 1 1 'rb)
           (or (and (at/after end)
                    cite)
               (blank 1)))))

(define-syntax (make-phase-slide stx)
  (syntax-parse stx
    [(_ (args ...) (def val) ...)
     #:with (stripped-args ...) (datum->syntax #f (syntax->datum #'(args ...)))
     #:with (stripped-def ...) (datum->syntax #f (syntax->datum #'(def ...)))
     #'(play-n
        #:steps 20
        #:delay 0.025
        (λ (stripped-args ...)
          (define stripped-def val) ...
          (vc-append
           20
           (scale
            (cellophane
             (scale
              (vc-append
               25
               (disk 25)
               (blank 5)
               (disk 25)
               (blank 5)
               (disk 25)
               (mlt* "Phase 3")
               (mlt* "Phase 2"))
              (max 0.01 (* 0.8 n3)))
             n3)
            (- 1 (* n4 0.4)))
           (scale
            (scale
             (cc-superimpose
              (cellophane (scale (mlt* "Compile Time") (max 0.01 (- 1 n2)) 1)
                          (- 1 n2))
              (cellophane (scale (mlt* "Phase 1") (max 0.01 n2) 1)
                          n2))
             (- 1 (* 0.2 n3)))
            (- 1 (* n4 0.4)))
           (scale
            (scale
             (cc-superimpose
              (cellophane (scale (mlt* "Run Time") (max 0.01 (- 1 n1)) 1)
                          (- 1 n1))
              (cellophane (scale (mlt* "Phase 0") (max 0.01 n1) 1)
                          n1))
             (- 1 (* 0.2 n3)))
            (- 1 (* n4 0.4)))
           (cellophane
            (scale
             (vc-append
              25
              (mlt* "Phase -1")
              (mlt* "Phase -2")
              (disk 25)
              (blank 5)
              (disk 25)
              (blank 5)
              (disk 25))
             (max 0.01 (* 0.8 0.6 n4)))
            n4))))]))

;; ===================================================================================================
;; Section 1: Video the Language

(slide
 #:name "Section 1: Video the Language"
 (mt "Movies as Programs")
 (scale the-logo 0.4)
 (t "Leif Andersen"))

(slide
 (mt "Accessibility")
 (scale (code (prominent code)) 1.5)
 (code (some code))
 (apply hc-append
        (for/list ([color (list color-1 color-2 color-3)])
          (filled-rectangle 100 100 #:color color))))

(slide
 (send leif draw))

#|
I like to record things.
|#
(pslide
 #:go (coord 1/2 1/2 'cc #:compose hc-append)
 (send leif draw)
 small-logo)

#|
One day, I foolishly offered to record and edit the recordings for a conference.
|#
(pslide
 #:go (coord 0 1/2 'lc #:compose hc-append)
 (send leif draw)
 #:go (coord 0.45 0.35 'cc)
 tiny-logo
 #:go (coord 0.74 0.26 'rc)
 (scale the-microphone 1.7)
 #:go (coord 0.9 1/4 'rc #:compose hc-append)
 (scale (send ben draw) 0.66)
 #:go (coord 1 0.2 'rc)
 (desktop-machine 1.5))

#|
The recording went well, but then I needed to find a way to clean up the
recording and upload it to the internet. That means I needed to combine
three feeds, the presontor's video, the presontor's audio, and the presentor's
screen, into one video. Producing something like this.
|#
(let ()
  (define the-machine (desktop-machine 1.5))
  (define the-cloud (cc-superimpose
                     (cloud 400 125 #:style '(wide))
                     (scale the-? 0.3)))
  (staged [items arrows]
          (pslide #:go (coord 0 0 'lt)
                  the-machine
                  #:go (coord 0.5 0 'ct)
                  tiny-logo
                  #:go (coord 0.95 0 'rt)
                  the-microphone
                  #:go (coord 0.5 0.4 'cc)
                  (if (at/after arrows) the-cloud (ghost the-cloud))
                  #:go (coord 0.5 0.8 'cc)
                  (if (at/after arrows) cloud-demo (ghost cloud-demo))
                  #:set (let ([p ppict-do-state])
                          (if (at/after arrows)
                              (let* ([p (pin-arrow-line
                                         20 p
                                         the-machine cb-find
                                         the-cloud (λ (a b)
                                                     (let-values ([(x y) (ct-find a b)])
                                                       (values (- x 100) y)))
                                         #:start-angle (* pi -1/2)
                                         #:end-angle (* pi -1/2)
                                         #:line-width 8)]
                                     [p (pin-arrow-line
                                         20 p
                                         tiny-logo cb-find
                                         the-cloud ct-find
                                         #:line-width 8)]
                                     [p (pin-arrow-line
                                         20 p
                                         the-microphone cb-find
                                         the-cloud (λ (a b)
                                                     (let-values ([(x y) (ct-find a b)])
                                                       (values (+ x 100) y)))
                                         #:start-angle (* pi -1/2)
                                         #:end-angle (* pi -1/2)
                                         #:line-width 8)]
                                     [p (pin-arrow-line
                                         20 p
                                         the-cloud cb-find
                                         cloud-demo ct-find
                                         #:line-width 8)])
                                p)
                              p)))))

(slide the-small-scaled-nlve)

(play-n
 #:delay 0.01
 #:steps 100
 (λ (n)
   (define index (min (exact-floor (* n (length clock-list)))
                      (sub1 (length clock-list))))
   (scale (list-ref clock-list index) 2)))

#|
I had one video edited.
|#
(slide
 (lt "One down"))

#|
The problem is that this was a conference, not just one talk. So I still had
19 more edits to go.
|#
(slide
 (lt "One down")
 (lt "19 more to go..."))

(play-n
 #:delay 0.01
 #:steps 50
 (λ (n)
   (define index (min (exact-floor (* n (length clock-list)))
                      (sub1 (length clock-list))))
   (list-ref clock-wall-list index)))

(slide
 (mt "We Need Automation"))

(the-landscape-slide)

(make-a-dsl-slide #:slogan 'spark)

(slide
 (lt "Make a DSL!")
 (bitmap "res/racket-logo.png"))

(staged [lib lang]
        (slide
         (lang-lib #:inner (at/after lib)
                   #:outer (at/after lang))))

(play-n
 (λ (n1)
   (fade-pict n1
              (lang-lib #:outer #f)
              (part-circle))))

(slide
 (part-circle #:filters #f
              #:playlists #f
              #:multitracks #f))


(let ()
  (define p (mt "Producers"))
  (define rtype
    (scale
     (hc-append (tt "render : Producer → ")
                (file-icon 50 60 "bisque"))
     1.2))
  (define ctype (scale (tt "clip : String → Producer") 1.2))
  (define ex (hc-append (scale (hc-append (code (render (clip "demo.mp4"))) (tt " ⇒ ")) 1.2)
                        producer-demo))
  (staged [title type type2 example]
          (slide
           p
           (if (at/after type) rtype (ghost rtype))
           (if (at/after type2) ctype (ghost ctype))
           (if (at/after example) ex (ghost ex)))))

(slide
 (part-circle #:producers #f
              #:playlists #f
              #:multitracks #f))

(let ()
 (define prod1
   (send
    (new block%
         [label "Producer"]
         [label-scale 0.2]
         [shape (λ (w h)
                  (filled-rectangle w h #:color "SeaShell"))])
    draw 200 400))
 (define prod2
   (send
    (new block%
         [label "Producer"]
         [label-scale 0.2]
         [shape (λ (w h)
                  (filled-rectangle w h #:color "DeepPink"))])
    draw 200 400))
 (define filt
   (hc-append prod1
              (blank 500)
              prod2))
  (slide
   (pin-arrow-line 15 filt
                   prod1 rc-find
                   prod2 lc-find
                   #:label (mst "Filter")
                   #:line-width 5
                   #:style 'dot)))

(make-filter-slide #f)

(make-filter-slide #t)

(slide
 (part-circle #:producers #f
              #:filters #f
              #:multitracks #f))

(pslide
 #:go (coord 1/2 1/2 'cc)
 (hc-append
  25
  (send
   (new block%
        [label "Producer"]
        [label-scale 0.2]
        [shape (λ (w h)
                 (filled-rectangle w h #:color "orange"))])
   draw 150 350)
  (send
   (new block%
        [label "Producer"]
        [label-scale 0.2]
        [shape (λ (w h)
                 (filled-rectangle w h #:color (light "gold")))])
   draw 150 350)
  (send
   (new block%
        [label "Producer"]
        [label-scale 0.2]
        [shape (λ (w h)
                 (filled-rectangle w h #:color (light "purple")))])
   draw 150 350)
  (send
   (new block%
        [label "Producer"]
        [label-scale 0.2]
        [shape (λ (w h)
                 (filled-rectangle w h #:color (light "red")))])
   draw 150 350))
 #:go (coord 1/2 0.9 'cc)
 time-arrow)

(slide
 (code
  (playlist (clip "jumping.mp4")
            (clip "flying.mp4")))
 (mk-demo
  (v:playlist (v:clip "res/bbb/jumping.mp4")
              (v:clip "res/bbb/flying.mp4")
              (v:clip "res/bbb/jumping.mp4")
              (v:clip "res/bbb/flying.mp4")
              (v:clip "res/bbb/jumping.mp4")
              (v:clip "res/bbb/flying.mp4"))))

(play-n
 (λ (n)
   (ppict-do ((pslide-base-pict))
             #:go (coord 1/2 1/2 'cc)
             (cc-superimpose
              (hc-append
               25
               (send
                (new block%
                     [label "Producer"]
                     [label-scale 0.2]
                     [shape (λ (w h)
                              (filled-rectangle w h #:color "orange"))])
                draw 150 350)
               (send
                (new block%
                     [label "Producer"]
                     [label-scale 0.2]
                     [shape (λ (w h)
                              (filled-rectangle w h #:color (light "gold")))])
                draw 150 350)
               (send
                (new block%
                     [label "Producer"]
                     [label-scale 0.2]
                     [shape (λ (w h)
                              (filled-rectangle w h #:color (light "purple")))])
                draw 150 350)
               (send
                (new block%
                     [label "Producer"]
                     [label-scale 0.2]
                     [shape (λ (w h)
                              (filled-rectangle w h #:color (light "red")))])
                draw 150 350))
              (cellophane
               (send
                (new block%
                     [label "Transition"]
                     [label-scale 0.2]
                     [shape (λ (w h)
                              (filled-ellipse w h #:color "yellow"))])
                draw 200 350)
               n))
             #:go (coord 1/2 0.9 'cc)
             time-arrow)))

(slide
 (code
  (playlist (clip "jumping.mp4")
            (fade-transition 1)
            (clip "flying.mp4")))
 (mk-demo
  (v:clip "res/bbb/jumpflytrans.mp4")))

(slide
 (part-circle #:producers #f
              #:filters #f
              #:playlists #f))

(play-n
 (λ (n)
   (ppict-do ((pslide-base-pict))
             #:go (coord 0.55 1/2 'cc)
             (cc-superimpose
              (vc-append
               25
               (send
                (new block%
                     [label "Producer"]
                     [shape (λ (w h)
                              (filled-rectangle w h #:color (light "red")))])
                draw 600 75)
               (send
                (new block%
                     [label "Producer"]
                     [shape (λ (w h)
                              (filled-rectangle w h #:color "pink"))])
                draw 600 75)
               (send
                (new block%
                     [label "Producer"]
                     [shape (λ (w h)
                              (filled-rectangle w h #:color "yellow"))])
                draw 600 75)
               (send
                (new block%
                     [label "Producer"]
                     [shape (λ (w h)
                              (filled-rectangle w h #:color (light "green")))])
                draw 600 75))
              (cellophane
               (send
                (new block%
                     [label "Merge"]
                     [shape (λ (w h)
                              (filled-ellipse w h #:color "cyan"))])
                draw 600 150)
               n))
              #:go (coord 0.55 0.9 'cc)
              time-arrow
              #:go (coord 0.1 0.45 'cc)
             layers-arrow)))


(slide
 (scale
  (code (define WIDTH 1920)
        (define HEIGHT 1080)
        (multitrack (color "black")
                    (overlay-merge 0 0 (/ WIDTH 2) HEIGHT)
                    (clip "running.mp4")
                    (overlay-merge (/ WIDTH 2) 0 (/ WIDTH 2) HEIGHT)
                    (clip "flying.mp4")))
  0.8)
 (mk-demo (video:clip "res/bbb/split.mp4")))

(staged [lib lang]
        (slide
         (cc-superimpose
          (lang-lib #:inner (at/after lib)
                    #:outer (at/after lang))
          (part-circle))))

(vid-slide)

#;
(slide
 (mk-demo (video:clip "res/bbb/mosaic.mp4")))

(live-demo `(playlist
             (clip "res/bbb/mosaic.mp4")
             (clip "res/bbb/mosaic.mp4"))
           'horizontal
           @~a{
 #lang video
 ;; Create a mosaic of four videos
 (for/vertical ([i (in-range 2)])
   (for/horizontal ([j (in-range 2)])
     (external-video "branded.vid"
       (clip "logo.png")
       (clip (format "~aX~a.mp4" i j)))))})

(live-demo `(playlist
             (clip "res/bbb/mosaic.mp4")
             (clip "res/bbb/mosaic.mp4")
             (clip "res/bbb/mosaic.mp4"))
           'horizontal
           @~a{
 #lang video
 ;; Create a mosaic of four videos
 (for/vertical ([i (in-range 2)])
   (for/horizontal ([j (in-range 2)])
     (external-video "branded.vid"
       (clip "logo.png")
       (clip (format "~aX~a.mp4" i j)))))})

(live-demo `(playlist
             (clip "res/dragon.mp4")
             (clip "res/bbb/mosaic.mp4")
             (clip "res/bbb/mosaic.mp4")
             (clip "res/bbb/mosaic.mp4"))
           'horizontal
           @~a{
 #lang video
 (clip "dragon.mp4")
 ;; Create a mosaic of four videos
 (for/vertical ([i (in-range 2)])
   (for/horizontal ([j (in-range 2)])
     (external-video "branded.vid"
       (clip "logo.png")
       (clip (format "~aX~a.mp4" i j)))))})

(slide
 (hc-append
  75
  (vc-append
   script-clock
   (t* "Implementing Video")
   (t* "+ Editing"))
  (vc-append
   nlve-clock
   (t* "Manual Editing")
   (t* ""))))

;; ===================================================================================================
;; Section 2: Implementing a Language

(slide
 #:name "Section 2: Implementing a Language"
 (mt "From Libraries to")
 (lt "Languages"))

(slide
 (scale linguistic-inheritance 1.5))

(mk-tower-slide)

(slide
 (scale
  (code
   (for/playlist ([scene (in-list scene-list)])
     (multitrack scene
                 (overlay-merge 10 10 300 300)
                 (clip "logo.mp4"))))
  1.1))

(make-repl-slides
 #'(define (for/playlist seq body)
     (apply playlist
            (for/list ([i (in-list seq)])
              (body i))))
 #'(for/playlist (list (clip "a.mp4")
                       (clip "b.mp4"))
     (λ (scene)
       (multitrack scene
                   (overlay-merge 10 10 300 300)
                   (clip "logo.mp4")))))

(make-repl-slides
 (quote-syntax
  (define-macro (for/playlist seq . body)
    `(apply playlist
            (for/list ,seq
              ,@body))))
 #'(let ([playlist 42])
     (for/playlist ([s (list (clip "a.mp4"))])
       (multitrack s
                   (overlay-merge 10 10 300 300)
                   (clip "logo.mp4"))))
 #:middle
 (λ ()
   (staged (el ev)
     (define earrow (hc-append (t "⇒") (st " evaluates")))
     (define e (tt "#<playlist>"))
     (slide
      (vc-append
       25
       (code (for/playlist ([s (list (clip "a.mp4"))])
               (multitrack ...)))
       (hc-append (t "⇒") (st " elaborates"))
       (code (apply playlist
                    (for/list ([s (list (clip "a.mp4"))])
                      (multitrack ....))))
       (if (at/after ev) earrow (ghost earrow))
       (if (at/after ev) e (ghost e)))))
   (staged (e1 e2 e3)
     (define e2-arrow (hc-append (t "⇒") (st " elaborates")))
     (define e2-code
       (code (let ([playlist 42])
               (apply playlist ....))))
     (define e3-arrow (hc-append (t "⇒") (st " evaluates")))
     (define e3-code eval-bomb)
     (slide
      (vc-append
       25
       (code (let ([playlist 42])
               (for/playlist ....)))
       (if (at/after e2) e2-arrow (ghost e2-arrow))
       (if (at/after e2) e2-code (ghost e2-code))
       (if (at/after e3) e3-arrow (ghost e3-arrow))
       (if (at/after e3) e3-code (ghost e3-code)))))))

(make-repl-slides
 (quote-syntax
  (define-syntax-rule (for/playlist seq
                        body ...)
    (apply playlist
           (for/list seq
             body ...))))
 #'(let ([playlist 42])
     (for/playlist ([s (list (clip "a.mp4"))])
       (multitrack s
                   (overlay-merge 10 10 300 300)
                   (clip "logo.mp4")))))

(staged [def use exp]
  (define lang-file
   (codeblock-file "lang-extension.rkt" @~a{
 #lang racket
 (provide for/playlist)
 (define-syntax-rule (for/playlist seq
                       body ...)
   (apply playlist
          (for/list seq
            body ...)))}))
  (define user-file
   (codeblock-file "user-prog.rkt" @~a{
 #lang racket
 (require "lang-extension.rkt"
 (define playlist 42)
 (for/playlist ([i (list (clip "a.mp4")
                         (clip "b.mp4"))])
   (multitrack ....))}))
  (define exp-file
    (codeblock-file "user-prog.rkt" @~a{
 #lang racket
 (require "lang-extension.rkt"
 (define playlist 42)
 (apply playlist
        (for/list ([i (list (clip "a.mp4")
                            (clip "b.mp4"))])
          (multitrack ....)))}))
  (pslide
   #:go (coord 0.305 0.710 'cc)
   (if (at/after exp) (colorize (filled-rectangle 150 36) color-1) (blank))
   #:go (coord 0.29 0.760 'cc)
   (if (at/after exp) (colorize (filled-rectangle 150 36) color-3) (blank))
   #:go (coord 0.39 0.33 'cc)
   (if (at/after exp) (colorize (filled-rectangle 150 36) color-3) (blank))
   #:go (coord 1/2 0.25 'cc)
   lang-file
   #:go (coord 1/2 0.5 'ct)
   (cond
     [(at/after exp) exp-file]
     [(at/after use) user-file]
     [else (blank)])))

(slide
 (mt "Non-Local")
 (mt "Language Features"))

(let ()
  (define (mk-pieces)
    (define lang (codeblock-pict "#lang video\n"))
    (define logo-1 (code logo))
    (define logo-2 (code logo))
    (define deflogo (code (define logo ...)))
    (define talk (code talk))
    (define deftalk (code (define talk ...)))
    (define prov (code (provide vid)))
    (define vid (code (define vid (playlist
                                   #,(ghost logo-1)
                                   #,(ghost talk)
                                   #,(ghost logo-2)))))
    (values lang logo-1 logo-2 deflogo talk deftalk prov vid))
  (define-values (lva l1a l2a dla ta dta pa va)
    (mk-pieces))
  (define-values (lvb l1b l2b dlb tb dtb pb vb)
    (mk-pieces))
  (define-values (lv l1 l2 dl t dt p v)
    (mk-pieces))
  (define the-pict
    (ghost
     (cc-superimpose
      (vl-append
       lva
       (code
        #,l1a
        #,dla
        (code:blank)
        #,ta
        #,dta
        (code:blank)
        #,l2a))
      (vl-append
       lvb
       (code
        #,pb
        (code:blank)
        #,dlb
        #,dtb
        (code:blank)
        #,vb)))))
  (play-n
   #:steps 20
   #:delay 0.025
   (λ (n)
     (scale
      (let* ([_ (slide-pict the-pict lv lva lvb n)]
             [_ (slide-pict _ dt dta dtb n)]
             [_ (slide-pict _ dl dla dlb n)]
             [_ (pin-over _ pb lt-find (cellophane p n))]
             [_ (slide-pict _ t ta tb n)]
             [_ (slide-pict _ l1 l1a l1b n)]
             [_ (slide-pict _ l2 l2a l2b n)]
             [_ (pin-over _ vb lt-find (cellophane v n))])
        _)
      1.5))))

(pslide
 #:go (coord 1/2 0 'ct)
 (mt "Interposition Points")
 #:go (coord 1/2 1/2 'cc)
 (vc-append
  25
  (scale (code #%app) 2)
  (scale (code #%module-begin) 2)))

(slide
 (scale (code (+ 1 2)) 1.3)
 (=> "elaborates")
 (scale (code (#%app + 1 2)) 1.3))

(mk-modbeg-slide)

(mk-interpos-slide)

(staged [n h]
  (define defstx
    (if (at/after h)
        (cc-superimpose
         (colorize (filled-rectangle 250 30) "yellow")
         (code define-syntax))
        (code define-syntax)))
  (slide
   (scale
    (code
     (require syntax/wrapping-modbeg)
     (#,defstx video-module-begin
       (make-wrapping-module-begin ...)))
    1.3)))

(slide
 (scale
   (codeblock-pict #:keep-lang-line? #f @~a{
 #lang scribble/text
 #lang racket/base
 ... run time code ...
 
 (define-syntax macro-name
   ... compile time code ...)
   
 ... run time code ...))})
  1.4))

(slide
 (scale (code (define-syntax id expr)) 2)
 (blank 100)
 (hc-append (scale (code id) 1.5) (t " : run time binding"))
 (hc-append (scale (code expr) 1.5) (t " : compile time expression")))

;; ===================================================================================================
;; Section 3: Towers of Languages

(slide
 #:name "Section 3: Towers of Languages"
 (mt "Movies as Programs:")
 (mt "A Tower of Languages"))

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
  (define tower
    (freeze
     (mk-video-tower #:render-sp #f
                     #:render-ffi #f
                     #:render-video #t
                     #:render-ts #f
                     #:render-tv #f
                     #:render-scribble #f
                     #:render-viddoc #f
                     #:render-top #f)))
  (staged [a b]
    (slide
     (cc-superimpose
      tower
      (if (at/after b) t (ghost t))))))

(slide
 (scale ffi-cloud 2))

(pslide #:go (coord 1/2 1/3 'cc)
        video-architecture)

(make-a-dsl-slide "FFI"
                  #:lower -80
                  #:carrot-offset -20)

(let ()
  (define av-frame-get-buffer
    (let ()
      (define x (code av-frame-get-buffer))
      (cc-superimpose
       (colorize (filled-rectangle (+ (pict-width x) 5)
                                   (+ (pict-height x) 5))
                 "yellow")
       x)))
  (define mlt-ffi-code
    (scale
     (parameterize ([code-italic-underscore-enabled #f])
       (code (define-ffmpeg #,av-frame-get-buffer
               (_fun [frame : _av-frame] [align : _int]
                     -> [ret : _int]
                     -> (maybe-error? ret)))))
    1.2))
  (define mlt-ffi-short-code
    (scale
     (code (define-ffmpeg av-frame-alloc ...)
           (define-ffmpeg av-frame-free ...))
     1.2))
  (staged [c r]
          (pslide
           #:go (coord 1/2 0.1 'cc)
           (t "An FFI DSL")
           #:go (coord 0.37 0.275 'cc)
           (colorize (filled-rectangle 425 40) "yellow")
           #:go (coord 1/2 1/2 'cc)
           (scale (codeblock-pict #:keep-lang-line? #f @~a{
#lang scribble/base
int av_frame_get_buffer(AVFrame *frame,
                        int align);})
                  1.1)
           (blank 100)
           (if (at/after r) mlt-ffi-code (ghost mlt-ffi-code))
           #:go (coord 1 1 'rb)
           (st "(Scheme Workshop, 2004)")))
  (pslide
   #:go (coord 1/2 0.1 'cc)
   (t "An Object DSL")
   #:go (coord 1/2 1/2 'cc)
   mlt-ffi-short-code
   (blank 100)
   (scale
    (code
     (define-constructor clip video
       ... av-frame-alloc ...
           av-frame-free ...))
    1.2)))

(slide
 (mk-video-tower #:render-sp #f
                 #:render-scribble #f
                 #:render-ts #f
                 #:render-tv #f
                 #:render-viddoc #f
                 #:render-top #f))

(slide
 (scale doc-cloud 1.5))

(make-a-dsl-slide "Documentation"
                  #:lower -80
                  #:carrot-offset -20)

(let ()
  (define scrib
            (scale (codeblock-pict #:keep-lang-line? #f @~a|{
#lang scribble/manual
#lang video/documentation
@title{Video: The Language}
@(defmodulelang video)

Video Language (or VidLang, sometimes referred
to as just Video) is a DSL for editing...videos.
It aims to merge the capabilities of a traditional}|)
         0.80))
  (staged [d c]
          (pslide
           #:go (coord 1/2 0.01 'ct)
           (st* "A Documentation DSL")
           #:go (coord 1/2 1/2 'cc)
           (vc-append
            60
            (scale (bitmap "res/docs.png") 0.6)
            (if (at/after c) scrib (ghost scrib)))
           #:go (coord 1 1 'rb)
           (st "(ICFP, 2009)"))))

(slide
 (freeze (scale (bitmap "res/tools.png") 0.58)))

(slide
 (mk-video-tower #:render-sp #f
                 #:render-ts #f
                 #:render-tv #f
                 #:render-top #f))

(slide
 (scale type-cloud 2))

(play-n
 #:steps 20
 #:delay 0.025
 (λ (n1 n2)
   (cc-superimpose
    (scale
     (fade-around-pict
      n1
      (code (clip "clip.mp4"
                  #:start 0
                  #:end 50))
      (λ (x)
        (code (cut-producer #,x
                            #:start 0
                            #:end 100))))
     1.5)
    (cellophane the-X n2))))

(let ()
  (define a (bitmap "res/prod-type.png"))
  (define b (scale (bitmap "res/clip-type.png") 1));0.6))
  (define c (scale (bitmap "res/playlist-type.png") 0.55))
  (define d (scale (bitmap "res/kind-rule.png") 0.6))
  (define c1 (scale (codeblock-pict #:keep-lang-line? #f @~a|{
#lang racket
(define-typed-syntax (clip f) ≫
  [⊢ f ≫ _ ⇐ File] #:where n (length f)
  -------------------------------------
  [⊢ (untyped:clip f) ⇒ (Producer n)])}|)
 1.1))
  (staged [rule]
          (pslide
           #:go (coord 1/2 0.1 'cc)
           (t "A Typed DSL")
           #:go (coord 1/2 1/2 'cc)
           a))
  (make-a-dsl-slide "Types"
                  #:lower -80
                  #:carrot-offset -20)
  (staged [example1 code1]
          (pslide
           #:go (coord 1/2 0.1 'cc)
           (if (at example1)
               (t "A Typed DSL")
               (t "A Type Implementation DSL"))
           #:go (coord 1/2 1/2 'cc)
           (if (at/after example1) b (ghost b))
           (blank 100)
           (if (at/after code1) c1 (ghost c1))
           #:go (coord 1 1 'rb)
           (st "(POPL, 2017)"))))

(slide
 (mk-video-tower #:render-sp #f
                 #:render-top #f))

(make-a-dsl-slide "DSL"
                  #:slogan 'syntax-parse
                  #:cite (st "(ICFP, 2010)"))

(make-repl-slides
 (quote-syntax
  (define-syntax-rule
    (define/playlist (name args ...)
      body ...)
    (define name
      (λ (args ...)
        (playlist body ...)))))
 #'(define/playlist (double A)
     A
     A)
 #'(define/playlist (double (A B C))
     A))

(make-repl-slides
 #:background
 (ppict-do ((pslide-base-pict))
           #:go (coord 0.700 0.41 'cc)
           (colorize (filled-rectangle 480 40) "yellow"))
 (quote-syntax
  (define-simple-macro
    (define/playlist header:function-header
      body ...)
    (define header.name
      (λ header.args
        (playlist body ...)))))
  #'(define/playlist (double A)
     A
     A)
 #'(define/playlist (double (A B C))
     A))

(slide
 (mk-video-tower #:render-top #f))

(let ()
  (define the-video (video:clip (build-path h "res" "demo.mp4")))
  (define the-logo (video:clip (build-path h "res" "racket-logo.png")))
  (define the-logo-video (video:clip (build-path h "res" "racket-logo.mp4")))
  (define vps (new video-player-server%
                   [video
                    (video:playlist (video:cut-producer the-logo-video
                                                        #:start 0
                                                        #:end 5)
                                    the-video)]))
  (slide
   (hc-append
    50
    (interactive (blank 650 350)
                 (λ (frame)
                   (define editor
                     (new video-editor%
                          [track-height 100]
                          [initial-tracks 3]))
                   (define vt (new video-text%))
                   (send vt insert "(clip \"talk.mp4\")")
                   (define vt2 (new video-text%))
                   (send vt2 insert "(clip \"logo.png\")")
                   (send editor insert-video vt 0 0 400)
                   (send editor insert-video vt2 1 0 100)
                   (define canvis
                     (new editor-canvas%
                          [parent frame]
                          [editor editor]))
                   (λ () (void))))
    (vc-append
     35
     (interactive (blank 150 50)
                  (λ (frame)
                    (new button%
                         [label "Start"]
                         [parent frame]
                         [font (make-object font% 50 'default)]
                         [callback (λ _ (send vps play))]
                         [stretchable-width #t]
                         [stretchable-height #t])
                    (λ () (void))))
     (interactive (blank 150 50)
                  (λ (frame)
                    (new button%
                         [label "Stop"]
                         [parent frame]
                         [font (make-object font% 50 'default)]
                         [stretchable-width #t]
                         [stretchable-height #t])
                    (λ () (void))))))
   (interactive (blank 640 360)
                (λ (frame)
                  (send frame show #t)
                  (define screen (new video-canvas%
                                      [parent frame]
                                      [width (send frame get-width)]
                                      [height (send frame get-height)]))
                  (send vps set-canvas screen)
                  (send vps render-audio #f)
                  (λ ()
                    (thread
                     (λ ()
                       (send vps stop)))
                    (send frame show #f))))))

(slide
 (scale (bitmap "res/vidgui2.png") 0.8))

;; ===================================================================================================
;; Section 4: The future, editor-oriented programming

(slide
 #:name "Section 4: The future, editor-oriented programming"
 (mt "Editor-Oriented")
 (mlt "Programming"))

(slide
 (mt "The Future..."))

(play-n
 #:steps 40
 #:delay 0.025
 (λ (n1-2)
   (define n1 (max 0.001 (min 1 (* n1-2 2))))
   (define n2 (max 0.001 (- (* n1-2 2) 1)))
   (vc-append
    50
    (fade-pict
     n1
     (scale (scale (code begin-for-syntax) 2.3) (- 1 n1) 1)
     (scale (scale (code begin-for-editor) 2.3) n1 1))
    (fade-pict
     n2
     (scale (scale (code define-syntax) 2.3) (- 1 n2) 1)
     (scale (scale (code define-editor) 2.3) n2 1)))))

(slide
 (vl-append
  25
  (codeblock-pict
   "#lang editor")
  (code (define-editor video-editor ...))
  (code ...)
  (hb-append
   (codeblock-pict #:keep-lang-line? #f "#lang racket\n(play ")
   (interactive (blank 650 350)
                (λ (frame)
                  (define editor
                    (new video-editor%
                         [track-height 100]
                         [initial-tracks 3]))
                  (define vt (new video-text%))
                  (send vt insert "(clip \"talk.mp4\")")
                  (define vt2 (new video-text%))
                  (send vt2 insert "(clip \"logo.png\")")
                  (send editor insert-video vt 0 0 400)
                  (send editor insert-video vt2 1 0 100)
                  (define canvis
                    (new editor-canvas%
                         [parent frame]
                         [editor editor]))
                  (λ () (void))))
   (codeblock-pict #:keep-lang-line? #f "#lang racket\n)"))))

(slide
 (mk-video-tower))

(slide
 (freeze (scale (bitmap "res/slidesrc.png") 0.36)))

(play-n
 #:steps 20
 #:delay 0.025
 (λ (n)
   (define web (freeze (scale (bitmap "res/website.png") 0.36)))
   (define src (freeze (scale (bitmap "res/website-src.png") 0.36)))
   (vc-append
    (scale (tt "https://lang.video") 1.5)
    (fade-pict n web src))))

(pslide
 #:go (coord 0.01 0.99 'lb)
 (freeze (scale (bitmap "res/fear-of-macros.png") 0.4))
 #:go (coord 0.99 0.01 'rt)
 (freeze (scale (bitmap "res/beautiful-racket.png") 0.4)))

(pslide
 #:go (coord 0.01 0.01 'lt)
 (freeze (shadow-frame (scale (bitmap "res/lang-as-lib.png") 0.3)
                       #:margin 0))
 #:go (coord 0.99 0.99 'rb)
 (freeze (shadow-frame (scale (bitmap "res/want-it-when.png") 0.35)
                       #:margin 0)))

(end-slide)
