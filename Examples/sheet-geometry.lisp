(in-package #:clim-user)

(defparameter *redisplay-counter* 0)
(defparameter *sheet->stream-transformation*
  (compose-transformation-with-translation
   (make-scaling-transformation 1 1) 100 100))

(defclass tracked-sheet-mixin () ())

(defclass my-pane (basic-pane  clime:always-repaint-background-mixin) ())
(defclass my-bbrd (bboard-pane clime:always-repaint-background-mixin) ())

(define-presentation-type translate-handler ())
(define-presentation-type scale-handler     ())
(define-presentation-type rotate-handler    ())
(define-presentation-type reset-handler     ())

;;; This handler is used to either move or translate the sheet. Conceptually
;;; both operations are equivalent.
(define-presentation-type move-handler ())

;;; This handler is used to either resize or scale the sheet. Resizing the
;;; sheet region doesn't change the sheet transformation while scaling it
;;; does, so both operations have a different effect.
(define-presentation-type resize-handler ())

(defun draw-handler-label (stream text x y align-x align-y)
  (surrounding-output-with-border (stream :shape :rectangle
                                          :background +light-yellow+)
    
    (draw-text* stream text x y
                :align-x align-x :align-y align-y)))

(define-presentation-method present
    (object (type my-pane) stream view &key)
  (declare (ignore type view))
  (let* ((region (sheet-region object))
         (transformation (sheet-transformation object)))
    
    (with-bounding-rectangle* (x1 y1 x2 y2)
        (transform-region transformation region)
      (draw-design stream region :ink +light-blue+
                                 :filled t :transformation transformation)
      (draw-rectangle* stream x1 y1 x2 y2 :ink +deep-pink+ :filled nil)
      (with-output-as-presentation (stream object 'move-handler)
        (draw-handler-label stream "Move" x1 y1 :left :top))
      (with-output-as-presentation (stream object 'rotate-handler)
        (draw-handler-label stream "Rotate" x2 y1 :right :top))
      (with-output-as-presentation (stream object 'resize-handler)
        (draw-handler-label stream "Resize" x2 y2 :right :bottom))
      (with-output-as-presentation (stream object 'reset-handler)
        (draw-handler-label stream "Reset" x1 y2 :left :bottom)))
    (with-bounding-rectangle* (x1 y1 x2 y2) region
      (draw-arrow* stream (+ x1 30) (+ y1 30) (- x2 30) (+ y1 30)
                   :ink +blue+ :line-thickness 2 :transformation transformation)
      (draw-arrow* stream (+ x1 30) (+ y1 30) (+ x1 30) (- y2 30)
                   :ink +blue+ :line-thickness 2 :transformation transformation))))

(define-gesture-name :reset-sheet :pointer-button-press (:middle :control))
(define-gesture-name :zoom-in :pointer-scroll (:wheel-up :control))
(define-gesture-name :zoom-out :pointer-scroll (:wheel-down :control))
(define-gesture-name :rotate-left :pointer-scroll (:wheel-up :shift))
(define-gesture-name :rotate-right :pointer-scroll (:wheel-down :shift))
(define-gesture-name :drag-start :pointer-button-press (:left :shift))

(define-command-table sheet-geometry-table)

(define-command (cmd-scale-sheet :command-table sheet-geometry-table)
    ((object 'my-pane)
     (scale-x 'real)
     (scale-y 'real)
     (origin-x 'real)
     (origin-y 'real))
  (setf (sheet-transformation object)
        (compose-transformations
         (sheet-transformation object)
         (make-scaling-transformation* scale-x scale-y origin-x origin-y))))

(define-command (cmd-translate-sheet :command-table sheet-geometry-table)
    ((object 'my-pane)
     (dx 'real)
     (dy 'real))
  (setf (sheet-transformation object)
        (compose-transformations
         (sheet-transformation object)
         (make-translation-transformation dx dy))))

(define-command (cmd-rotate-sheet :command-table sheet-geometry-table)
    ((object 'my-pane)
     (angle 'real)
     (origin-x 'real)
     (origin-y 'real))
  (setf (sheet-transformation object)
        (compose-transformations
         (sheet-transformation object)
         (make-rotation-transformation* angle origin-x origin-y))))

(define-command (cmd-reset-sheet :command-table sheet-geometry-table)
    ((object 'my-pane))
  (let ((region (case (menu-choose
                       '(circle square triangle)
                       :printer
                       #'(lambda (item stream)
                           (clim:with-drawing-options (stream :ink clim:+dark-red+)
                             (case item
                               (circle (clim:draw-circle* stream 0 0 10))
                               (square (clim:draw-rectangle* stream 0 0 20 20))
                               (triangle (clim:draw-polygon*
                                          stream '(10 8 0 -10 -10 8)))))))
                  (circle (make-ellipse* 100 100 100 0 0 100))
                  (triangle (make-polygon* '(100 0 200 200 0 200 100 0)))
                  (square (make-rectangle* 0 0 200 200))
                  (otherwise (return-from cmd-reset-sheet)))))
    (setf (sheet-transformation object)
          (make-translation-transformation 50 50))
    (setf (sheet-region object) region)))

(define-command (cmd-move-sheet :command-table sheet-geometry-table)
    ((object 'my-pane)
     (x 'real)
     (y 'real))
  (move-sheet object x y))

(define-command (cmd-resize-sheet :command-table sheet-geometry-table)
    ((object 'my-pane)
     (w 'real)
     (h 'real))
  (resize-sheet object w h))

(define-presentation-translator |-> reset sheet|
    (reset-handler command global-command-table)
    (object)
  `(cmd-reset-sheet ,object))

(defun show-hierarchy (frame pane)
  (format pane "redisplay counter: ~d" (incf *redisplay-counter*))
  (with-bounding-rectangle* (x1 y1 x2 y2) pane
    (draw-arrow* pane 100 25 100 (- y2 25) :ink +red+ :line-thickness 3)
    (draw-arrow* pane 25 100 (- x2 25) 100 :ink +red+ :line-thickness 3)
    ;; FIXME order of operations in WITH-DRAWING-OPTIONS should depend on the
    ;; order in which they appear (i.e first clipping, then transformation).
    ;;
    ;; FIXME text "redisplay counter" is clipped on consecutive displays despite
    ;; being draw outside of the clipping context.
    #+ (or)
    (with-drawing-options
        (pane :clipping-region (make-rectangle* -175 -175 575 575)
              :transformation *sheet->stream-transformation*)
      (present (app* frame)))
    ;;
    ;; FIXME when we use a clipping region, the stream recomputes its size to be
    ;; bigger neverless.
    (with-drawing-options (pane :clipping-region (sheet-region pane))
      (with-drawing-options
          (pane :transformation *sheet->stream-transformation*)
        (present (find-pane-named frame 'app))))))

(defun draw-pane (pane)
  (with-scaling (pane 1.5 1.5)
    (draw-rectangle* pane -1000 -1000 1000 1000 :ink +light-sea-green+)
    (draw-rectangle* pane 0 0 100 100 :ink +light-steel-blue+)
    (draw-rectangle* pane 10 10 20 20 :ink +dark-red+)
    (draw-rectangle* pane 80 10 90 20 :ink +dark-cyan+)
    (draw-rectangle* pane 80 80 90 90 :ink +dark-green+)
    (draw-rectangle* pane 10 80 20 90 :ink +dark-blue+)
    (draw-arrow* pane 30 30 70 70 :ink +deep-pink+ :line-thickness 4)
    (medium-finish-output pane)))

(defmethod handle-repaint ((pane my-pane) region)
  (draw-pane pane))

(defmethod handle-event ((pane my-pane) (event pointer-event))
  (let ((x0 (pointer-event-x event))
        (y0 (pointer-event-y event)))
    (cond ((event-matches-gesture-name-p event :zoom-in)
           (cmd-scale-sheet pane 1.1 1.1 x0 y0))
          ((event-matches-gesture-name-p event :zoom-out)
           (cmd-scale-sheet pane .9 .9 x0 y0))
          ((event-matches-gesture-name-p event :rotate-left)
           (cmd-rotate-sheet pane (/ pi -16) x0 y0))
          ((event-matches-gesture-name-p event :rotate-right)
           (cmd-rotate-sheet pane (/ pi 16) x0 y0))
          ((event-matches-gesture-name-p event :reset-sheet)
           (cmd-translate-sheet pane 50 50))
          ((event-matches-gesture-name-p event :drag-start)
           (prog* ((parent (sheet-parent pane))
                   (region (sheet-region pane))
                   (tr (sheet-transformation pane))
                   (transformed (transform-region tr region)))
              (tracking-pointer (parent)
                (:pointer-button-release
                 (x y)
                 (multiple-value-bind (x y) (untransform-position tr x y)
                   (cmd-translate-sheet pane (- x x0) (- y y0)))
                 (return)))))
          (t
           (return-from handle-event
             (call-next-method)))))
  (redisplay-frame-panes *application-frame*))

(defun make-wrapper (pane)
  (let ((bboard (make-pane 'my-bbrd :contents (list pane)
                           :width 1280 :height 300)))
    (move-and-resize-sheet pane 50 50 200 200)
    bboard))

#+ (or)
(defun make-wrapper (pane)
  (scrolling (:scroll-bars :both) pane))

(define-application-frame sheet-geometry-demo ()
  ()
  (:pointer-documentation t)
  (:panes (app (make-pane 'my-pane :width 200 :height 200
                                   :background +light-pink+
                                   :name 'app))
          (out :application
               :display-function 'show-hierarchy :display-time :command-loop
               :scroll-bars nil :borders nil
               :text-margins '(:bottom 0 :top 0 :left 0 :right 0)))
  (:layouts (default
             (horizontally ()
               (640 out)
               (clim:make-pane 'clime:box-adjuster-gadget)
               (640
                (outlining (:thickness 100 :background +white+)
                  (make-wrapper app)))))))

(let ((clim:*default-server-path* '(:clx-ttf :mirroring :single
                                    )))
  (run-frame-top-level (make-application-frame 'sheet-geometry-demo)))

