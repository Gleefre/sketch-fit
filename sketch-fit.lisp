(defpackage #:sketch-fit
  (:use #:cl #:sketch)
  (:export #:fit #:with-fit #:fit-point))

(in-package #:sketch-fit)

;; Fit -- function to fit desired width/height to rectangle on screen

(defun fit (width height from-width from-height &optional (to-x 0) (to-y 0) (from-x 0) (from-y 0) max-scale)
  (translate from-x from-y)
  (let* ((scale (min (/ from-width width)
                     (/ from-height height)
                     (if max-scale max-scale 10000)))
         (x-shift (/ (- from-width (* width scale)) 2))
         (y-shift (/ (- from-height (* height scale)) 2)))
    (translate x-shift y-shift)
    (scale scale))
  (translate (- to-x) (- to-y)))

(defun fit-point (x y width height from-width from-height
                  &optional (to-x 0) (to-y 0) (from-x 0) (from-y 0) max-scale)
  (setf x (- x from-x)
        y (- y from-y))
  (let* ((scale (min (/ from-width width)
                     (/ from-height height)
                     (if max-scale max-scale 10000)))
         (x-shift (/ (- from-width (* width scale)) 2))
         (y-shift (/ (- from-height (* height scale)) 2)))
    (setf x (- x x-shift)
          y (- y y-shift))
    (setf x (/ x scale)
          y (/ y scale)))
  (setf x (+ x to-x)
        y (+ y to-y))
  (list x y))

(defmacro with-fit ((width height from-width from-height
                     &optional  (to-x 0) (to-y 0) (from-x 0) (from-y 0) max-scale)
                    &body body)
  `(progn
     (push-matrix)
     (fit ,width ,height ,from-width ,from-height ,to-x ,to-y ,from-x ,from-y ,max-scale)
     ,@body
     (pop-matrix)))
