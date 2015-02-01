(in-package #:faitaa-client)

(defparameter *window-width* 800)
(defparameter *window-height* 800)

(let ((camera (make-instance 'camera 
			     :pos (sb-cga:vec 0.0 0.0 100.0) 
			     :direction (sb-cga:vec 0.0 0.0 -0.1)))
      (proj-info (make-projection-info
		  :fov 60.0
		  :height (ensure-float *window-height*)
		  :width (ensure-float *window-width*)
		  :z-near 500.0
		  :z-far -500.0))
      (circle)
      (waiting-message)
      (quad))
  
  (defun graphics-init ()
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear-depth -500.0)
    (gl:enable :depth-test)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:depth-func :greater)
    (setf quad (make-instance 'ortho-quad))
    (setf circle (make-instance 'circle)))
  
  (defun render-scene ()
    (gl:clear :color-buffer-bit :depth-buffer-bit)

    ;; GUI depth range (10, 1)
    ;; fields (-1,-10)
    
    (set-ortho-world-pos (sb-cga:vec 5.0 5.0 1.0))
    (set-ortho-scale (sb-cga:vec 20.0 20.0 1.0))
    (update-ortho-pipeline camera proj-info)	 
    (circle-render circle (get-ortho-projection-transform) (get-ortho-model-view-transform) (sb-cga:vec 1.0 0.0 0.0))
    
;;     (ecase (current-screen *game-state*)
;;       (:title-screen
       
;;        (set-ortho-world-pos (sb-cga:vec 0.0 0.0 1.0))
;;        (set-ortho-scale (sb-cga:vec 390.0 390.0 1.0))
;;        (update-ortho-pipeline camera proj-info)
;;        (ortho-quad-render quad (get-ortho-projection-transform) (get-ortho-model-view-transform) (sb-cga:vec 0.0 1.0 0.0)))
      
;;       (:waiting-for-opponent 

;;        (set-ortho-world-pos (sb-cga:vec 0.0 0.0 1.0))
;;        (set-ortho-scale (sb-cga:vec 200.0 50.0 1.0))
;;        (update-ortho-pipeline camera proj-info)
;;        (text-billboard-render waiting-message (get-ortho-projection-transform) (get-ortho-model-view-transform)))
      
;;       (:game-play
  
;; )
       
;;       (:end-score))
    )
    
  (defun get-3d-ray-under-mouse (x y)
    (let ((viewport (list 0.0 0.0 *window-width* *window-height*)))
      (values
       (unproject (sb-cga:vec x y 0.0)
		  (look-at (camera-pos camera)
			   (sb-cga:vec+ (camera-pos camera) (camera-direction camera))
			   (camera-up camera))
		  (ortho (- (/ (projection-info-width proj-info) 2.0))
			 (/ (projection-info-width proj-info) 2.0)
			 (- (/ (projection-info-height proj-info) 2.0))
			 (/ (projection-info-height proj-info) 2.0)
			 (projection-info-z-near proj-info)
			 (projection-info-z-far proj-info))
		  viewport)
       
       (unproject (sb-cga:vec x y 1.0)
		  (look-at (camera-pos camera)
			   (sb-cga:vec+ (camera-pos camera) (camera-direction camera))
			   (camera-up camera))
		  (ortho (- (/ (projection-info-width proj-info) 2.0))
			 (/ (projection-info-width proj-info) 2.0)
			 (- (/ (projection-info-height proj-info) 2.0))
			 (/ (projection-info-height proj-info) 2.0)
			 (projection-info-z-near proj-info)
			 (projection-info-z-far proj-info))
		  viewport)))))

