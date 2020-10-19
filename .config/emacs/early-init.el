;; http://akrl.sdf.org/
(setq garbage-collection-messages t)

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold (* 1000 1000 1000))

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
    (run-with-idle-timer 15 t
        (lambda ()
            (message "Garbage Collector has run for %.03fsec"
                (k-time (garbage-collect))))))
;; --

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)
