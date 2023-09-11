;;; aio-contrib.el --- extra functions for aio -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; These are additional, useful functions that do not make sense to
;; include in the main aio package. This file has additional
;; dependencies beyond that of aio.

;;; Code:

(require 'aio)
(require 'elfeed nil t)
(require 'filenotify)
(require 'hashcash)
(require 'skewer-mode nil t)

;; Note: Threads are too broken to be useful in at least Emacs 26.1.
;; So, by extension, this function isn't useful either, which is why
;; it's here in contrib. Hopefully someday it will be, though.
(defun aio-make-thread (function &optional name)
  "Like `make-thread', but FUNCTION's return value is delivered by promise.

Returns a cons of (promise . thread). Calling `aio-await' on the
returned promise is analogous to using `thread-join'."
  (let* ((promise (aio-promise))
         (wrapper (lambda ()
                    (aio-with-promise promise
                      (funcall function))))
         (thread (make-thread wrapper name)))
    (cons promise thread)))

(defun aio-skewer-eval (string &rest args)
  "Like `skewer-eval' but the result is delivered by a promise."
  (let* ((promise (aio-promise))
         (callback (lambda (result)
                     (aio-resolve promise (lambda () result)))))
    (prog1 promise
      (apply #'skewer-eval string callback args))))

(defun aio-elfeed-curl-enqueue (url &rest args)
  "Like `elfeed-curl-enqueue' but delivered by a promise.

The result is a plist with the following keys:
:success -- the callback argument (t or nil)
:headers -- `elfeed-curl-headers'
:status-code -- `elfeed-curl-status-code'
:error-message -- `elfeed-curl-error-message'
:location -- `elfeed-curl-location'
:content -- (buffer-string)"
  (let* ((promise (aio-promise))
         (cb (lambda (success)
               (let ((result (list :success success
                                   :headers elfeed-curl-headers
                                   :status-code elfeed-curl-status-code
                                   :error-message elfeed-curl-error-message
                                   :location elfeed-curl-location
                                   :content (buffer-string))))
                 (aio-resolve promise (lambda () result))))))
    (prog1 promise
      (apply #'elfeed-curl-enqueue url cb args))))

(defun aio-file-notify-add-watch (file flags)
  "Like `file-notify-add-watch' but delivered by a promise."
  (let* ((promise (aio-promise))
         (cb (lambda (event) (aio-resolve promise (lambda () event)))))
    (prog1 promise
      (file-notify-add-watch file flags cb))))

(defun aio-hashcash-generate-payment (str val)
  "Like `hashcash-generate-payment' but delivered by a promise."
  ;; Note: In Emacs 26.1 this only works when byte compiled due to a
  ;; bug in hashcash.el.
  (let* ((promise (aio-promise))
         (cb (lambda (_proc result)
               (aio-resolve promise (lambda () result)))))
    (prog1 promise
      (hashcash-generate-payment-async str val cb))))

(aio-defun aio-accept-process-output (process &optional seconds just-this-one max-wait-time)
  "Like `accept-process-output' but delivered by a promise, with
extra logic to allow cancellability in the case of a hanged
process.

When JUST-THIS-ONE is non-nil, it is always used as 't
MAX-WAIT-TIME is a floating-point value specifying the number of
seconds to wait between calls to `accept-process-output'"
  (let* ((max-wait-time (or max-wait-time 1.0))
	 (start-time (car (time-convert nil 1000)))
	 (time-difference 0)
	 result)
    (unless
	(with-local-quit
	  (while (and (not (aio-promise-cancelled-p aio-current-promise))
		      (null result)
		      (and process (process-live-p process))
		      (or (null seconds)
			  (> seconds time-difference)))
	    (setf result
		  (accept-process-output
		   process
		   (if seconds
		       (min (- seconds time-difference) max-wait-time)
		     max-wait-time)
		   nil (when just-this-one t)))
	    (setf time-difference
		  (/ (- (car (time-convert nil 1000)) start-time) 1000.0)))
	  t)
      (signal 'quit nil))
    result))

(provide 'aio-contrib)

;;; aio-contrib.el ends here
