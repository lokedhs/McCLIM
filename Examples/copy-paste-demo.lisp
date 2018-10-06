(defpackage :copy-paste-demo
  (:use :cl)
  (:export :copy-paste-test))

(in-package :copy-paste-demo)

(defclass markup-text ()
  ((text :initarg :text
         :reader markup-text/text)))

(clim:define-presentation-method clim:present (obj (type markup-text) stream view &key)
  (clim:with-drawing-options (stream :ink clim:+blue+)
    (format stream "~a" (markup-text/text obj))))

(clim:define-presentation-method clim-internals::convert-clipboard-content
    (obj (type markup-text) (output-type (eql :string)) check-only)
  (markup-text/text obj))

(clim:define-presentation-method clim-internals::convert-clipboard-content
    (obj (type markup-text) (output-type (eql :html)) check-only)
  (format nil "Highlighted content: <b>~a</b>" (markup-text/text obj)))

(defun display-copy-paste-test (frame stream)
  (declare (ignore frame))
  (format stream "Select text using shift and the left mouse button~%")
  (format stream "You can also use the command \"Copy to clipboard\" to select a presentation.~%")
  (let ((obj (make-instance 'markup-text :text "This text can be copied in HTML form")))
    (clim:stream-present stream obj (clim:presentation-type-of obj))))

(clim:define-application-frame copy-paste-test ()
  ()
  (:panes (content-pane :application
                        :display-function 'display-copy-paste-test
                        :min-width 600)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       content-pane
                       interaction-pane))))

(defun copy-paste-test ()
  (let ((frame (clim:make-application-frame 'copy-paste-test)))
    (clim:run-frame-top-level frame)))

(define-copy-paste-test-command (com-show-clipboard :name "Show Clipboard")
    ()
  (clim-internals::request-clipboard *standard-output* :clipboard-type :clipboard :request-type :string))
