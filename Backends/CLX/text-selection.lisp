(in-package :clim-clx)

(defclass clx-text-selection-port-mixin ()
  ())

;;;; Backend component of text selection support

;;; Event classes

(defclass clx-selection-notify-event (window-event)
  ((target   :initarg :target
             :reader selection-event-target)
   (property :initarg :property
             :reader selection-event-property)))

(defclass clx-selection-request-event (window-event)
  ((target    :initarg :target
              :reader selection-event-target)
   (property  :initarg :property
              :reader selection-event-property)
   (selection :initarg :selection
              :reader selection-event-selection)
   (requestor :initarg :requestor
              :reader selection-event-requestor)))

(defclass clx-selection-clear-event (window-event)
  ())

;;; Conversions

;; we at least want to support:

;;; :TEXT, :STRING
;;;
;;; :UTF8_STRING
;;;    As seen from xterm [make that the preferred encoding]
;;;
;;; :COMPOUND_TEXT
;;;    Perhaps relatively easy to produce, hard to grok.
;;;
;;; :TARGETS
;;;    Clients want legitimately to find out what we support.
;;;
;;; :TIMESTAMP
;;;    Clients want to know when we took ownership of the selection.

(defun send-selection-string (selection content requestor target property)
  (let ((result (clim-internals::convert-clipboard-content :string content)))
    (xlib:change-property requestor property (babel:string-to-octets result :encoding :utf-8) target 8)
    (xlib:send-event requestor :selection-notify nil
                               :window requestor
                               :event-window requestor
                               :selection selection
                               :target target
                               :property property)))

(defun handle-selection-request (frame selection requestor target property)
  (log:info "selection request: frame=~s, sel=~s, tgt=~s, prop=~s" frame selection target property)
  (let ((content (first (clim-internals::clipboard-for-type frame (clx->clipboard selection)))))
    (case target
      ((:UTF8_STRING :|text/plain;charset=utf-8|)
       (send-selection-string selection content requestor target property))
      (t
       (xlib:send-event requestor :selection-notify nil
                                  :window requestor
                                  :event-window requestor
                                  :selection selection
                                  :target target
                                  :property nil)))))

(defmethod dispatch-event :around (pane (event clx-selection-request-event))
  (let ((selection (selection-event-selection event))
        (requestor (selection-event-requestor event))
        (target (selection-event-target event))
        (property (selection-event-property event)))
    (handle-selection-request (pane-frame pane) selection requestor target property)))

;;; Protocol functions

(defun clipboard->clx (type)
  (ecase type
    (:selection :primary)
    (:clipboard :clipboard)))

(defun clx->clipboard (type)
  (ecase type
    (:primary :selection)
    (:clipboard :clipboard)))

(defmethod clim-internals::bind-clipboard-for-port ((port clx-text-selection-port-mixin) window type object)
  (let ((type (clipboard->clx type))
        (mirror (sheet-direct-xmirror window)))
    (log:info "xmirror for pane: ~s" mirror)
    (xlib:set-selection-owner (xlib:window-display mirror) type mirror)
    (let ((result (eq (xlib:selection-owner (xlib:window-display mirror) type) mirror)))
      (log:info "bind result! ~s" result)
      result)))

(defmethod clim-internals::release-clipboard-for-port ((port clx-text-selection-port-mixin) window type)
  (xlib:set-selection-owner (clx-port-display port) (clipboard->clx type) nil))

#+nil
(defmethod bind-selection ((port clx-text-selection-port-mixin) window &optional time)
  (xlib:set-selection-owner (xlib:window-display (sheet-direct-xmirror window)) :primary (sheet-direct-xmirror window) time)
  (eq (xlib:selection-owner (xlib:window-display (sheet-direct-xmirror window)) :primary)
      (sheet-direct-xmirror window)))

#+nil
(defmethod release-selection ((port clx-text-selection-port-mixin) &optional time)
  (xlib:set-selection-owner
   (clx-port-display port)
   :primary nil time)
  (setf (selection-owner port) nil)
  (setf (selection-timestamp port) nil))

#+nil
(defmethod request-selection ((port clx-text-selection-port-mixin) requestor time)
  (xlib:convert-selection :primary :UTF8_STRING requestor :bounce time))

#+nil
(defmethod get-selection-from-event ((port clx-text-selection-port-mixin) (event clx-selection-notify-event))
  (if (null (selection-event-property event))
      (progn
        (format *trace-output* "~&;; Oops, selection-notify property is null. Trying the cut buffer instead..~%")
        (xlib:cut-buffer (clx-port-display port)))                
      (let ((v (xlib:get-property (sheet-xmirror (event-sheet event))
                                  (selection-event-property event)
                                  ;; :type :text
                                  :delete-p t
                                  :result-type '(vector (unsigned-byte 8)))))
        (case (clim-clx::selection-event-target event)
          (:string (babel:octets-to-string v :encoding :iso-88519-1))
          (:utf8_string (babel:octets-to-string v :encoding :utf-8))))))

;; Incredibly crappy broken unportable Latin 1 encoder which should be
;; replaced by various implementation-specific versions.
#+nil
(flet ((latin1-code-p (x)
	 (not (or (< x 9) (< 10 x 32) (< #x7f x #xa0) (> x 255)))))
  (defun string-encode (string)
    (delete-if-not #'latin1-code-p (map 'vector #'char-code string)))
  (defun exactly-encodable-as-string-p (string)
    (every #'latin1-code-p (map 'vector #'char-code string))))

;;; TODO: INCR property?
;;;
;;; FIXME: per ICCCM we MUST support :MULTIPLE
#+nil
(defmethod send-selection ((port clx-text-selection-port-mixin) (event clx-selection-request-event) string)
  (let ((requestor (selection-event-requestor event))
        (property  (selection-event-property event))
        (target    (selection-event-target event))
        (time      (event-timestamp event)))
    (when (null property)
      (format *trace-output* "~&* Requestor property is null! *~%"))
    (flet ((send-event (&key target (property property))
	     ;; debugging output, but the KDE Klipper client turns out
	     ;; to poll other clients for selection, which means it
	     ;; would be bad to print at every request.
             (xlib:send-event requestor
			      :selection-notify nil
			      :window requestor
			      :event-window requestor
			      :selection (climi::selection-event-selection event)
			      :target target
			      :property property
			      :time time)))
      (case target
	((:UTF8_STRING)
	 (xlib:change-property requestor property
			       (utf8-string-encode
				(map 'vector #'char-code string))
			       :UTF8_STRING 8)
	 (send-event :target :UTF8_STRING))
	((:STRING :COMPOUND_TEXT)
	 (xlib:change-property requestor property
			       (string-encode string)
			       target 8)            
	 (send-event :target target))
	((:TEXT)
	 (cond
	   ((exactly-encodable-as-string-p string)
	    (xlib:change-property requestor property
				  (string-encode string)
				  :STRING 8)
	    (send-event :target :STRING))
	   (t 
	    (xlib:change-property requestor property
				  (utf8-string-encode
				   (map 'vector #'char-code string))
				  :UTF8_STRING 8)
	    (send-event :target :UTF8_STRING))))
	((:TARGETS)
	 (let* ((display (clx-port-display port))
		(targets (mapcar (lambda (x) (xlib:intern-atom display x))
				 '(:TARGETS :STRING :TEXT :UTF8_STRING
				   :COMPOUND_TEXT :TIMESTAMP))))
	   (xlib:change-property requestor property targets target 32))
	 (send-event :target :TARGETS))
	((:TIMESTAMP)
	 (when (null (selection-timestamp port))
	   (format *trace-output* "~&;; selection-timestamp is null!~%"))
	 (xlib:change-property requestor property
			       (list (selection-timestamp port))
			       target 32)
	 (send-event :target :TIMESTAMP))
	(t
	 (format *trace-output*
		 "~&;; Warning, unhandled type \"~A\". ~
                  Sending property NIL to target.~%" target)
	 (send-event :target target :property nil))))
    (xlib:display-force-output (xlib:window-display requestor))))
