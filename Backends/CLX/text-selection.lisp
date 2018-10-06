(in-package :clim-clx)

(defclass clx-text-selection-port-mixin ()
  ())

;;;; Backend component of text selection support

;;; Event classes

(defclass clx-selection-notify-event (window-event)
  ((target   :initarg :target
             :reader selection-event-target)
   (property :initarg :property
             :reader selection-event-property)
   (selection :initarg :selection
              :reader selection-event-selection)))

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
  ((selection :initarg :selection
              :reader selection-event-selection)))

(defun send-selection-string (selection obj presentation-type requestor target property timestamp)
  (let ((result (clim-internals::convert-clipboard-content obj :string :type presentation-type)))
    (xlib:change-property requestor property (babel:string-to-octets result :encoding :utf-8) target 8)
    (xlib:send-event requestor :selection-notify nil
                               :window requestor
                               :event-window requestor
                               :selection selection
                               :target target
                               :property property
                               :time timestamp)))

(defun send-selection-html (selection obj presentation-type requestor target property timestamp)
  (let ((result (clim-internals::convert-clipboard-content obj :html :type presentation-type)))
    (xlib:change-property requestor property (babel:string-to-octets result :encoding :utf-8) target 8)
    (log:info "Sending html to remote: ~s" result)
    (xlib:send-event requestor :selection-notify nil
                               :window requestor
                               :event-window requestor
                               :selection selection
                               :target target
                               :property property
                               :time timestamp)))

(defparameter *request-type-to-target* '((:string :UTF8_STRING :STRING :TEXT :|text/plain;charset=utf-8|)
                                         (:html :|text/html|)))

(defun handle-selection-request (pane selection requestor target property timestamp)
  (log:info "Got selection request for target: ~s (req=~s, prop=~s)" target requestor property)
  (let* ((frame (pane-frame pane))
         (display (xlib:window-display (sheet-direct-xmirror pane))))
    ;;
    ;;
    (destructuring-bind (obj presentation-type pane)
        (clim-internals::clipboard-for-type frame (clx->clipboard selection))
      (declare (ignore pane))
      ;;
      (labels ((send-error-reply ()
                 (xlib:send-event requestor :selection-notify nil
                                            :window requestor
                                            :event-window requestor
                                            :selection selection
                                            :target target
                                            :property nil
                                            :time timestamp))
               ;;
               (can-convert-p (type)
                 (clim-internals::convert-clipboard-content obj type
                                                            :type presentation-type
                                                            :check-only t)))
        ;;
        (case target
          ((:TARGETS)
           (let ((available-types (append '(:TARGETS :MULTIPLE)
                                          (loop
                                            for type in '((:string :UTF8_STRING :STRING :TEXT)
                                                          (:html :|text/html|))
                                            when (can-convert-p (car type))
                                              append (cdr type)))))
             (log:info "available types for ~s: ~s" obj available-types)
             (xlib:change-property requestor
                                   property
                                   (mapcar (lambda (v) (xlib:intern-atom display v)) available-types)
                                   target 32)
             (xlib:send-event requestor :selection-notify nil
                                        :window requestor
                                        :event-window requestor
                                        :selection selection
                                        :target target
                                        :property property
                                        :time timestamp)))
          ((:UTF8_STRING :|text/plain;charset=utf-8| :STRING :TEXT)
           (if (can-convert-p :string)
               (send-selection-string selection obj presentation-type requestor target property timestamp)
               (send-error-reply)))
          ((:|text/html|)
           (log:info "checking for html conversion, result=~s" (can-convert-p :html))
           (if (can-convert-p :html)
               (send-selection-html selection obj presentation-type requestor target property timestamp)
               (send-error-reply)))
          (t
           (send-error-reply)))))))

(defmethod dispatch-event :around (pane (event clx-selection-request-event))
  (let ((selection (selection-event-selection event))
        (requestor (selection-event-requestor event))
        (target (selection-event-target event))
        (property (selection-event-property event))
        (timestamp (event-timestamp event)))
    (handle-selection-request pane selection requestor target property timestamp)))

(defmethod dispatch-event :around (pane (event clx-selection-clear-event))
  (let ((selection (selection-event-selection event)))
    (setf (clim-internals::clipboard-for-type (pane-frame pane) (clx->clipboard selection)) nil)))

(defun request-type-matches-target (request-types target)
  (loop
    for type in request-types
    for type-entry = (find type *request-type-to-target* :key #'car)
    when type-entry
      do (let ((matched-type (loop
                               for v in (cdr type-entry)
                               for matched-type = (member v target)
                               when matched-type
                                 return v)))
           (when matched-type
             (return matched-type)))))

(defun create-clipboard-request-result (event pane mirror)
  (labels ((prop-string ()
             (babel:octets-to-string (xlib:get-property mirror (selection-event-property event)
                                                        :delete-p t
                                                        :result-type '(vector (unsigned-byte 8)))
                                     :encoding :utf-8)))
    ;;
    (let ((target (selection-event-target event)))
      (alexandria:when-let ((type-entry (find-if (lambda (v) (member target (cdr v))) *request-type-to-target*)))
        (ecase (car type-entry)
          (:string (make-instance 'climi::clipboard-string-result-event :content (prop-string)
                                                                        :sheet pane
                                                                        :timestamp (event-timestamp event)))
          (:html (make-instance 'climi::clipboard-html-result-event :content (prop-string)
                                                                    :sheet pane
                                                                    :timestamp (event-timestamp event))))))))

(defun publish-clipboard-request-result (event pane mirror)
  (alexandria:when-let ((result (create-clipboard-request-result event pane mirror)))
    (log:info "queuing result event to: ~s: ~s" pane result)
    (queue-event pane result)))

(defmethod dispatch-event :around (pane (event clx-selection-notify-event))
  (let ((mirror (sheet-direct-xmirror pane)))
    ;;
    (labels ((clear-outstanding-request ()
               (setf (getf (xlib:window-plist mirror) 'clipboard-request) nil)))
      ;;
      (log:info "outstanding request: ~s" (getf (xlib:window-plist mirror) 'clipboard-request))
      (alexandria:when-let ((outstanding-request (getf (xlib:window-plist mirror) 'clipboard-request)))
        (destructuring-bind (pane clipboard-type request-types state)
            outstanding-request
          ;;
          (cond ((not (eq clipboard-type (selection-event-selection event)))
                 (log:error "Unexpected clipboard type: ~s (expected: ~s)" (selection-event-selection event) clipboard-type)
                 (clear-outstanding-request))
                ((null (selection-event-property event))
                 (log:info "no event property")
                 (clear-outstanding-request))
                ((eq state :targets)
                 (let ((property (xlib:get-property mirror (selection-event-property event) :delete-p t))
                       (display (xlib:window-display mirror)))
                   (log:info "prop result: ~s" property)
                   (cond ((eq (selection-event-target event) :targets)
                          (let ((available-targets (mapcar (lambda (v) (xlib:atom-name display v)) property)))
                            (let ((matched-type (request-type-matches-target request-types available-targets)))
                              (cond (matched-type
                                     (setf (fourth outstanding-request) :request)
                                     (xlib:convert-selection (selection-event-selection event) matched-type mirror))
                                    (t
                                     (log:info "no matching targets: ~s" request-types)
                                     (clear-outstanding-request))))))
                         (t
                          (log:info "Unexpected target: ~s" (selection-event-target event))
                          (clear-outstanding-request)))))
                ((eq state :request)
                 (publish-clipboard-request-result event pane mirror)
                 (clear-outstanding-request))
                (t
                 (log:error "Got unexpected selection event")
                 (clear-outstanding-request))))))))

;;; Protocol functions

(defun clipboard->clx (type)
  (ecase type
    (:selection :primary)
    (:clipboard :clipboard)))

(defun clx->clipboard (type)
  (ecase type
    (:primary :selection)
    (:clipboard :clipboard)))

(defmethod clim-internals::bind-clipboard-for-port ((port clx-text-selection-port-mixin) window clipboard-type object object-type)
  (let ((clipboard-type (clipboard->clx clipboard-type))
        (mirror (sheet-direct-xmirror window)))
    (xlib:set-selection-owner (xlib:window-display mirror) clipboard-type mirror)
    (let ((result (eq (xlib:selection-owner (xlib:window-display mirror) clipboard-type) mirror)))
      result)))

(defmethod clim-internals::release-clipboard-for-port ((port clx-text-selection-port-mixin) window clipboard-type)
  (xlib:set-selection-owner (clx-port-display port) (clipboard->clx clipboard-type) nil))

(defmethod clim-internals::request-clipboard-for-port ((port clx-text-selection-port-mixin) window clipboard-type request-types)
  (let* ((clipboard-type (clipboard->clx clipboard-type))
         (mirror (sheet-direct-xmirror window))
         (outstanding-request (getf (xlib:window-plist mirror) 'clipboard-request)))
    (cond (outstanding-request
           (log:warn "Attempt to request clipboard content while a request is already outstanding"))
          (t
           (setf (getf (xlib:window-plist mirror) 'clipboard-request)
                 (list window clipboard-type request-types :targets))
           (xlib:convert-selection clipboard-type :targets mirror :clim_selection)))))

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
