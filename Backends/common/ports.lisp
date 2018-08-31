(in-package :clim-standard)

(defclass standard-port (basic-port)
  ((mirrored-sheet->current-pointer-cursor :initform (make-hash-table :test #'eq))
   #+nil
   (selection-owner :initform nil :accessor port-selection-owner)
   #+nil
   (selection-requester :initform nil :accessor port-selection-requester)))


(defmethod port-lookup-current-pointer-cursor ((port standard-port) sheet)
  (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)))

(defmethod set-sheet-pointer-cursor :before ((port standard-port) sheet cursor)
  (setf (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)) cursor))
