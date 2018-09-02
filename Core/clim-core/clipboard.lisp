(in-package :clim-internals)

(define-presentation-generic-function %convert-clipboard-content convert-clipboard-content
  (type-key parameters options object type output-type))

(define-presentation-method convert-clipboard-content (obj (type string) (output-type (eql :string)))
  (log:info "Converting string to string: ~s" obj)
  (check-type obj string)
  obj)
