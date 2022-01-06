;;;; fndb.lisp -- DEFKNOWNish bits for SBCL

(cl:in-package :nibbles)

#+sbcl (progn

#+#.(cl:if (cl:find-package "SB-NIBBLES") '(:and) '(:or)) (progn

(macrolet ((def (name size signedp setterp be-p)
             (let* ((result-type `(,(if signedp 'signed-byte 'unsigned-byte) ,size))
                    (arg-types `(array index ,@(when setterp (list result-type)))))
               `(sb-c:defknown ,name ,arg-types ,result-type (sb-c:any)
                  :overwrite-fndb-silently t))))
  (def ub16ref/be 16 nil nil t)
  (def ub16ref/le 16 nil nil nil)
  (def ub16set/be 16 nil t t)
  (def ub16set/le 16 nil t nil)
  (def sb16ref/be 16 t nil t)
  (def sb16ref/le 16 t nil nil)
  (def sb16set/be 16 t t t)
  (def sb16set/le 16 t t nil)

  (def ub32ref/be 32 nil nil t)
  (def ub32ref/le 32 nil nil nil)
  (def ub32set/be 32 nil t t)
  (def ub32set/le 32 nil t nil)
  (def sb32ref/be 32 t nil t)
  (def sb32ref/le 32 t nil nil)
  (def sb32set/be 32 t t t)
  (def sb32set/le 32 t t nil)

  (def ub64ref/be 64 nil nil t)
  (def ub64ref/le 64 nil nil nil)
  (def ub64set/be 64 nil t t)
  (def ub64set/le 64 nil t nil)
  (def sb64ref/be 64 t nil t)
  (def sb64ref/le 64 t nil nil)
  (def sb64set/be 64 t t t)
  (def sb64set/le 64 t t nil))

);#+(find-package "SB-NIBBLES")

#-#.(cl:if (cl:find-package "SB-NIBBLES") '(:and) '(:or)) (progn

;;; Efficient array bounds checking
(sb-c:defknown %check-bound
  ((simple-array (unsigned-byte 8) (*)) index (and fixnum sb-vm:word)
   (member 2 4 8 16))
    index (sb-c:any) :overwrite-fndb-silently t)

;; We DEFKNOWN the exported functions so we can DEFTRANSFORM them.
;; We DEFKNOWN the %-functions so we can DEFINE-VOP them.

#.(loop for i from 0 to #-x86-64 #b0111 #+x86-64 #b1011
        for bitsize = (ecase (ldb (byte 2 2) i)
                        (0 16)
                        (1 32)
                        (2 64))
        for signedp = (logbitp 1 i)
        for setterp = (logbitp 0 i)
        for byte-fun = (if setterp
                           #'byte-set-fun-name
                           #'byte-ref-fun-name)
        for big-fun = (funcall byte-fun bitsize signedp t)
        for little-fun = (funcall byte-fun bitsize signedp nil)
        for internal-big = (internalify big-fun)
        for internal-little = (internalify little-fun)
        for arg-type = `(,(if signedp
                              'signed-byte
                              'unsigned-byte)
                              ,bitsize)
        for external-arg-types = `(array index ,@(when setterp
                                                   `(,arg-type)))
        for internal-arg-types = (subst '(simple-array (unsigned-byte 8)) 'array
                                        external-arg-types)
        collect `(sb-c:defknown (,big-fun ,little-fun) ,external-arg-types
                     ,arg-type (sb-c:any) :overwrite-fndb-silently t) into defknowns
        collect `(sb-c:defknown (,internal-big ,internal-little)
                     ,internal-arg-types
                     ,arg-type (sb-c:any) :overwrite-fndb-silently t) into defknowns
        finally (return `(progn ,@defknowns)))

);#-(find-package "SB-NIBBLES")

);#+sbcl
