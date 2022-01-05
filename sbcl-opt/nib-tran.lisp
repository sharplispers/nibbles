;;;; nib-tran.lisp -- DEFTRANSFORMs for SBCL

(cl:in-package :nibbles)

#+sbcl (progn

#+#.(cl:if (cl:find-package "SB-NIBBLES") '(:and) '(:or)) (progn

(macrolet ((def (name size signedp setterp be-p)
             (let* ((arglist `(vector offset ,@(when setterp '(value))))
                    (sb-name (find-symbol (symbol-name name) "SB-NIBBLES"))
                    (result-type `(,(if signedp 'signed-byte 'unsigned-byte) ,size))
                    (arg-types `(array index ,@(when setterp (list result-type)))))
               (when sb-name
                 `(sb-c:deftransform ,name (,arglist ,arg-types ,result-type)
                    `(progn
                       (,',sb-name vector (sb-nibbles::%check-bound vector (length vector) offset ,',(truncate size 8)) ,@',(when setterp '(value)))
                       ,@',(when setterp '(value))))))))
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

(sb-c:deftransform %check-bound ((vector bound offset n-bytes)
				 ((simple-array (unsigned-byte 8) (*)) index
				  (and fixnum sb-vm:word)
				  (member 2 4 8 16))
				 * :node node)
  "optimize away bounds check"
  ;; cf. sb-c::%check-bound transform
  (cond ((sb-c:policy node (= sb-c::insert-array-bounds-checks 0))
	 'offset)
	((not (sb-c::constant-lvar-p bound))
	 (sb-c::give-up-ir1-transform))
	(t
	 (let* ((dim (sb-c::lvar-value bound))
		(n-bytes (sb-c::lvar-value n-bytes))
		(upper-bound `(integer 0 (,(- dim n-bytes -1)))))
	   (if (> n-bytes dim)
	       (sb-c::give-up-ir1-transform)
	       `(the ,upper-bound offset))))))

#.(flet ((specialized-includep (bitsize signedp setterp)
           (declare (ignorable bitsize signedp setterp))
           ;; Bleh.  No good way to solve this atm.
           ;;
           ;; Non-x86.  No support.
           #-(or x86 x86-64)
           nil
           ;; x86 and x86-64.  Can do everything.
           #+(or x86 x86-64)
           t)
         (generic-transform-form (fun-name arglist n-bytes
                                           setterp signedp big-endian-p)
           (let ((offset-type `(integer 0 ,(- array-dimension-limit n-bytes))))
           `(sb-c:deftransform ,fun-name ,arglist
              `(locally (declare (type ,',offset-type offset))
		 ,',(if setterp
			(set-form 'vector 'offset 'value n-bytes big-endian-p)
			(ref-form 'vector 'offset n-bytes signedp big-endian-p)))))))
    (loop for i from 0 to #-x86-64 #b0111 #+x86-64 #b1011
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
          for n-bytes = (truncate bitsize 8)
          for arg-type = `(,(if signedp
                                'signed-byte
                                'unsigned-byte)
                                ,bitsize)
          for arglist = `(vector offset ,@(when setterp '(value)))
          for external-arg-types = `(array index ,@(when setterp
                                                     `(,arg-type)))
          for internal-arg-types = (subst '(simple-array (unsigned-byte 8)) 'array
                                          external-arg-types)
          for transform-arglist = `(,arglist ,internal-arg-types ,arg-type)
          for specialized-big-transform
            = `(sb-c:deftransform ,big-fun ,transform-arglist
                 '(,internal-big vector (%check-bound vector (length vector) offset ,n-bytes)
                   ,@(when setterp '(value))))
          for specialized-little-transform
            = (subst internal-little internal-big
                                     (subst little-fun big-fun
                                            specialized-big-transform))
          ;; Also include inlining versions for when the argument type
          ;; is known to be a simple octet vector and we don't have a
          ;; native assembly implementation.
          for generic-big-transform
            = (generic-transform-form big-fun transform-arglist n-bytes
                      setterp signedp t)
          for generic-little-transform
            = (generic-transform-form little-fun transform-arglist n-bytes
                      setterp signedp nil)
          if (specialized-includep bitsize signedp setterp)
            collect specialized-big-transform into transforms
          else if (<= bitsize sb-vm:n-word-bits)
            collect generic-big-transform into transforms
          if (specialized-includep bitsize signedp setterp)
            collect specialized-little-transform into transforms
          else if (<= bitsize sb-vm:n-word-bits)
            collect generic-little-transform into transforms
          finally (return `(progn ,@transforms))))

);#-(find-package "SB-NIBBLES")

);#+sbcl
