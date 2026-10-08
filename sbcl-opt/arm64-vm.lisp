;;;; arm64-vm.lisp -- VOP definitions SBCL

(cl:in-package :sb-vm)

(define-vop (%check-bound)
  (:translate nibbles::%check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (bound :scs (any-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-unsigned-byte-8 positive-fixnum tagged-num
              (:constant (member 2 3 4 8 16)))
  (:info offset)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:vop-var vop)
  (:generator 5
    (let ((error (generate-error-code vop 'invalid-array-index-error
                                      array bound index)))
      ;; We want to check the conditions:
      ;;
      ;; 0 <= INDEX
      ;; INDEX < BOUND
      ;; 0 <= INDEX + OFFSET
      ;; (INDEX + OFFSET) < BOUND
      ;;
      ;; We can do this naively with two unsigned checks:
      ;;
      ;; INDEX <_u BOUND
      ;; INDEX + OFFSET <_u BOUND
      ;;
      ;; If INDEX + OFFSET <_u BOUND, though, INDEX must be less than
      ;; BOUND.  We *do* need to check for 0 <= INDEX, but that has
      ;; already been assured by higher-level machinery.
      (inst add temp index (fixnumize offset))
      (inst cmp temp bound)
      (inst b :hi error)
      (move result index))))

#.(flet ((frob (bitsize setterp signedp big-endian-p)
           (let* ((name (funcall (if setterp
                                     #'nibbles::byte-set-fun-name
                                     #'nibbles::byte-ref-fun-name)
                                 bitsize signedp big-endian-p))
                  (internal-name (nibbles::internalify name))
                  (sign-extending-load (and signedp (not big-endian-p)))
                  (insn (if setterp
                            (ecase bitsize (16 'strh) ((32 64) 'str))
                            (if sign-extending-load
                                (ecase bitsize (16 'ldrsh) (32 'ldrsw) (64 'ldr))
                                (ecase bitsize (16 'ldrh)  ((32 64) 'ldr)))))
                  (result-sc (if signedp 'signed-reg 'unsigned-reg))
                  (result-type (if signedp 'signed-num 'unsigned-num))
                  (swap-insn (ecase bitsize (16 'rev16) (32 'rev32) (64 'rev)))
                  (tn (if setterp (if big-endian-p 'temp 'value*) 'result))
                  (tn (if (= bitsize 32) `(32-bit-reg ,tn) tn)))
             `(define-vop (,name)
                (:translate ,internal-name)
                (:policy :fast-safe)
                (:args (vector :scs (descriptor-reg))
                       (index :scs (any-reg unsigned-reg immediate))
                       ,@(when setterp
                           `((value* :scs (,result-sc) :target result))))
                (:arg-types simple-array-unsigned-byte-8
                            positive-fixnum
                            ,@(when setterp
                                `(,result-type)))
                ,@(when (and setterp big-endian-p)
                    `((:temporary (:sc unsigned-reg
                                   :from (:load 0)
                                   :to (:result 0)) temp)))
                (:results (result :scs (,result-sc)))
                (:result-types ,result-type)
                (:generator 3
                  (let ((base-disp (- (* vector-data-offset n-word-bytes)
                                      other-pointer-lowtag)))
                    ,@(when (and setterp big-endian-p)
                        `((inst ,swap-insn temp value*)))
                    ;; a la compiler/arm64/macros.lisp DEFINE-PARTIAL-{REFFER,SETTER}
                    (sc-case index
                      (immediate
                       (inst ,insn ,tn
                             (@ vector (load-store-offset (+ (tn-value index) base-disp)))))
                      (t
                       (inst add tmp-tn vector (lsr index (if (sc-is index any-reg) 1 0)))
                       (inst ,insn ,tn (@ tmp-tn base-disp))))
                    ,@(if setterp
                          '((move result value*))
                          (when big-endian-p
                            `((inst ,swap-insn result result)
                              ,(when (and signedp (/= bitsize 64))
                                 `(inst sbfm result result 0 ,(1- bitsize))))))))))))
    (loop for i from 0 upto #b10111
          for bitsize = (ecase (ldb (byte 2 3) i)
                          (0 16)
                          (1 32)
                          (2 64))
          for setterp = (logbitp 2 i)
          for signedp = (logbitp 1 i)
          for big-endian-p = (logbitp 0 i)
          collect (frob bitsize setterp signedp big-endian-p) into forms
          finally (return `(progn ,@forms))))

;;; 24-bit accessors need to be handled specially.
#.(flet ((frob (setterp signedp big-endian-p)
           (let* ((name (funcall (if setterp
                                     #'nibbles::byte-set-fun-name
                                     #'nibbles::byte-ref-fun-name)
                                 24 signedp big-endian-p))
                  (internal-name (nibbles::internalify name))
                  (result-sc (if signedp 'signed-reg 'unsigned-reg))
                  (result-type (if signedp 'signed-num 'unsigned-num))
                  (byte-insn (if setterp 'strb 'ldrb))
                  (half-insn (if setterp 'strh (if signedp 'ldrsh 'ldrh))))
             `(define-vop (,name)
                (:translate ,internal-name)
                (:policy :fast-safe)
                (:args (vector :scs (descriptor-reg))
                       (index :scs (immediate unsigned-reg))
                       ,@(when setterp `((value* :scs (,result-sc) :target result))))
                (:arg-types simple-array-unsigned-byte-8 positive-fixnum
                            ,@(when setterp `(,result-type)))
                (:temporary (:sc unsigned-reg) upper)
                (:results (result :scs (,result-sc) :from (:load 0)))
                (:result-types ,result-type)
                (:generator 3
                 (let ((base-disp (- (* vector-data-offset n-word-bytes)
                                      other-pointer-lowtag)))
                   ,@(when setterp
                       `((move result value*)
                         (inst lsr upper result 8)
                         ,(when big-endian-p
                            `(inst rev16 upper upper))))
                   ,@(flet ((generate-body (f)
                              `(sc-case index
                                 (immediate
                                  ,@(funcall f
                                             (lambda (offset)
                                               `(@ vector
                                                   (load-store-offset
                                                    (+ (tn-value index) base-disp ,offset))))))
                                 (t
                                  (inst add tmp-tn vector
                                        (lsr index (if (sc-is index any-reg) 1 0)))
                                  ,@(funcall f
                                             (lambda (offset)
                                               `(@ tmp-tn (+ base-disp ,offset))))))))
                         (cond
                           ((or (not (and signedp big-endian-p)) setterp)
                            `(,(generate-body
                                (lambda (disp)
                                  `((inst ,byte-insn result ,(funcall disp (if big-endian-p 2 0)))
                                    (inst ,half-insn upper ,(funcall disp (if big-endian-p 0 1))))))
                              ,@(unless setterp
                                  `(,(when big-endian-p
                                       `(inst rev16 upper upper))
                                    (inst orr result result (lsl upper 8))))))
                           (t
                            ;; We swap the sizes of the loads for signed big-endian.
                            `(,(generate-body
                                (lambda (disp)
                                  `((inst ldrsb upper ,(funcall disp 0))
                                    (inst ldrh result ,(funcall disp 1)))))
                              (inst rev16 result result)
                              (inst orr result result (lsl upper 16))))))))))))
    (loop for i from 0 upto #b111
          for setterp = (logbitp 2 i)
          for signedp = (logbitp 1 i)
          for big-endian-p = (logbitp 0 i)
          collect (frob setterp signedp big-endian-p) into forms
          finally (return `(progn ,@forms))))
