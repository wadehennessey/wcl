;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; The ordering of the forms in this file is important for both
;;; compilation and starting the library.

(defvar *target-bits-per-word* (host_bits_per_word))

;;; See lisp.h for more details.
(defconstant array-tag                                      #x0)
(defconstant other-tag                                      #x1)
(defconstant vector-tag                                     #x0)
(defconstant multi-array-tag                                #x2)
(defconstant number-tag                                     #x1)

(defconstant vector-tag-mask                                #x3)
(defconstant number-tag-mask                                #x3)

(defconstant vector-mask                                    #x7b)
(defconstant array-mask                                     #x79)

(defconstant simple-vector-tag                              #x0)
(defconstant simple-multi-array-tag                         #x2)
(defconstant complex-vector-tag                             #x4)
(defconstant complex-multi-array-tag                        #x6)

(defconstant array-element-type-mask                        #x78)
(defconstant element-type-bit                               #x00)
(defconstant element-type-signed-8bit                       #x08)
(defconstant element-type-unsigned-8bit                     #x10)
(defconstant element-type-char                              #x18)
(defconstant element-type-signed-16bit                      #x20)
(defconstant element-type-unsigned-16bit                    #x28)
(defconstant element-type-signed-32bit                      #x30)
(defconstant element-type-unsigned-32bit                    #x38)
(defconstant element-type-ptr                               #x40)
(defconstant element-type-float                             #x48)

(defconstant type-simple-bit-vector                         #x00)
(defconstant type-simple-signed-8bit-vector                 #x08)
(defconstant type-simple-unsigned-8bit-vector               #x10)
(defconstant type-simple-string                             #x18)
(defconstant type-simple-signed-16bit-vector                #x20)
(defconstant type-simple-unsigned-16bit-vector              #x28)
(defconstant type-simple-signed-32bit-vector                #x30)
(defconstant type-simple-unsigned-32bit-vector              #x38)
(defconstant type-simple-vector                             #x40)
(defconstant type-simple-float-vector                       #x48)


(defconstant type-simple-bit-multi-array                    #x02)
(defconstant type-simple-signed-8bit-multi-array            #x0a)
(defconstant type-simple-unsigned-8bit-multi-array          #x12)
(defconstant type-simple-char-multi-array                   #x1a)
(defconstant type-simple-signed-16bit-multi-array           #x22)
(defconstant type-simple-unsigned-16bit-multi-array         #x2a)
(defconstant type-simple-signed-32bit-multi-array           #x32)
(defconstant type-simple-unsigned-32bit-multi-array         #x3a)
(defconstant type-simple-ptr-multi-array                    #x42)
(defconstant type-simple-float-multi-array                  #x4a)


(defconstant type-complex-bit-vector                        #x04)
(defconstant type-complex-signed-8bit-vector                #x0c)
(defconstant type-complex-unsigned-8bit-vector              #x14)
(defconstant type-complex-char-vector                       #x1c)
(defconstant type-complex-signed-16bit-vector               #x24)
(defconstant type-complex-unsigned-16bit-vector             #x2c)
(defconstant type-complex-signed-32bit-vector               #x34)
(defconstant type-complex-unsigned-32bit-vector             #x3c)
(defconstant type-complex-ptr-vector                        #x44)
(defconstant type-complex-float-vector                      #x4c)


(defconstant type-complex-bit-multi-array                   #x06)
(defconstant type-complex-signed-8bit-multi-array           #x0e)
(defconstant type-complex-unsigned-8bit-multi-array         #x16)
(defconstant type-complex-char-multi-array                  #x1e)
(defconstant type-complex-signed-16bit-multi-array          #x26)
(defconstant type-complex-unsigned-16bit-multi-array        #x2e)
(defconstant type-complex-signed-32bit-multi-array          #x36)
(defconstant type-complex-unsigned-32bit-multi-array        #x3e)
(defconstant type-complex-ptr-multi-array                   #x46)
(defconstant type-complex-float-multi-array                 #x4e)


(defconstant type-bignum                                    #x01)
(defconstant type-ratio                                     #x09)
(defconstant type-float                                     #x05)
(defconstant type-complex                                   #x0d)

(defconstant type-symbol                                    #x03)
(defconstant type-line-symbol                               #x0b)

(defconstant type-character                                 #x07)
(defconstant type-cons                                      #x0f)
(defconstant type-oe                                        #x17)
(defconstant type-foreign-ptr                               #x1f)
(defconstant type-procedure                                 #x27)
(defconstant type-structure                                 #x2f)
(defconstant type-void                                      #x37)
(defconstant type-closure                                   #x3f)
(defconstant type-forwarding-ptr                            #x47)
(defconstant type-ubv                                       #x4f)
(defconstant type-end-of-page                               #x7f)

;;; the length field of a procedure object tells what kind of
;;; of procedure we have.
(defconstant open-procedure-flag 1)
(defconstant closed-procedure-flag 1234)
(defconstant funcallable-instance-flag 5678)

(defconstant indirect-array-offset 0)
(defconstant vector-fill-pointer-offset 1)
(defconstant vector-displaced-index-offset 2)
(defconstant array-dims-vector-offset 1)
(defconstant array-mult-vector-offset 2)
(defconstant array-displaced-index-offset 3)

(defconstant special-symbol-flag 0)
(defconstant constant-symbol-flag 1)
(defconstant macro-symbol-flag 2)
(defconstant print-escape-flag 3)
(defconstant print-escape-flag-valid? 4)

(defconstant symbol-value-offset 0)
(defconstant symbol-package-offset 1)
(defconstant symbol-self-link-offset 2)
(defconstant symbol-plist-offset 3)
(defconstant symbol-function-offset 4)
(defconstant symbol-hashcode-offset 5)
(defconstant symbol-flags-offset 6)
(defconstant symbol-name-offset 7)

(defconstant line-symbol-line-offset 0)

(defconstant symbol-tag-mask #x7)
(defconstant either-symbol-tag #x3)

;;; Need this to boot defstruct
(defconstant lambda-list-keywords
  '(&optional &rest &restv &key &allow-other-keys
    &aux &whole &environment &body))

(defconstant hash-mask 1048575)

(defconstant unix-maxpathlen 1024)
(defconstant unix-eof-flag -1)
			     
			  




			
		      






