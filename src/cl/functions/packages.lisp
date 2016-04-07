;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun abbreviate-package-name (string)
  (cond ((string= string "LISP") "lsp")
	((string= string "KEYWORD") "key")
	(t (remove-if-not #'alpha-char-p (string-downcase string)))))

(defun add-symbol (s package)
  (let ((table (package-symbols package)))
    (setf (symbol-package s) package)
    (setf (gethash s table) (if (eq package *keyword-package*)
				:external
				:internal))))

(defun apropos-1 (string package collect-symbols?)
  (let ((symbols nil))
    (if (null package)
	(do-all-symbols (symbol)
	  (when (search string (symbol-name symbol))
	    (if collect-symbols?
		(push symbol symbols)
		(apropos-describe-symbol symbol))))
	(do-symbols (symbol package)
	  (when (search string (symbol-name symbol))
	    (if collect-symbols?
		(push symbol symbols)
		(apropos-describe-symbol symbol)))))
    (if collect-symbols?
	symbols
	(values))))

(defun apropos-describe-symbol (symbol)
  (format *standard-output* "~S~A~A~%"
	  symbol
	  (if (fboundp symbol)
	      (if (macro-name? symbol) ", Macro" ", Function")
	      "")
	  (if (boundp symbol) ", Bound" "")))

(defun apropos-list (string &optional package)
  (apropos-1 string package t))

(defun apropos (string &optional package)
  (apropos-1 string package nil))

(defun coerce-to-package (name)
  (if (packagep name)
      name
      (let ((p (typecase name
		 (simple-string (find-package name))
		 (symbol (find-package (symbol-name name)))
		 (t 0))))
	(if (packagep p)
	    p
	    (error "~S is not a package or a package name" name)))))

(defun export (symbols &optional (p *package*))
  (let ((package (coerce-to-package p)))
    (dolist (symbol (if (listp symbols) symbols (list symbols)))
      (multiple-value-bind (symbol type)
	  (find-symbol symbol package)
	(case type
	  (:internal (setf (gethash symbol (package-symbols package))
			   :external))
	  (:external nil)		; ignore
	  (:inherited (let ((table (package-symbols package)))
			(setf (gethash symbol table) :external))))))
    t))

(defun find-all-symbols (name)
  ;; name can be a string or a symbol
  (error "Write find-all-symbols"))

(defun find-package (name)
  (dolist (p *all-packages* nil)
    (when (or (string= name (package-name p))
	      (or (dolist (nickname (package-nicknames p))
		    (when (string= name nickname)
		      (return t)))))
      (return p))))

(defun-inline find-symbol (name &optional (p *package*))
  (find-symbol/3 name (coerce-to-package p) (sxhash name)))

(defun find-symbol/3 (name package hash-code)
  (unless *packages-initialized?*
    (initialize-package-structures))
  (multiple-value-bind (symbol type found?)
      (gethash-with-key name
			(package-symbols (coerce-to-package package))
			hash-code)
    (if found?
	(values symbol type)
	(dolist (used-package (package-uses-packages package)
		 (values nil nil))
	  (multiple-value-bind (symbol type found?)
	      (gethash-with-key name (package-symbols used-package) hash-code)
	    (when (and found? (eq type :external))
	      (return (values symbol :inherited))))))))

(defun import (symbols &optional (p *package*))
  (let* ((package (coerce-to-package p))
	 (table (package-symbols package)))
    (dolist (symbol (if (listp symbols) symbols (list symbols)))
      (if (symbolp symbol)
	  (setf (gethash symbol table) :internal)
	  (error "Only symbols may be imported, and ~A is not a symbol")))
    t))

(defun in-package (name &key nicknames (use '("LISP")))
  (let ((package (find-or-make-package name nicknames use)))
    (use-package use package)
    (setf *package* package)
    package))

(defun find-or-make-package (name &optional nicknames (use '("LISP")))
  (or (find-package name)
      (make-package name :nicknames nicknames :use use)))

(defun-inline intern (name &optional (p *package*))
  ;; HEY! coerce name to simple-string
  (intern/2 name (coerce-to-package p)))
  
(defun intern/2 (name package)
  (let ((hash-code (sxhash/simple-string name)))
    (multiple-value-bind (symbol type) (find-symbol/3 name package hash-code)
      (if (null type)
	  (make-new-internal-symbol name package hash-code)
	  (values symbol type)))))

(defun make-new-internal-symbol (name package hash-code)
  (let ((s (make-static-symbol name hash-code))
	(table (package-symbols package)))
    (setf (symbol-package s) package)
    (if (eq package *keyword-package*)
	(progn (setf (symbol-value s) s)
	       (setf (gethash s table) :external)
	       (values s :external))
	(progn (setf (gethash s table) :internal)
	       (values s nil)))))

(defun list-all-packages ()
  (copy-list *all-packages*))

(defun make-package (name &key (use '("LISP")) nicknames (size 200))
  (let* ((name-string (string name))
	 (abbrev-name (abbreviate-package-name name-string))
	 (abbrev-vector (vector (concatenate 'string "s_" abbrev-name "_")
				(concatenate 'string "p_" abbrev-name "_")
				(concatenate 'string "m_" abbrev-name "_")))
	 (p (find-package name-string)))
    (if (null p)
	(let ((p (fresh-package
		  :name name-string
		  :symbols (make-hash-table :test #'string= :size size)
		  :uses-packages
		  (mapcar #'coerce-to-package
			  (if (listp use) use (list use)))
		  :nicknames-list (if (listp nicknames)
				      (cons (string-upcase abbrev-name)
					    nicknames)
				      (error "~A is not a list of nicknames"
					     nicknames))
		  :static-abbreviation abbrev-vector
		  :used-by-packages nil
		  :shadowing-symbols nil)))
	  (push p *all-packages*)
	  p)
	(error "A package named ~A already exists" name-string))))

(defun package-abbrev (package index)
  (svref (if (null package)
	     #("s__" "p__" "m__")
	     (package-static-abbreviation package))
	 index))

(defun package-nicknames (p)
  (copy-list (package-nicknames-list (coerce-to-package p))))

;;; These can be sorted by marking a region and then running m-x sort-lines
(defvar *cltl2-exports*
  '(
    &allow-other-keys 
    &aux 
    &body 
    &environment 
    &key 
    &optional 
    &rest
    &whole 
    * 
    ** 
    *** 
    *applyhook* 
    *break-on-warnings* 
    *debug-io* 
    *default-pathname-defaults* 
    *error-output* 
    *evalhook* 
    *features* 
    *load-verbose* 
    *macroexpand-hook* 
    *modules* 
    *package* 
    *print-array* 
    *print-base* 
    *print-case* 
    *print-circle* 
    *print-escape* 
    *print-gensym* 
    *print-length* 
    *print-level* 
    *print-pretty* 
    *print-radix* 
    *query-io* 
    *random-state* 
    *read-base* 
    *read-default-float-format* 
    *read-suppress* 
    *readtable* 
    *standard-input* 
    *standard-output* 
    *terminal-io* 
    *trace-output* 
    + 
    ++ 
    +++ 
    - 
    / 
    // 
    /// 
    /= 
    1+ 
    1- 
    < 
    <= 
    = 
    > 
    >= 
    abs 
    acons 
    acos 
    acosh 
    adjoin 
    adjust-array 
    adjustable-array-p 
    alpha-char-p 
    alphanumericp 
    and 
    append 
    apply 
    applyhook 
    apropos 
    apropos-list 
    aref 
    array 
    array-dimension 
    array-dimension-limit 
    array-dimensions 
    array-element-type 
    array-has-fill-pointer-p 
    array-in-bounds-p 
    array-rank 
    array-rank-limit 
    array-row-major-index 
    array-total-size 
    array-total-size-limit 
    arrayp 
    ash 
    asin 
    asinh 
    assert 
    assoc 
    assoc-if 
    assoc-if-not 
    atan 
    atanh 
    atom 
    bignum 
    bit 
    bit-and 
    bit-andc1 
    bit-andc2 
    bit-eqv 
    bit-ior 
    bit-nand 
    bit-nor 
    bit-not 
    bit-orc1 
    bit-orc2 
    bit-vector 
    bit-vector-p 
    bit-xor 
    block 
    boole 
    boole-1 
    boole-2 
    boole-and 
    boole-andc1 
    boole-andc2 
    boole-c1 
    boole-c2 
    boole-clr 
    boole-eqv 
    boole-ior 
    boole-nand 
    boole-nor 
    boole-orc1 
    boole-orc2 
    boole-set 
    boole-xor 
    both-case-p 
    boundp 
    break 
    butlast 
    byte 
    byte-position 
    byte-size 
    caaaar 
    caaadr 
    caaar 
    caadar 
    caaddr 
    caadr 
    caar 
    cadaar 
    cadadr 
    cadar 
    caddar 
    cadddr 
    caddr 
    cadr 
    call-arguments-limit 
    car 
    case 
    catch 
    ccase 
    cdaaar 
    cdaadr 
    cdaar 
    cdadar 
    cdaddr 
    cdadr 
    cdar 
    cddaar 
    cddadr 
    cddar 
    cdddar 
    cddddr 
    cdddr 
    cddr 
    cdr 
    ceiling 
    cerror 
    char 
    char-bit 
    char-bits 
    char-bits-limit 
    char-code 
    char-code-limit 
    char-control-bit 
    char-downcase 
    char-equal 
    char-font 
    char-font-limit 
    char-greaterp 
    char-hyper-bit 
    char-int 
    char-lessp 
    char-meta-bit 
    char-name 
    char-not-equal 
    char-not-greaterp 
    char-not-lessp 
    char-super-bit 
    char-upcase 
    char/= 
    char< 
    char<= 
    char= 
    char> 
    char>= 
    character 
    characterp 
    check-type 
    cis 
    clear-input 
    clear-output 
    close 
    clrhash 
    code-char 
    coerce 
    common 
    commonp 
    compilation-speed 
    compile 
    compile-file 
    compiled-function 
    compiled-function-p 
    compiler-let 
    complex 
    complexp 
    concatenate 
    cond 
    conjugate 
    cons 
    consp 
    constantp 
    copy-alist 
    copy-list 
    copy-readtable 
    copy-seq 
    copy-symbol 
    copy-tree 
    cos 
    cosh 
    count 
    count-if 
    count-if-not 
    ctypecase 
    decf 
    declaration 
    declare 
    decode-float 
    decode-universal-time 
    defconstant 
    define-modify-macro 
    define-setf-method 
    defmacro 
    defparameter 
    defsetf 
    defstruct 
    deftype 
    defun 
    defvar 
    delete 
    delete-duplicates 
    delete-file 
    delete-if 
    delete-if-not 
    denominator 
    deposit-field 
    describe 
    digit-char 
    digit-char-p 
    directory 
    directory-namestring 
    disassemble 
    do 
    do* 
    do-all-symbols 
    do-external-symbols 
    do-symbols 
    documentation 
    dolist 
    dotimes 
    double-float 
    double-float-epsilon 
    double-float-negative-epsilon 
    dpb 
    dribble 
    ecase 
    ed 
    eighth 
    elt 
    encode-universal-time 
    endp 
    enough-namestring 
    eq 
    eql 
    equal 
    equalp 
    error 
    etypecase 
    eval 
    eval-when 
    evalhook 
    evenp 
    every 
    exp 
    export 
    expt 
    fboundp 
    fceiling 
    ffloor 
    fifth 
    file-author 
    file-length 
    file-namestring 
    file-position 
    file-write-date 
    fill 
    fill-pointer 
    find 
    find-all-symbols 
    find-if 
    find-if-not 
    find-package 
    find-symbol 
    finish-output 
    first 
    fixnum 
    flet 
    float 
    float-digits 
    float-precision 
    float-radix 
    float-sign 
    floatp 
    floor 
    fmakunbound 
    force-output 
    format 
    fourth 
    fresh-line 
    fround 
    ftruncate 
    ftype 
    funcall 
    function 
    functionp 
    gcd 
    gensym 
    gentemp 
    get 
    get-decoded-time 
    get-dispatch-macro-character 
    get-internal-real-time 
    get-internal-run-time 
    get-macro-character 
    get-output-stream-string 
    get-properties 
    get-setf-method 
    get-setf-method-multiple-value 
    get-universal-time 
    getf 
    gethash 
    go 
    graphic-char-p 
    hash-table 
    hash-table-count 
    hash-table-p 
    host-namestring 
    identity 
    if 
    ignore 
    imagpart 
    import 
    in-package 
    incf 
    inline 
    input-stream-p 
    inspect 
    int-char 
    integer 
    integer-decode-float 
    integer-length 
    integerp 
    intern 
    internal-time-units-per-second 
    intersection 
    isqrt 
    keyword 
    keywordp 
    labels 
    lambda 
    lambda-list-keywords 
    lambda-parameters-limit 
    last 
    lcm 
    ldb 
    ldb-test 
    ldiff 
    least-negative-double-float 
    least-negative-long-float 
    least-negative-short-float 
    least-negative-single-float 
    least-positive-double-float 
    least-positive-long-float 
    least-positive-short-float 
    least-positive-single-float 
    length 
    let 
    let* 
    lisp-implementation-type 
    lisp-implementation-version 
    list 
    list* 
    list-all-packages 
    list-length 
    listen 
    listp 
    load 
    locally 
    log 
    logand 
    logandc1 
    logandc2 
    logbitp 
    logcount 
    logeqv 
    logior 
    lognand 
    lognor 
    lognot 
    logorc1 
    logorc2 
    logtest 
    logxor 
    long-float 
    long-float-epsilon 
    long-float-negative-epsilon 
    long-site-name 
    loop 
    lower-case-p 
    machine-instance 
    machine-type 
    machine-version 
    macro-function 
    macroexpand 
    macroexpand-1 
    macrolet 
    make-array 
    make-broadcast-stream 
    make-char 
    make-concatenated-stream 
    make-dispatch-macro-character 
    make-echo-stream 
    make-hash-table 
    make-list 
    make-package 
    make-pathname 
    make-random-state 
    make-sequence 
    make-string 
    make-string-input-stream 
    make-string-output-stream 
    make-symbol 
    make-synonym-stream 
    make-two-way-stream 
    makunbound 
    map 
    mapc 
    mapcan 
    mapcar 
    mapcon 
    maphash 
    mapl 
    maplist 
    mask-field 
    max 
    member 
    member-if 
    member-if-not 
    merge 
    merge-pathnames 
    min 
    minusp 
    mismatch 
    mod 
    most-negative-double-float 
    most-negative-fixnum 
    most-negative-long-float 
    most-negative-short-float 
    most-negative-single-float 
    most-positive-double-float 
    most-positive-fixnum 
    most-positive-long-float 
    most-positive-short-float 
    most-positive-single-float 
    multiple-value-bind 
    multiple-value-call 
    multiple-value-list 
    multiple-value-prog1 
    multiple-value-setq 
    multiple-values-limit 
    name-char 
    namestring 
    nbutlast 
    nconc 
    nil 
    nintersection 
    ninth 
    not 
    notany 
    notevery 
    notinline 
    nreconc 
    nreverse 
    nset-difference 
    nset-exclusive-or 
    nstring-capitalize 
    nstring-downcase 
    nstring-upcase 
    nsublis 
    nsubst 
    nsubst-if 
    nsubst-if-not 
    nsubstitute 
    nsubstitute-if 
    nsubstitute-if-not 
    nth 
    nthcdr 
    null 
    number 
    numberp 
    numerator 
    nunion 
    oddp 
    open 
    optimize 
    or 
    otherwise 
    output-stream-p 
    package 
    package-name 
    package-nicknames 
    package-shadowing-symbols 
    package-use-list 
    package-used-by-list 
    packagep 
    pairlis 
    parse-integer 
    parse-namestring 
    pathname 
    pathname-device 
    pathname-directory 
    pathname-host 
    pathname-name 
    pathname-type 
    pathname-version 
    pathnamep 
    peek-char 
    phase 
    pi 
    plusp 
    pop 
    position 
    position-if 
    position-if-not 
    pprint 
    prin1 
    prin1-to-string 
    princ 
    princ-to-string 
    print 
    probe-file 
    proclaim 
    prog 
    prog* 
    prog1 
    prog2 
    progn 
    progv 
    provide 
    psetf 
    psetq 
    push 
    pushnew 
    quote 
    random 
    random-state 
    random-state-p 
    rassoc 
    rassoc-if 
    rassoc-if-not 
    ratio 
    rational 
    rationalize 
    rationalp 
    read 
    read-byte 
    read-char 
    read-char-no-hang 
    read-delimited-list 
    read-from-string 
    read-line 
    read-preserving-whitespace 
    readtable 
    readtablep
    real
    realpart 
    reduce 
    rem 
    remf 
    remhash 
    remove 
    remove-duplicates 
    remove-if 
    remove-if-not 
    remprop 
    rename-file 
    rename-package 
    replace 
    require 
    rest 
    return 
    return-from 
    revappend 
    reverse 
    room 
    rotatef 
    round 
    rplaca 
    rplacd 
    safety 
    satisfies 
    sbit 
    scale-float 
    schar 
    search 
    second 
    sequence 
    set 
    set-char-bit 
    set-difference 
    set-dispatch-macro-character 
    set-exclusive-or 
    set-macro-character 
    set-syntax-from-char 
    setf 
    setq 
    seventh 
    shadow 
    shadowing-import 
    shiftf 
    short-float 
    short-float-epsilon 
    short-float-negative-epsilon 
    short-site-name 
    signed-byte 
    signum 
    simple-array 
    simple-bit-vector 
    simple-bit-vector-p 
    simple-string 
    simple-string-p 
    simple-vector 
    simple-vector-p 
    sin 
    single-float 
    single-float-epsilon 
    single-float-negative-epsilon 
    sinh 
    sixth 
    sleep 
    software-type 
    software-version 
    some 
    sort 
    space 
    special 
    special-form-p 
    speed 
    sqrt 
    stable-sort 
    standard-char 
    standard-char-p 
    step 
    stream 
    stream-element-type 
    streamp 
    string 
    string-capitalize 
    string-char 
    string-char-p 
    string-downcase 
    string-equal 
    string-greaterp 
    string-left-trim 
    string-lessp 
    string-not-equal 
    string-not-greaterp 
    string-not-lessp 
    string-right-trim 
    string-trim 
    string-upcase 
    string/= 
    string< 
    string<= 
    string= 
    string> 
    string>= 
    stringp 
    structure 
    sublis 
    subseq 
    subsetp 
    subst 
    subst-if 
    subst-if-not 
    substitute 
    substitute-if 
    substitute-if-not 
    subtypep 
    svref 
    sxhash 
    symbol 
    symbol-function 
    symbol-name 
    symbol-package 
    symbol-plist 
    symbol-value 
    symbolp 
    t 
    tagbody 
    tailp 
    tan 
    tanh 
    tenth 
    terpri 
    the 
    third 
    throw 
    time 
    trace 
    tree-equal 
    truename 
    truncate 
    type 
    type-of 
    typecase 
    typep 
    unexport 
    unintern 
    union 
    unless 
    unread-char 
    unsigned-byte 
    untrace 
    unuse-package 
    unwind-protect 
    upper-case-p 
    use-package 
    user-homedir-pathname 
    values 
    values-list 
    variable 
    vector 
    vector-pop 
    vector-push 
    vector-push-extend 
    vectorp 
    warn 
    when 
    with-input-from-string 
    with-open-file 
    with-open-stream 
    with-output-to-string 
    write 
    write-byte 
    write-char 
    write-line 
    write-string 
    write-to-string 
    y-or-n-p 
    yes-or-no-p 
    zerop 
    ))

(defvar *wcl-exports*
  '(
    load-object-file
    define-library
    link-library
    link-executable
    link-bundle
    command-line-argument
    print-unreadable-object
    byte-specifier
    dynamic-extent
    machine
    quit
    &restv
    *print-structure*
    *compile-file-pathname*
    *compile-verbose*
    *constant-redefinition-verbose*
    *compile-print*
    *source-pathname*
    *target-machine*
    *sparcstation-cc*
    *sparcstation-gcc*
    cd
    compiler-macroexpand
    compiler-macroexpand-1
    declaim
    defforeign
    define-compiler-macro
    define-loop-macro
    define-loop-path
    define-loop-sequence-path
    destructuring-bind
    function-lambda-expression
    gc
    getenv
    loop
    loop-finish
    loop-named-variable
    loop-sequence-elements-path
    loop-sequencer
    loop-simplep
    loop-simplep-1
    loop-tassoc
    loop-tequal
    loop-tmember
    loop-use-system-destructuring?
    open-user-stream
    print-time
    read-vector
    write-vector
    putenv
    pwd
    shell
    uncompile
    ;; barf, defforeign wants this stuff
    =>
    void
    fptr*
    char
    char*
    float
    double
    uint32
    int
    procedure

    load-pprint
    load-clos
    load-logical-pathnames

    ;; Condition stuff
    *break-on-signals*
    *debugger-hook*
    signal
    handler-case
    handler-bind
    ignore-errors
    define-condition
    make-condition
    with-simple-restart
    restart-case
    restart-bind
    restart-name
    restart-name
    find-restart
    compute-restarts
    invoke-restart
    invoke-restart-interactively
    abort
    continue
    muffle-warning
    store-value
    use-value
    invoke-debugger
    restart
    condition
    warning
    serious-condition
    simple-condition
    simple-warning
    simple-error
    simple-condition-format-string
    simple-condition-format-arguments
    storage-condition
    stack-overflow
    storage-exhausted
    type-error
    type-error-datum
    type-error-expected-type
    simple-type-error
    program-error
    control-error
    stream-error
    stream-error-stream
    end-of-file
    file-error
    file-error-pathname
    cell-error
    unbound-variable
    undefined-function
    arithmetic-error
    arithmetic-error-operation
    arithmetic-error-operands
    package-error
    package-error-package
    division-by-zero
    floating-point-overflow
    floating-point-underflow

    ))

(defun initialize-package-structures (&optional verbose?)
  (when verbose?
    (warn "Initializing packages"))
  (intern_static_symbols)
  (setf *packages-initialized?* t)	; allow EXPORT to work.
  (export *cltl2-exports* "LISP")
  (export *wcl-exports* "LISP")
  t)

(defun package-use-list (package)
  (copy-list (package-uses-packages (coerce-to-package package))))

(defun rename-package (package name &optional (nicknames nil))
  (error "RENAME-PACKAGE isn't written yet"))

;;; Add to shadow list. If sym of name is inherited, create an
;;; new symbol that's internal to avoid ever finding the inherited one.
(defun shadow (symbols &optional (p *package*))
  (let ((package (coerce-to-package p)))
    (dolist (symbol (if (listp symbols) symbols (list symbols)))
      (multiple-value-bind (found-symbol type) (find-symbol symbol package)
	(ecase type
	  (:internal nil)
	  (:external nil)		; ???
	  ;; create an internal symbol to shadow the inherited one,
	  ;; or a new internal symbol if it did not exist:
	  ((:inherited nil)
	   (let ((s (make-new-internal-symbol (symbol-name symbol)
					      package
					      (symbol-hash-code symbol))))
	     (pushnew s (package-shadowing-symbols package)))))))
    t))

(defun shadowing-import (symbols &optional (package *package*))
  (shadow symbols package)
  (import symbols package))

(defun unintern (symbol &optional (p *package*))
  (let* ((package (coerce-to-package p))
	 (removed? (remhash symbol (package-symbols package))))
    (when removed?
      (setf (symbol-package symbol) nil))
    removed?))

(defun use-package (package-list &optional (using *package*))
  (let ((using-package (coerce-to-package using)))
    (loop for p in (if (listp package-list) package-list (list package-list))
	  do (let ((package (coerce-to-package p)))
	       (cond ((eq package *keyword-package*)
		      (error "No package can USE-PACKAGE the keyword package"))
		     ;; HEY! Add more error checks for name conflicts
		     (t (pushnew package
				 (package-uses-packages using-package)))))))
  t)

(defun write-package (p stream depth)
  (format stream "#<PACKAGE ~S (~D symbols) ~X>"
	  (package-name p)
	  (hash-table-count (package-symbols p))
	  (object->pointer p)))

