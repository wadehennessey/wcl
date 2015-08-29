
(in-package "PCL" :use (list (or (find-package :walker)
				 (make-package :walker :use '(:lisp)))
			     (or (find-package :iterate)
				 (make-package :iterate
					       :use '(:lisp :walker)))
			     (find-package :lisp)))

(export (intern (symbol-name :iterate)		;Have to do this here,
		(find-package :iterate))	;because in the defsystem
	(find-package :iterate))		;(later in this file)
						;we use the symbol iterate
						;to name the file

