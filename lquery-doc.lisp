#|
  This file is a part of lQuery-Doc
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.lquery.doc
  (:use :cl :lquery :cxml-dom :alexandria
        #+:lispworks :clos
        #+:sbcl :sb-mop
        #+:allegro :mop)
  (:nicknames :lquery-doc)
  (:export :write-documentation
           :documentate-object
           :get-symbol-info))
(in-package :org.tymoonnext.radiance.lib.lquery.doc)

(defun write-documentation (package input-file
                            &key
                              (output-file input-file)
                              (target "#docs")
                              (template "#template")
                              (fields '(:name (".funcname")
                                        :desc (".funcdesc")
                                        :args (".funcargs")
                                        :type (".functype")))
                              (exclude '(:internal))
                              (modifier #'documentate-object)
                              (if-exists :supersede)
                              (if-does-not-exist :create))
  "Writes the documentation for a package into the specified HTML file and writes it back out. target denotes a selector for a container for the resulting HTML nodes. It is emptied before filling. Template is a selector to the template to use for the documentation. The rest is explained in the documentate function."
  (let ((old-doc (first ($)))
        (new-doc ($ (initialize input-file)))
        (nodes (documentate ($ template) package :fields fields :exclude exclude :modifier modifier)))
    ($ target (empty) (append nodes))
    ($ (write-to-file output-file :if-exists if-exists :if-does-not-exist if-does-not-exist))
    (lquery:initialize old-doc)))

(defun documentate (template package 
                    &key
                      (fields '(:name (".funcname")
                                :desc (".funcdesc")
                                :args (".funcargs")
                                :type (".functype")))
                      (exclude '(:internal))
                      (modifier #'documentate-object))
  "Create a list of documentation blocks using the template node. The data is filled into the fields selected by the targets. Targets is a plist with :name, :desc, :args and :type as keys and lists of selectors as values. Any symbol in the exclude list will be skipped. Alternatively, the following symbols allow for more general exclusion: :internal :external :inherited :constant :special :class :function :macro :generic :method :missing-docstring.

It is expected that lQuery has already been initialized."
  (let ((template (first (lquery::nodes-or-build template))))
    (alexandria:flatten
     (loop for symbol in (sort (get-all-symbols package) #'string-lessp :key (lambda (a) (format NIL "~a" a)))
        collect (loop for object in (get-symbol-info symbol)
                   for type = (nth 1 object)
                   for scope = (nth 2 object)
                   for docstring = (nth 3 object)
                   for args = (nth 4 object)
                   for target = (dom:clone-node template T)
                   collect (if (and (not (find symbol exclude))
                                    (not (find scope exclude))
                                    (not (find type exclude))
                                    (not (and (find :missing-docstring exclude) (not docstring))))
                               (funcall modifier target object fields)))))))

(defgeneric documentate-object (template object fields)
  (:documentation "Changes the given template node to include all required information stored in object (see get-symbol-info) on the given fields."))

(defmethod documentate-object (template object fields)
  "Create a filled documentation template for the specified object."
  (let ((vars (list :name (symbol-name (nth 0 object))
                    :desc (format NIL "~a" (nth 3 object))
                    :args (format NIL "~a" (nth 4 object))
                    :type (format NIL "~a ~a" (nth 2 object) (nth 1 object)))))
    (loop for (key val) on fields by #'cddr
         do (loop for field in val
                 do ($ template field (text (getf vars key))))))
  template)

(defun get-symbol-info (symbol)
  "Gets all necessary information for the symbol. Returns a list containing symbol, type, docstring and args."
  (let ((types (get-types-of-symbol symbol))
        (info ()))
    (loop for type in types
       collect (list symbol
                     type
                     (symbol-scope symbol)
                     (get-docstring-of-symbol symbol type)
                     (get-args-of-symbol symbol)) into result
       finally (setf info result))
    ;Special handling for methods
    (if (find :generic types)
        (loop for method in (generic-function-methods (fdefinition symbol))
             collect (list symbol
                           :method
                           (symbol-scope symbol)
                           (documentation method T)
                           (append (get-args-of-symbol symbol) (method-specializers method))) into result
             finally (setf info (append info result))))
    info))

(defun get-args-of-symbol (symbol)
  "Retrieves the arguments of a symbol as a list."
  #+:sbcl
  (sb-introspect:function-lambda-list symbol)
  #+:allegro
  (excl:arglist symbol))

(defun get-docstring-of-symbol (symbol type)
  "Retrieves the docstring of a symbol of a given type."
  (case type
    ((:function :macro :generic) (documentation symbol 'function))
    (:method (loop for method in (generic-function-methods (fdefinition symbol))
                collect (documentation method T)))
    ((:special :constant) (documentation symbol 'variable))
    (:class (documentation symbol 'type))))

(defun get-types-of-symbol (symbol)
  "Retrieves a list of all types of objects this symbol can represent."
  (remove NIL `(,(when (symbol-constant-p symbol) :constant)
                 ,(when (symbol-special-p symbol) :special)
                 ,(when (symbol-class-p symbol) :class)
                 ,(when (symbol-macro-p symbol) :macro)
                 ,(when (symbol-generic-p symbol) :generic)
                 ,(when (symbol-function-p symbol) :function))))

(defun get-all-symbols (&optional package)
  "Gets all symbols within a package."
  (let ((lst ())
        (package (find-package package)))
    (do-all-symbols (s lst)
      (when (fboundp s)
        (if package
            (when (eql (symbol-package s) package)
              (push s lst))
            (push s lst))))
    lst))

(defun symbol-constant-p (symbol)
  "Returns true if the symbol represents a constant."
  #+:lispworks (sys:symbol-constant-p symbol)
  #-:lispworks (constantp symbol))

(defun symbol-special-p (symbol)
  "Returns true if the symbol represents a special variable."
  (and (not (symbol-constant-p symbol))
       #+:lispworks (sys:declared-special-p symbol)
       #+:sbcl (eql :special (sb-int:info :variable :kind symbol))
       #+:allegro (eq (sys:variable-information symbol) :special)))

(defun symbol-class-p (symbol)
  "Returns true if the symbol represents a class."
  (if (find-class symbol nil) T NIL))

(defun symbol-macro-p (symbol)
  "Returns true if the symbol represents a macro."
  (and (fboundp symbol)
       (macro-function symbol)))

(defun symbol-generic-p (symbol)
  "Returns true if the symbol represents a generic function."
  (and (fboundp symbol)
       (typep (fdefinition symbol) 'standard-generic-function)))

(defun symbol-function-p (symbol)
  "Returns true if the symbol represents a function."
  (and (fboundp symbol)
       (or (listp symbol)
           (not (macro-function symbol)))
       (not (typep (fdefinition symbol) 'standard-generic-function))))

(defun symbol-scope (symbol)
  "Returns true if the symbol is external."
  (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol))))
