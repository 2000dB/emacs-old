(defun temp-el-070722 ()
  (remove-text-properties 1 25 '(syntax-table nil))
  (syntax-ppss-flush-cache -1)
  (setq syntax-ppss-last nil)
  (goto-char (point-min))
  (let ((parse-sexp-lookup-properties t)) (syntax-ppss 24))
  )
