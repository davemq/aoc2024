(let*
    (
     left
     right
     (buffer (find-file "input.txt"))
     (str (buffer-string))
     (lines (string-lines str))
     )
  (dolist (l lines)
    (let (
	  (values (split-string l))
	  )
      (setq left (append left (list (string-to-number (nth 0 values)))))
      (setq right (append right (list (string-to-number (nth 1 values)))))
      )
    ;;; sort left and right numerically
    )
  )
