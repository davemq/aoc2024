(let
    (
     left
     right
     )
  (find-file "input.txt")
  (dolist (l (string-lines (buffer-string)))
    (let (
	  (values (split-string l))
	  )
      (setq left (append left (list (string-to-number (nth 0 values)))))
      (setq right (append right (list (string-to-number (nth 1 values)))))
      )
    ;;; sort left and right numerically
    )
  )
