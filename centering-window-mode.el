;; Based mostly on these
;; https://github.com/anler/centered-window-mode
;; http://alexkehayias.tumblr.com/post/98888273308/simple-centered-text-mode-in-emacs


;; This is used While in centered mode, because windows drop
;; out of centered mode in splitscreen
(setq stay-centered nil)

;; https://github.com/emacs-mirror/emacs/blob/master/lisp/scroll-all.el
(defun one-function-for-all (func)
  "Apply function FUNC to all visible windows."
  (let ((num-windows (count-windows))
	(count 1))
    (when (> num-windows 1)
      (other-window 1)
      (while (< count num-windows)
	(condition-case nil
	    (funcall func)
	  ;; Ignore beginning- or end-of-buffer error in other windows.
	  (error nil)
	  )
	(other-window 1)
	(setq count (1+ count))))))


;; center text
(defun center-text ()
  "Center the text in the middle of the buffer. Works best in full screen"
  (interactive)
  (progn
    (let ((left     (car (window-margins)))
	  (right    (cdr (window-margins)))
	  (left-to  (/   (window-width) 4))
	  (right-to (/   (window-width) 4)))

      (unless (and left (>= left left-to) right (>= right right-to))
	(set-window-margins
	 (car (get-buffer-window-list (current-buffer) nil t))
	 (/ (window-width) 4)
	 (/ (window-width) 4)))
      (setq stay-centered t))))



;; remove centering
(defun center-text-clear ()
  (interactive)
  (progn
    (set-window-margins
     (car (get-buffer-window-list (current-buffer) nil t))
     nil
     nil)
    (setq stay-centered nil)))



;; toggle
(defun centering-toggle ()
  (interactive)
  (let ((left     (car  (window-margins)))
	(right    (cdr  (window-margins))))
    (if (or left right (and left (< 0 left)) (and right (< 0 right)))
	(center-text-clear)
      (center-text))))



;; Add hooks to detect changes
(defun cwm/setup ()
  (add-hook
   'window-configuration-change-hook
   'cwm/window-configuration-change)
  (cwm/window-configuration-change))



;; Remove hooks
(defun cwm/teardown ()
  (progn
    (one-function-for-all 'center-text-clear)
    (remove-hook
     'window-configuration-change-hook
     'cwm/window-configuration-change)))



;; Remove centering on right-split
(defadvice split-window-right
    (before cwm/reset-on-split activate)
  (center-text-clear))



;; autodetect what to do
(defun cwm/window-configuration-change ()
  (if (or stay-centered (< (length (window-list)) 2))
      (center-text)
    (center-text-clear)))



;;;###autoload
(setq has-been-set nil)
(define-minor-mode my-centered-window-mode
  (if has-been-set
      (progn
	(center-text-clear)
	(cwm/teardown)
	(setq has-been-set nil))
    (progn
      (cwm/setup)
      (setq has-been-set t))))


(provide 'centering-window-mode)
(provide 'centering-toggle)
