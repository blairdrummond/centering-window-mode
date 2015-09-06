;; Based mostly on these
;; https://github.com/anler/centered-window-mode
;; http://alexkehayias.tumblr.com/post/98888273308/simple-centered-text-mode-in-emacs


;; Remove minibuffer from window-list
(defun list-windows () (window-list (selected-frame) -1))


;; center a window
(defun center-text (arg)
  "Center the text in the middle of the buffer. Works best in full screens"
  (let ((left     (car (window-margins     arg)))
	(right    (cdr (window-margins     arg)))
	(left-to  (/   (window-total-width arg) 4))
	(right-to (/   (window-total-width arg) 4)))
    (unless (and left (>= left left-to) right (>= right right-to))
      (set-window-margins arg left-to right-to))))



;; remove centering from window
(defun center-text-clear (arg)
  (set-window-margins arg nil nil))



;; Add hooks to detect changes on all windows
(defun centering-setup ()
  (add-hook
   'window-configuration-change-hook
   'centering-window-configuration-change)
  (centering-window-configuration-change))



;; Remove hooks
(defun centering-teardown ()
    (mapcar 'center-text-clear (list-windows))
    (remove-hook
     'window-configuration-change-hook
     'centering-window-configuration-change))



;; autodetect what to do
(defun centering-window-configuration-change ()
  (mapcar 'choose (list-windows)))


;; Manually override the settings for a window
(set-default 'override nil)
(defun choose (arg)
  (let ((user-overridden (buffer-local-value 'override (window-buffer arg)))
	(good (window-full-width-p arg)))
  (cond
   ((eq user-overridden  1)  (center-text       arg)) ;; Defaults centered
   ((eq user-overridden -1)  (center-text-clear arg)) ;; Defaults not centered
   (good                     (center-text       arg))
   ((not good)               (center-text-clear arg)))))


;; the following 3 set the behaviour of a window
(defun centering-override-center ()
  (interactive)
  (setq-local override 1)
  (centering-window-configuration-change))

(defun centering-override-no-center ()
  (interactive)
  (setq-local override -1)
  (centering-window-configuration-change))

(defun centering-override-default ()
  (interactive)
  (setq-local override nil)
  (centering-window-configuration-change))


;;;###autoload
(setq has-been-set nil)
(define-minor-mode centering-window-mode
  (if has-been-set
      (progn
	(centering-teardown)
	(setq has-been-set nil))
    (progn
      (centering-setup)
      (setq has-been-set t))))



(provide 'centering-window-mode)
(provide 'centering-override-center)
(provide 'centering-override-no-center)
(provide 'centering-override-default)
