(require 'avy)
(setq avy-timeout-seconds 0.25)
(define-key global-map (kbd "H-SPC") 'avy-goto-char-timer)
(global-unset-key (kbd "<end>"))
(global-set-key (kbd "<end>") 'avy-goto-char-timer)
(define-key global-map (kbd "H-S-SPC") 'avy-pop-mark)
(define-key global-map (kbd "M-z") 'avy-zap-up-to-char)
(define-key global-map (kbd "M-SPC") 'avy-goto-end-of-line)
(define-key global-map (kbd "M-S-SPC") 'avy-goto-line)

(setq avy-keys (nconc (number-sequence ?a ?z)
                      (number-sequence ?A ?Z)))

(setf (alist-get ?\C-y avy-dispatch-alist) 'avy-action-yank 
      (alist-get ?\M-w avy-dispatch-alist) 'avy-action-copy 
      (alist-get ?\C-k avy-dispatch-alist) 'avy-action-kill-move 
      (alist-get ?\C-t avy-dispatch-alist) 'avy-action-teleport)

(defun avy-generic-command-action (action-f)
  "Excecutes action-f at point and stays"
  (save-excursion
    (goto-char pt)
    (funcall action-f))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(defun avy-generic-command-action-no-stay (action-f)
  "Excecutes action-f at point and returns to original position"
  (goto-char pt)
  (funcall action-f)
  t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char (+ 1 pt)))
(setf (alist-get 67108896 avy-dispatch-alist) 'avy-action-mark-to-char) ; C-SPC

(defun avy-action-helpful (pt)
  (avy-generic-command-action #'helpful-at-point))
(setf (alist-get ?\C-h avy-dispatch-alist) 'avy-action-helpful)

(defun avy-action-flyspell (pt)
  (avy-generic-command-action #'flyspell-auto-correct-word))
(setf (alist-get ?\C-. avy-dispatch-alist) 'avy-action-flyspell)

(defun avy-action-open-at-point (pt)
  (goto-char pt)
  (org-open-at-point)
  t)
(setf (alist-get ?\C-o avy-dispatch-alist) 'avy-action-open-at-point)

(defun avy-action-clone-line (pt)
    (goto-char pt)
    (move-beginning-of-line 1)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end))
    (yank)
    (indent-for-tab-command)
    t)
(setf (alist-get ?\C-l avy-dispatch-alist) 'avy-action-clone-line)

(defun avy-action-mark-point (pt)
  "Sets a point for other commands"
  (setq my-avy-point pt)
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  (message "Point set!"))
(setf (alist-get ?. avy-dispatch-alist) 'avy-action-mark-point)

(defun avy--quick-mark-region (pt)
  "Intermediate function to mark regions, used in region actions"
  (when (> my-avy-point pt)
    (progn
      (setf aux pt)
      (setf pt my-avy-point)
      (setf my-avy-point aux)))
  (goto-char my-avy-point)
  (set-mark my-avy-point)
  (activate-mark)
  (goto-char (+ 1 pt))
  (setq my-avy-point nil))

(defun avy--return-point-region-action ()
  "Makes sure that the point returns to its original place even if it is in another window"
  (let ((dat (ring-ref avy-ring 0)))
    (select-frame-set-input-focus
     (window-frame (cdr dat)))
    (select-window (cdr dat))
    (goto-char (car dat))))

(defun avy--check-for-region-errors ()
  "Cheks if set point action was previously made, cleans action otherwise"
  (progn (message "No point set") 
         (avy--return-point-region-action)
         nil))

(defun avy-action-copy-region (pt)
  "Copy region and stays"
  (if my-avy-point
      (progn 
        (save-excursion
          (avy--quick-mark-region pt)
          (call-interactively 'kill-ring-save))
        (avy--return-point-region-action)
        (message "Copied: %s" (current-kill 0))
        t)
    (avy--check-for-region-errors)))
(setf (alist-get ?\M-W avy-dispatch-alist) 'avy-action-copy-region)

(defun avy-action-yank-region (pt)
  "Yank region and stays"
  (avy-action-copy-region pt)
  (yank)
  t)
(setf (alist-get 33554457 avy-dispatch-alist) 'avy-action-yank-region) ; C-Y

(defun avy-action-kill-region-move (pt)
  "Kills a region and moves"
  (if my-avy-point
      (progn 
        (avy--quick-mark-region pt)
        (call-interactively 'kill-region)	     
        (message "Killed: %s" (current-kill 0))
        (point)
        t)
    (avy--check-for-region-errors)))
(setf (alist-get 33554443 avy-dispatch-alist) 'avy-action-kill-region-move) ; C-K

(defun avy-action-teleport-region (pt)
  "Teleports an arbitrary region using my-avy-point"
  (if my-avy-point
    (progn
      (save-excursion
        (avy--quick-mark-region pt)
        (call-interactively 'kill-region))
      (select-window
       (cdr
        (ring-ref avy-ring 0)))
      (yank)
      t)
    (avy--check-for-region-errors)))
(setf (alist-get 33554452 avy-dispatch-alist) 'avy-action-teleport-region) ; C-T

(defun avy-goto-quick-char (char &optional arg)
  "Simulates char press for filtering"
  (interactive (list char
                     current-prefix-arg))
  (avy-with avy-goto-char
    (avy-jump

     (regexp-quote (string char)))))

(defun avy-goto-parenthesis ()
  "Filter avy selecton with open parenthesis"
  (interactive)
  (avy-goto-quick-char 40)) ;; (
(define-key emacs-lisp-mode-map (kbd "S-SPC") 'avy-goto-parenthesis)
(define-key clojure-mode-map (kbd "S-SPC") 'avy-goto-parenthesis)

(defun avy-set-auto-action (res action)
  "Sets an auto action to excecute on the next avy interaction, it is necessary to unset afterwards if not longer desired
res param is needed for default behavior"
  (avy-pre-action-default res)
  (setq avy-action action))

(defun avy-unset-auto-action ()
  "Unsets an auto action"
  (setq avy-pre-action 'avy-pre-action-default))

(setq processing-compound? nil)
(setq avy-interrupted? nil)

(defun clean-avy-auto-action ()
  "Resets default  avy preaction and compound flag"
  (avy-unset-auto-action)
  (setq processing-compound? nil))

(defun avy-intial-interruption-check (&optional arg)
  "Function to run before avy command to check if the last action was interrupted"
  (if avy-interrupted?
      (progn
        (clean-avy-auto-action))
    (setq avy-interrupted? t)))

(defun avy-set-no-interruption (&optional arg)
  "Function to run after avy command to state that there were no interruption"
  (setq avy-interrupted? nil))

(advice-add 'avy-goto-char-timer :before #'avy-intial-interruption-check)
(advice-add 'avy-goto-char-timer :after #'avy-set-no-interruption)

(defun avy-auto-set-point-preaction (res)
  "Preaction for setting the point as part of a region action"
  (avy-set-auto-action res 'avy-action-mark-point))


(defun avy-region-compound ()
  "Compound for manipulating regions"
  (interactive)
  (setq processing-compound? t)
  (setq avy-pre-action 'avy-auto-set-point-preaction)
  (when (call-interactively 'avy-goto-char-timer) ; nil if interrupted
    (avy-unset-auto-action)
    (call-interactively 'avy-goto-char-timer))
  (clean-avy-auto-action))

 (define-key global-map (kbd "C-H-SPC") 'avy-region-compound)

(defun avy-action-yank-proxy (pt)
  "Applies regular or region based yank"
  (if processing-compound?
      (avy-action-yank-region pt)
    (avy-action-yank pt)))

(setf (alist-get ?\C-y avy-dispatch-alist) 'avy-action-yank-proxy) ; override

(defun avy-action-kill-proxy (pt)
  "Applies regular or region based kill"
  (if processing-compound?
      (avy-action-kill-region-move pt)
    (avy-action-kill-move pt)))

(setf (alist-get ?\C-k avy-dispatch-alist) 'avy-action-kill-proxy)

(defun avy-action-teleport-proxy (pt)
  "Applies regular or region based teleport"
  (if processing-compound?
      (avy-action-teleport-region pt)
    (avy-action-teleport pt)))

(setf (alist-get ?\C-t avy-dispatch-alist) 'avy-action-teleport-proxy)

(defun avy-action-copy-proxy (pt)
  "Applies regular or region based copy"
  (if processing-compound?
      (avy-action-copy-region pt)
    (avy-action-copy pt)))

(setf (alist-get ?\M-w avy-dispatch-alist) 'avy-action-copy-proxy)

(defun avy-auto-yank-preaction (res)
  "Preaction for yanking"
  (avy-set-auto-action res 'avy-action-yank))

(defun avy-replace-action (pt)
  "Compound action for replacing a candidate with another candidate"
  (avy-action-kill-move pt)
  (setq avy-pre-action 'avy-auto-yank-preaction)
  (setq avy-interrupted? nil) ; next goto-char cleans
  (call-interactively 'avy-goto-char-timer)
  (clean-avy-auto-action)
  (call-interactively 'avy-pop-mark)
  (call-interactively 'avy-pop-mark))

(setf (alist-get ?\C-r avy-dispatch-alist) 'avy-replace-action)

(defun avy-action-lsp-help (pt)
  (avy-generic-command-action #'lsp-describe-thing-at-point))
(setf (alist-get 16777320 avy-dispatch-alist) 'avy-action-lsp-help) ; H-h

(defun avy-action-lsp-goto-definition (pt)
  (avy-generic-command-action-no-stay #'lsp-find-definition))
(setf (alist-get 16777319 avy-dispatch-alist) 'avy-action-lsp-goto-definition) ; H-g

(defun avy-action-lsp-goto-references (pt)
  (avy-generic-command-action-no-stay #'lsp-find-references))
(setf (alist-get 16777336 avy-dispatch-alist) 'avy-action-lsp-goto-references) ; H-x

(defun avy-action-lsp-rename (pt)
  (avy-generic-command-action
   (lambda () (call-interactively 'lsp-rename))))
(setf (alist-get 16777330 avy-dispatch-alist) 'avy-action-lsp-rename) ; H-r
