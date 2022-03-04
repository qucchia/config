(defun q/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil 0 ,@(cdr command-parts)))))

(defun q/exwm-init-hoom ()
  ;; Make workspace 1 be the one where we land at startup
  (exwk-workspace-switch-create 1)

  (eshell))

(defun q/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
  :config
  (setq exwm-workspace-number 5)

  (add-hook 'exwm-update-class-hook #'q/exwm-update-class)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (setq exwm-input-prefix-keys
     '(?\C-x
       ?\C-u
       ?\C-h
       ?\M-x
       ?\M-`
       ?\M-&
       ?\M-:
       ?\C-\M-j
       ?\C-\ )) ;; C-SPC

  ;; C-q will always send the next key directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (setq exwm-input-global-keys
      `(
           ;; Reset to line-mode
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([?\s-h] . windmove-left)
          ([?\s-j] . windmove-down)
          ([?\s-k] . windmove-up)
          ([?\s-l] . windmove-right)

          ;; Launch applications with shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  ;; Show time
  (setq display-time-default-load-average nil)
  (setq display-time-24hr-format t)
  (display-time-mode t)

  ;; Start server to allow opening files from other applications
  (server-start)

  (exwm-enable)

  (eshell-command "ssh-add ~/.ssh/codeberg"))
