(defun qucchia/play-star-wars-music ()
  "Play a different Star Wars soundtrack depending on the day of the week."
  (emms-play-directory-tree
    (let ((day-of-week (substring (current-time-string) 0 3))
          (star-wars-directory ""))
      (when (equal day-of-week "Sun")
        (setq star-wars-directory "01 The Phantom Menace"))
      (when (equal day-of-week "Mon")
        (setq star-wars-directory "02 Attack of the Clones"))
      (when (equal day-of-week "Tue")
        (setq star-wars-directory "03 Revenge of the Sith"))
      (when (equal day-of-week "Wed")
        (setq star-wars-directory "04 A New Hope"))
      (when (equal day-of-week "Thu")
        (setq star-wars-directory "05 The Empire Strikes Back"))
      (when (equal day-of-week "Fri")
        (setq star-wars-directory "06 Return of the Jedi"))
      (when (equal day-of-week "Sat")
        (setq star-wars-directory "A New Hope (Definitive)"))
      (string-join (list "~/Music/star-wars-soundtrack/" star-wars-directory)))))

(defun qucchia/exwm-init ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Set keyboard layout
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.dotfiles/layout/.Xmodmap")

  ;; Start Tor service
  (start-process-shell-command "tor" nil "sudo systemctl start tor@default.service")

  ;; Show time
  (setq display-time-default-load-average nil)
  (setq display-time-24hr-format t)
  (display-time-mode t)

  ;; Start server to allow opening files from other applications
  (server-start)

  ;; Play music
  (qucchia/play-star-wars-music)
  (emms-mode-line-mode -1) ;; Turn modeline off

  (eshell))

(defun qucchia/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil 0 ,@(cdr command-parts)))))

(defun qucchia/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
  :config
  (setq exwm-workspace-number 5)

  (add-hook 'exwm-update-class-hook #'qucchia/exwm-update-class)
  (add-hook 'exwm-init-hook #'qucchia/exwm-init)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  
  (setq exwm-input-prefix-keys
        '(?\C-x
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
  

  (exwm-enable))
