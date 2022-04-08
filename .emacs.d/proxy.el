(defcustom dolly-use-tor t
    "If non-nil, use torsocks in `dolly/start-process-shell-command'."
    :type 'boolean
    :group 'dolly)
  
  (defun dolly-toggle-tor ()
    "Toggle `dolly/use-tor'."
    (interactive)
    (setq dolly/use-tor (not dolly/use-tor)))
  
  (defun dolly/start-process-shell-command (name buffer command)
    "Start program in a subprocess.  If `dolly/use-tor' is non-nil, wrap it in torsocks.

NAME is a name for process.
BUFFER is the buffer (or buffer name) to associate with the process.
COMMAND is the shell command to run.
See `start-process-shell-command' for more details."
    (if dolly/use-tor
      (start-process-shell-command name buffer (string-join (list "torsocks " command)))
      (start-process-shell-command name buffer command)))
      
  (provide 'dolly)
