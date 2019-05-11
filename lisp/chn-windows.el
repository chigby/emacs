(set-default-font "Consolas 14")

(use-package powershell)
(use-package ahk-mode)

(defun run-powershell ()
  "Run powershell"
  (interactive)
  (async-shell-command "c:/windows/system32/WindowsPowerShell/v1.0/powershell.exe -Command -"
                       nil
                       nil))


(defun run-powershell2 ()
  "Run powershell"
  (interactive)
  (async-shell-command "C:/Program Files/PowerShell/6.0.3/pwsh.exe -Command -"
                       nil
                       nil))
