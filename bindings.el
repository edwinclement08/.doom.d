
(map! :leader
      (:prefix ("r" . "roam")
               "f" #'org-roam-node-find
               "i" #'org-roam-node-insert
               "b" #'org-roam-buffer-toggle
               "n" #'org-roam-capture
               "u" #'org-roam-ui-open
               (:prefix ("F" . "Find via Tag")
                        "m" #'edwin/org-roam-find-masters
                        "p" #'edwin/org-roam-find-project
                        "u" #'edwin/org-roam-find-university
                        )
               )
      )

(map! :leader
      :prefix ("e" . "edwin")
      "1" #'edwin-frame-1
      "2" #'edwin-frame-2
      "3" #'edwin-frame-3
      "p" #'edwin-center

      )

(map! :leader
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "5" #'winum-select-window-5
      "6" #'winum-select-window-6
      "7" #'winum-select-window-7
      "8" #'winum-select-window-8
      "9" #'winum-select-window-9
      )

(map! :leader
       (:prefix-map ("l" . "workspace")
        :desc "Display tab bar"           "`" #'+workspace/display
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "TAB"   #'+workspace/other
        :desc "New workspace"             "n"   #'+workspace/new
        :desc "New named workspace"       "N"   #'+workspace/new-named
        :desc "Load workspace from file"  "l"   #'+workspace/load
        :desc "Save workspace to file"    "s"   #'+workspace/save
        :desc "Delete session"            "x"   #'+workspace/kill-session
        :desc "Delete this workspace"     "d"   #'+workspace/delete
        :desc "Rename workspace"          "r"   #'+workspace/rename
        :desc "Restore last session"      "R"   #'+workspace/restore-last-session
        :desc "Next workspace"            "]"   #'+workspace/switch-right
        :desc "Previous workspace"        "["   #'+workspace/switch-left
        :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
        :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
        :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
        :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
        :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
        :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
        :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
        :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
        :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
        :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))

(map! :leader
      "TAB" #'evil-switch-to-windows-last-buffer
      )

(map! :leader
      ":" #'projectile--find-file
      "SPC" #'execute-extended-command
      )
