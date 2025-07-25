(in-package :stumpwm)

(set-prefix-key (kbd "Menu"))

(define-key *top-map* (kbd "Print") '*root-map*)
(define-key *top-map* (kbd "SunPrint_Screen") '*root-map*)
(define-key *top-map* (kbd "Sys_Req") '*root-map*)
(define-key *tile-group-top-map* (kbd "Print") '*tile-group-root-map*)
(define-key *tile-group-top-map* (kbd "SunPrint_Screen") '*tile-group-root-map*)
(define-key *tile-group-top-map* (kbd "Sys_Req") '*tile-group-root-map*)
(define-key *group-top-map* (kbd "Print") '*group-root-map*)
(define-key *group-top-map* (kbd "SunPrint_Screen") '*group-root-map*)
(define-key *group-top-map* (kbd "Sys_Req") '*group-root-map*)

(define-key *root-map* (kbd "M") "meta Menu")
(define-key *root-map* (kbd "N") "eval (run-commands \"exec pkill -9 xneur\" \"exec xneur &\")")
(define-key *root-map* (kbd "M-N") "exec pkill -9 xneur")
(define-key *root-map* (kbd "~") "command-mode")
(define-key *root-map* (kbd "ESC") "abort")
(define-key *root-map* (kbd "C-r") "loadrc")
(define-key *root-map* (kbd "O") "other")
(define-key *root-map* (kbd "L") "force-redisplay")
(define-key *root-map* (kbd "C-L") "fraction-size")
(define-key *root-map* (kbd "M-l") "tag-fraction-size")

(define-key *root-map* (kbd "C-K") "withdraw-current-window")

(define-key *root-map* (kbd "M-c") "exec urxvt")
(define-key *root-map* (kbd "C-c") "exec mlterm")
(define-key *root-map* (kbd "C-M-c") "exec konsole-launcher")
(define-key *root-map* (kbd "C-h") "exec :hour-sleep")
(define-key *root-map* (kbd "C-y") "exec :away")
(define-key *root-map* (kbd "P") "exec :asleep")

(define-key *root-map* (kbd "C-v") "exec gvim")
(define-key *root-map* (kbd "C-M-v") "exec konsole-launcher -e vim")

(define-key *root-map* (kbd "M-w") "exec ~/.nix-personal/personal-result/webkit/bin/webkit-program-GtkLauncher")
(define-key *root-map* (kbd "M-W") "exec ~/.nix-personal/personal-result/webkit_gtk2/bin/webkit-program-GtkLauncher")

(define-key *root-map* (kbd "C-f") "exec konsole-launcher -e zsh -c 'choose-tmux-session web-streams ~ view-web-streams'")
(define-key *root-map* (kbd "C-F") "exec ( ls ~/queries/web-streams/::all-fresh-non-deferred/ | grep '[.]txt$' && konsole-launcher -e zsh -c 'choose-tmux-session web-streams ~ view-web-stream-texts-non-deferred' ) || ( ls ~/queries/web-streams/::all-fresh-non-deferred/fresh/ | grep -v '[.]txt$' | grep . && konsole-launcher -e zsh -c 'choose-tmux-session web-streams ~ vim -c FlattenMixedStream' )")
(define-key *root-map* (kbd "C-M") "exec konsole-launcher -e zsh -c 'launch-view-emails'")
(define-key *root-map* (kbd "C-M-M") "exec konsole-launcher -e zsh -c 'launch-view-emails edit-new-email'")
(define-key *root-map* (kbd "C-M-m") "exec konsole-launcher -e zsh -c 'launch-view-emails view-mail-summaries'")
;(define-key *root-map* (kbd "C-F") "exec uzbl $(find-related-uri)") 

(define-key *root-map* (kbd "C-B") "exec konsole-launcher -e zsh -c 'choose-tmux-session breaking-news ~ + view-breaking-news'")
(define-key *root-map* (kbd "C-M-B") "exec cleanup-notifications")

(define-key *root-map* (kbd "C-M-f") "exec firefox")

(define-key *root-map* (kbd "I") "show-im-status")

(define-key *root-map* (kbd "e") "")
(define-key *root-map* (kbd "RET") "")

(define-key *root-map* (kbd "B") "exec brightness")

(define-key *root-map* (kbd "Menu") "globally-previous-wt")
(define-key *root-map* (kbd "Print") "globally-previous-wt")
(define-key *root-map* (kbd "Sys_Req") "globally-previous-wt")
(define-key *root-map* (kbd "SunPrint_Screen") "globally-previous-wt")
(define-key *root-map* (kbd "ISO_Level3_Shift") "activate-urgent")
(define-key *root-map* (kbd "XF86MenuKB") "activate-urgent")
(define-key *root-map* (kbd "Multi_key") "activate-urgent")

(define-key *root-map* (kbd "F12") "gselect .system")
(define-key *root-map* (kbd "F12") "gselect .tag-store")
(define-key *root-map* (kbd "DEL") "gselect Default")
(define-key *root-map* (kbd "M-F11") "pull+push+renumber t")
(define-key *root-map* (kbd "F1")   "local-shell-group")
(define-key *root-map* (kbd "F2")   "eval (progn (ftg-set-tags \"im\") (number-by-tags))")
(define-key *root-map* (kbd "F3")   "ftg-set-tags mail")
(define-key *root-map* (kbd "F4")   "heavy-browser-group")
(define-key *root-map* (kbd "F5")   "light-browser-group")
(define-key *root-map* (kbd "F6")   "ftg-set-tags view")
(define-key *root-map* (kbd "F7")   "editor-group")
(define-key *root-map* (kbd "F8")   "ftg-set-tags ssh")
(define-key *root-map* (kbd "F9")   "root-shell-group")
(define-key *root-map* (kbd "M-F1") "ftg-set-tags games")
(define-key *root-map* (kbd "M-F2") "ftg-set-tags monitor")
(define-key *root-map* (kbd "M-F3") "ftg-set-tags p2p")
(define-key *root-map* (kbd "M-F4") "lazarus-layout")
(define-key *root-map* (kbd "M-F5") "ftg-set-tags qemu")
(define-key *root-map* (kbd "M-F6") "gimp-layout")
(define-key *root-map* (kbd "M-F7") "dia-layout")
(define-key *root-map* (kbd "M-F8") "media-group")

(define-key *root-map* (kbd "S-F1")   "pull-tag sh")
(define-key *root-map* (kbd "S-F2")   "pull-tag im")
(define-key *root-map* (kbd "S-F3")   "pull-tag mail")
(define-key *root-map* (kbd "S-F4")   "pull-tag ff")
(define-key *root-map* (kbd "S-F5")   "pull-tag light-browser")
(define-key *root-map* (kbd "S-F6")   "pull-tag view mplayer xine")
(define-key *root-map* (kbd "S-F7")   "pull-tag vim gvim editor")
(define-key *root-map* (kbd "S-F8")   "pull-tag ssh")
(define-key *root-map* (kbd "S-F9")   "pull-tag root")
(define-key *root-map* (kbd "S-M-F1") "pull-tag games")
(define-key *root-map* (kbd "S-M-F2") "pull-tag monitor")
(define-key *root-map* (kbd "S-M-F3") "pull-tag p2p")
;(define-key *root-map* (kbd "S-M-F4") "lazarus-layout")
(define-key *root-map* (kbd "S-M-F5") "pull-tag qemu")
;(define-key *root-map* (kbd "S-M-F6") "gimp-layout")
;(define-key *root-map* (kbd "S-M-F7") "dia-layout")
(define-key *root-map* (kbd "S-M-F8") "pull-tag media")

(define-key *root-map* (kbd "S-F11") "pull-tag t")

(define-key *root-map* (kbd "M-1") "select-window-by-number 11")
(define-key *root-map* (kbd "M-2") "select-window-by-number 12")
(define-key *root-map* (kbd "M-3") "select-window-by-number 13")
(define-key *root-map* (kbd "M-4") "select-window-by-number 14")
(define-key *root-map* (kbd "M-5") "select-window-by-number 15")
(define-key *root-map* (kbd "M-6") "select-window-by-number 16")
(define-key *root-map* (kbd "M-7") "select-window-by-number 17")
(define-key *root-map* (kbd "M-8") "select-window-by-number 18")
(define-key *root-map* (kbd "M-9") "select-window-by-number 19")
(define-key *root-map* (kbd "M-0") "select-window-by-number 10")

(define-key *root-map* (kbd "N") "repack-window-numbers")
(define-key *root-map* (kbd "N") "number-by-tags")

(define-key *root-map* (kbd "T") "ftg-set-tags")
(define-key *root-map* (kbd "C-T") "tag-window")
(define-key *root-map* (kbd "C-M-t") "window-tags")
(define-key *root-map* (kbd "C-M-T") "pull-tag")
;(define-key *root-map* (kbd "s-t") "ftg-mark-windows")
;(define-key *root-map* (kbd "s-T") "eval (progn (setf (window-tags (current-window)) '(\"NO-AUTO-TAGS\")))")
(define-key *root-map* (kbd "M-T") "untag-window")

(define-key *root-map* (kbd "x") "push-window")

(define-key *root-map* (kbd "d") "dead-windows-cleanup")

(define-key *root-map* (kbd "D") "default-tags")
(define-key *root-map* (kbd "V") "tag-visible")

(define-key *root-map* (kbd "/") "exec choose-window &")
(define-key *root-map* (kbd "M-/") "exec choose-window-and-go")
(define-key *root-map* (kbd "C-/") "exec konsole-launcher -e env NO_DISPLAY=1 in-titled-term choose-window choose-window")
(define-key *root-map* (kbd ".") "all-tags-grouped")
(define-key *root-map* (kbd "C-.") "scrollable-window-tag-list")

(define-key *root-map* (kbd "C-s") "ftg-set-tag-re")
(define-key *root-map* (kbd "C-S") "ftg-add-tag-re")

;(define-key *top-map* (kbd "H-Right") "move-focus right")
;(define-key *top-map* (kbd "H-Left") "move-focus left")
;(define-key *top-map* (kbd "H-Up") "move-focus up")
;(define-key *top-map* (kbd "H-Down") "move-focus down")

;(define-key *root-map* (kbd "s-Left") "move-windows-dir Left")
;(define-key *root-map* (kbd "s-Right") "move-windows-dir Right")
;(define-key *root-map* (kbd "s-Up") "move-windows-dir Up")
;(define-key *root-map* (kbd "s-Down") "move-windows-dir Down")

;(define-key *root-map* (kbd "H-F1") "frame-push-pull-tags sh")
;(define-key *root-map* (kbd "H-F2") "frame-push-pull-tags im")
;(define-key *root-map* (kbd "H-F3") "frame-push-pull-tags mail")
;(define-key *root-map* (kbd "H-F4") "frame-push-pull-tags heavy-browser")
;(define-key *root-map* (kbd "H-F5") "frame-push-pull-tags light-browser")
;(define-key *root-map* (kbd "H-F6") "frame-push-pull-tags view mplayer xine")
;(define-key *root-map* (kbd "H-F7") "frame-push-pull-tag vim gvim limp editor")
;(define-key *root-map* (kbd "H-F8") "frame-push-pull-tag ssh")
;(define-key *root-map* (kbd "H-F9") "frame-push-pull-tag root")
;(define-key *root-map* (kbd "H-M-F1") "frame-push-pull-tag games")
;(define-key *root-map* (kbd "H-M-F2") "frame-push-pull-tag monitor")
;(define-key *root-map* (kbd "H-M-F3") "frame-push-pull-tag p2p")
;(define-key *root-map* (kbd "H-M-F5") "frame-push-pull-tag qemu")
;(define-key *root-map* (kbd "H-M-F6") "frame-push-pull-tag gimp")
;(define-key *root-map* (kbd "H-M-F7") "frame-push-pull-tag dia")

(define-key *root-map* (kbd "SPC") "ftg-next-window")
(define-key *root-map* (kbd "M-g") "set-ftg")
(define-key *root-map* (kbd "C-Q") "only")
(define-key *root-map* (kbd "Q") "ftg-only")
(define-key *root-map* (kbd "M-f") "focus-frame-by-tag-re")

(define-key *root-map* (kbd "M-b") "ratcenter")

(define-key *root-map* (kbd "C-I") "exec konsole-launcher -e sh -c 'xtitle ii-irc-summary; ii-all-new 10 | less '")
(define-key *root-map* (kbd "C-i") "exec konsole-launcher -e ii-interactive-screen")
(define-key *root-map* (kbd "C-X") "exec konsole-launcher -e sh -c 'xtitle mcabber-xmpp-summary; mcabber-all-new 10 | less '")
(define-key *root-map* (kbd "C-x") "exec konsole-launcher -e mcabber-interactive-screen")
(define-key *root-map* (kbd "M-x") "exec konsole-launcher -e mcabber-base-screen")

(define-key *root-map* (kbd "C-!") "exec run-popular-command")
(define-key *root-map* (kbd "M-!") "exec run-popular-command-subuser")
(define-key *root-map* (kbd "C-M-!") "exec run-popular-command-lisp")
(define-key *root-map* (kbd "C-@") "exec rerun-popular-command")

;(define-key *top-map* (kbd "H-SunPageUp") "exec increase-level PCM +1")
;(define-key *top-map* (kbd "H-SunPageDown") "exec increase-level PCM -1")
;(define-key *top-map* (kbd "H-Home") "exec increase-level PCM +10")
;(define-key *top-map* (kbd "H-End") "exec increase-level PCM -10")
;(define-key *top-map* (kbd "s-SunPageUp") "exec increase-level Master +1")
;(define-key *top-map* (kbd "s-SunPageDown") "exec increase-level Master -1")
;(define-key *top-map* (kbd "s-Home") "exec increase-level Master +10")
;(define-key *top-map* (kbd "s-End") "exec increase-level Master -10")
;(define-key *top-map* (kbd "s-DEL") "exec local-levels")
;(define-key *top-map* (kbd "H-DEL") "exec no_levels")
;(define-key *top-map* (kbd "H-Pause") "exec media-screen register p p; media-screen paste p")
;(define-key *top-map* (kbd "s-Left") "exec media-screen register p h; media-screen paste p")
;(define-key *top-map* (kbd "s-Right") "exec media-screen register p l; media-screen paste p")
;(define-key *top-map* (kbd "s-Down") "exec media-screen register p H; media-screen paste p")
;(define-key *top-map* (kbd "s-Up") "exec media-screen register p L; media-screen paste p")
;(define-key *top-map* (kbd "s-Pause") "exec media-screen register p '<'; media-screen paste p")
;(define-key *top-map* (kbd "s-KP_Add") "exec media-screen readreg p ~/src/sh/script/play-random-music; media-screen paste p")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec increase-highest-level +1 PCM Master")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec increase-highest-level -1 PCM Master")
(define-key *top-map* (kbd "S-XF86AudioRaiseVolume") "exec increase-highest-level +10 PCM Master")
(define-key *top-map* (kbd "S-XF86AudioLowerVolume") "exec increase-highest-level -10 PCM Master")
(define-key *top-map* (kbd "XF86AudioMute") "exec no_levels")
(define-key *top-map* (kbd "S-XF86AudioMute") "exec local-levels")
(define-key *top-map* (kbd "S-M-XF86AudioMute") "exec my_levels")
(define-key *top-map* (kbd "M-XF86AudioMute") "exec media-screen register p play-random-music; media-screen paste p")
(define-key *top-map* (kbd "C-XF86AudioMute") "exec media-screen register p p; media-screen paste p")
(define-key *top-map* (kbd "XF86AudioPlay") "exec media-screen register p play-random-music; media-screen paste p")
(define-key *top-map* (kbd "C-XF86AudioPlay") "exec media-screen register p p; media-screen paste p")
(define-key *top-map* (kbd "C-XF86AudioLowerVolume") "exec media-screen register p h; media-screen paste p")
(define-key *top-map* (kbd "C-XF86AudioRaiseVolume") "exec media-screen register p l; media-screen paste p")
(define-key *top-map* (kbd "C-M-XF86AudioLowerVolume") "exec media-screen register p H; media-screen paste p")
(define-key *top-map* (kbd "C-M-XF86AudioRaiseVolume") "exec media-screen register p L; media-screen paste p")
(define-key *top-map* (kbd "XF86AudioPrev") "exec media-screen register p h; media-screen paste p")
(define-key *top-map* (kbd "M-XF86AudioPrev") "exec media-screen register p H; media-screen paste p")
(define-key *top-map* (kbd "XF86AudioNext") "exec media-screen register p l; media-screen paste p")
(define-key *top-map* (kbd "M-XF86AudioNext") "exec media-screen register p L; media-screen paste p")

(define-key *top-map* (kbd "XF86TouchpadToggle") "exec touchpad toggle")

(define-key *top-map* (kbd "XF86Sleep") "exec s2both")
(define-key *top-map* (kbd "C-XF86Sleep") "exec standby-fast")
;(define-key *top-map* (kbd "S-XF86Sleep") "exec susp")
(define-key *top-map* (kbd "C-S-XF86Sleep") "exec s2both-fast")
(define-key *top-map* (kbd "M-XF86Sleep") "exec standby")

(define-key *root-map* (kbd "C-D") "exec sleep 1; xset dpms force off")

(define-key *top-map* (kbd "XF86Display") "exec x-randr-options")
(define-key *top-map* (kbd "XF86ScreenSaver") "exec xscreensaver-command -lock || { @- xscreensaver ; sleep 1; xscreensaver-command -lock; } ")

(define-key *root-map* (kbd "C-:") "eval+stdout")

;(define-key *top-map* (kbd "s-S-Up") "exec settrans -c -o +5")
;(define-key *top-map* (kbd "s-S-Down") "exec settrans -c -o -5")
;(define-key *top-map* (kbd "s-S-Right") "exec settrans -c -o 100")
;(define-key *top-map* (kbd "s-S-Left") "exec settrans -c -o -20")
;(define-key *top-map* (kbd "s-S-Pause") "exec settrans -c -o 0")

;(define-key *top-map* (kbd "s-F2") "wifi")
;(define-key *top-map* (kbd "s-F9") "exec touchpad toggle")

(define-key *root-map* (kbd "C-F8") "exec ssh-window-chosen")
;(define-key *top-map* (kbd "s-s") "exec ssh-window-chosen")

(define-key *root-map* (kbd "C-F7") "open-ssh-gvim")
(define-key *root-map* (kbd "C-F6") "exec open-popular-file")

(define-key *root-map* (kbd "C-F5") "exec site-slimerjs-chosen-with-extras")
(define-key *root-map* (kbd "C-S-F5") "exec CHOOSE_SLIMERJS_USER_PREFIX=1 site-slimerjs-chosen-with-extras")

(define-key *root-map* (kbd "C-F4") "exec subuser-firefox-chosen")
(define-key *root-map* (kbd "C-S-F4") "exec marionette-save-and-open-chosen")
(define-key *root-map* (kbd "C-S-M-F4") "exec download-link \"$(xclip -o -sele primary)\" /dev/null open")

(define-key *root-map* (kbd "C-F1") "exec konsole-launcher -e choose-tmux-session")

;(define-key *top-map* (kbd "s-f") "firefox-form-fill")

(define-key *root-map* (kbd "M-i") "exec for o in $(seq 0 10); do xcalib -a -i -o $o & done")

;(define-key *top-map* (kbd "s-c") "exec echo -e 'ButtonPress 1\\n' | xmacroplay $DISPLAY")

;(define-key *top-map* (kbd "s-v") "open-ssh-gvim")

;(define-key *top-map* (kbd "s-t") "choose-tag-combo")

(define-key *top-map* (kbd "C-ISO_Level3_Shift") "exec type-chosen-string")
(define-key *top-map* (kbd "C-M-ISO_Level3_Shift") "exec type-unicode-symbol")
(define-key *top-map* (kbd "C-XF86MenuKB") "exec type-chosen-string")
(define-key *top-map* (kbd "C-M-XF86MenuKB") "exec type-unicode-symbol")
(define-key *top-map* (kbd "C-Multi_key") "exec type-chosen-string")
(define-key *top-map* (kbd "C-M-Multi_key") "exec type-unicode-symbol")

(define-key *top-map* (kbd "M-!") "eval (xlib:lock-group *display* :group 0)")
(define-key *top-map* (kbd "M-@") "eval (xlib:lock-group *display* :group 1)")
(define-key *top-map* (kbd "M-#") "eval (xlib:lock-group *display* :group 2)")
(define-key *top-map* (kbd "C-M-!") "eval (progn (xlib:lock-group *display* :group 0) (set-window-layout 0))")
(define-key *top-map* (kbd "C-M-@") "eval (progn (xlib:lock-group *display* :group 1) (set-window-layout 1))")
(define-key *top-map* (kbd "C-M-#") "eval (progn (xlib:lock-group *display* :group 2) (set-window-layout 2))")
(define-key *top-map* (kbd "C-M-~") "eval (progn (set-window-layout \"!\"))")
(define-key *top-map* (kbd "M-~") "eval (progn (set-window-layout \"~\"))")

(define-key *root-map* (kbd "C-M-S-Left") "sprev")
(define-key *root-map* (kbd "C-M-S-Right") "snext")

(define-key *root-map* (kbd "[") "move-focus left")
(define-key *root-map* (kbd "]") "move-focus right")
(define-key *root-map* (kbd "{") "move-focus up")
(define-key *root-map* (kbd "}") "move-focus down")
(define-key *root-map* (kbd "M-[") "move-window left")
(define-key *root-map* (kbd "M-]") "move-window right")
(define-key *root-map* (kbd "M-{") "move-window up")
(define-key *root-map* (kbd "M-}") "move-window down")

(define-key *top-map* (kbd "C-:") "exec xdotool sleep 0.3 click 1")
(define-key *top-map* (kbd "C-M-:") "exec yes 'click -d 1 1 sleep 0.001' | head -n 300 | xargs xdotool sleep 0.3")

(define-key *root-map* (kbd "M--") "exec konsole-launcher -e choose-tmux-session -")
