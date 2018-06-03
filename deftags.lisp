(in-package :stumpwm)
(defun union-mild (a b) (union a b :test 'equalp))

(defun is-room (x s)
  (or
   (equal (window-role x) s)
   (equal (window-title x) s)
   (and
    (equal (window-class x) "Vacuum")
    (equal
     (cl-ppcre:regex-replace-all "[@].*" s "")
     (cl-ppcre:regex-replace-all " .*" (window-title x) "")
     )
    )
   ))

(defun deftags (x)
  (unless 
    (find "no-auto-tags" (windowtags::window-tags x) :test 'equalp)
    (reduce 
      #'union-mild 
      (list
	(mapcar (lambda(x) (cl-ppcre:regex-replace-all " " x "-"))
		(list 
		  (window-class x)
		  (concatenate 'string "i/" (window-res x))
		  (concatenate 'string "c/" (window-class x))
		  (concatenate 'string "r/" (window-role x))
		  (concatenate 'string "w/" 
			       (write-to-string 
				 (xlib:window-id 
				   (window-xwin x))))
		  ))
	(if (and 
	      (or 
		(equal (window-class x) "Carrier")
		(equal (window-class x) "Pidgin")
		) 
	      (equal (window-role x) "buddy_list")) 
	  (list 
	    ;"1" 
	    "im" "conversation" "base") nil)
	(if (or
	      (equal (window-title x) "Main 'screen' instance")
	      (equal (window-title x) "Meta 'screen' instance")
	      (equal (window-title x) "Meta 'screen' instance to rule them all")
	      )
	  (list 
	    ;"0" 
	    "base" "sh" "xkbgr/!") nil)
	(if (and 
	      (or 
		(equal (window-class x) "Carrier")
		(equal (window-class x) "Pidgin")
		) 
	      (equal (window-role x) "conversation")) 
	  (list 
	    ;"2" 
	    "im" "base") nil)
	(if (and
	      (equal (window-class x) "Gajim")
	      (equal (window-role x) "roster")
	      )
	  (list 
	    ;"2" 
	    "0"
	    "im" "base") nil)
	(if (and
	      (equal (window-class x) "psi")
	      )
	  (list 
	    "im" "base") nil)
	(if (and
	      (equal (window-class x) "Vacuum")
	      )
	  (list 
	    "im" "base") nil)
	(if (and
	      (equal (window-class x) "psi")
	      (equal (window-res x) "main")
	      )
	  (list 
	    ;"2" 
	    "0"
	    ) nil)
	(if (and
	      (equal (window-class x) "Vacuum")
	      (starts-with-subseq  "Vacuum-IM - " (window-title x))
	      )
	  (list 
	    ;"2" 
	    "0"
	    ) nil)
	(if (and
	      (equal (window-class x) "Gajim")
	      )
	  (list "im" "base" "gajim") nil)
	(if (or
	      (and
		(equal (window-class x) "Gajim.py")
		(not (equal (window-role x) "roster"))
		)
	      (and
		(equal (window-class x) "psi")
		(equal (window-res x) "groupchat")
		)
	      (and
		(equal (window-class x) "Vacuum")
		(ends-with-subseq  " - Conference" (window-title x))
		)
	      )
	  (cond
	    ((is-room x "webkit%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "15"))
	    ((is-room x "antiutopia-of-the-day@conference.dev.mccme.ru") 
	     (list "15"))
	    ((is-room x "nixos@conference.jabber.ru")
	     (list "14"))
	    ((is-room x "reprap@conference.dev.mccme.ru")
	     (list "13"))
	    ((is-room x "#dev%dev.mccme.ru@irc.401a0bf1.ignorelist.com") 
	     (list "12"))
	    ((is-room x "dev@conference.dev.mccme.ru") 
	     (list "11"))
	    ((is-room x "real_silence%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "10"))
	    ((is-room x "julia%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "9"))
	    ((is-room x "glendix%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "8"))
	    ((is-room x "irp%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "7"))
	    ((is-room x "scheme%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "6"))
	    ((is-room x "stumpwm%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "5"))
	    ((is-room x "uzbl%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "4"))
	    ((is-room x "monotone%irc.oftc.net@irc.401a0bf1.ignorelist.com")
	     (list "3"))
	    ((is-room x "btrfs%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "2"))
	    ((is-room x "nixos%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "1"))
	    (t nil)
	    ) nil)
	(if
	  (starts-with-subseq  "IRC screen: " (window-title x))
	  (list "im" "ii-interactive" "irc")
	  )
	(if
          (or
            (starts-with-subseq "IRC screen: irc/"               (window-title x))
            (starts-with-subseq "IRC screen: irc.freenode.net/"  (window-title x))
            (starts-with-subseq "IRC screen: irc.oftc.net/"      (window-title x))
            )
	  (list "xkbgr/0")
	  )
	(if
	  (starts-with-subseq  "XMPP screen: " (window-title x))
	  (list "im" "mcabber-interactive" "xmpp")
	  )
	(if
          (and
            (starts-with-subseq  "XMPP screen: raskin@dev.mccme.ru/out/" (window-title x))
            (ends-with-subseq  "@dev.mccme.ru" (window-title x))
            )
          (list "im" "mcabber-interactive" "xkbgr/1")
          )
	(if
	  (or
	    (starts-with-subseq  "IRC summary: " (window-title x))
	    (equalp (window-title x) "ii-irc-summary")
	    )
	  (list "im" "ii-summary" "irc")
	  )
	(if
	  (or
	    (starts-with-subseq  "XMPP summary: " (window-title x))
	    (equalp (window-title x) "mcabber-xmpp-summary")
	    )
	  (list "im" "mcabber-summary" "xmpp")
	  )
	(if (and 
	      (or
		(equal (window-class x) "Thunderbird-bin")
		(equal (window-class x) "Mail")
		(equal (window-class x) "Shredder")
		(equal (window-class x) "Lanikai")
		)
	      )
	  (list "mail" "tb" "base") nil)
	(if (and
	      (or
		(equal (window-class x) "Thunderbird-bin")
		(equal (window-class x) "Mail")
		(equal (window-class x) "Shredder")
		)
	      (equal (window-type x) :Normal)
	      (> (length (window-title x)) 8)
	      (not (equal (subseq (window-title x) 0 8) "Compose:"))
	      (not (equal (subseq (window-title x) 0 6) "Write:"))
	      )
	  (list 
	    ;"3"
	    ) nil)
	(if (and   
	      (equal (window-res x) "Navigator")
	      )
	  (list 
	    ;"4" 
	    "browser" "ff" "www" "base") nil)
	(if (or
	      (equal (window-res x) "Browser")
	      (equal (window-class x) "Minefield")
	      (equal (window-class x) "Firefox")
	      (equal (window-class x) "Iceweasel")
	      (equal (window-class x) "Shiretoko")
	      (equal (window-class x) "Namoroka")
	      (equal (window-class x) "Tumucumaque")
	      (equal (window-class x) "Aurora")
	      (and
		(equal (window-role x) "browser")
		(equal (window-res x) "Navigator")
		)
	      )
	  (list "browser" "ff" "www" "base" "heavy-browser") nil)
	(if (or
	      (equalp (window-class x) "chrome")
	      (equalp (window-class x) "chromium")
	      (equalp (window-class x) "chromium-browser")
	      ) 
	  (list "chrome" "heavy-browser"))
	(if (or
	      (equal (window-class x) "webkit-program-GtkLauncher")
	      (equal (window-class x) "Webkit-program-GtkLauncher")
	      )
	  (list 
	    ;"5" 
	    "browser" "webkit" "base" "wk" "light-browser") nil)
	(if (or
	      (equal (window-class x) ".midori-wrapped")
	      )
	  (list 
	    ;"5" 
	    "midori" "browser" "webkit" "base" "wk") nil)
	(if (or
	      (equal (window-class x) "Carrier")
	      (equal (window-class x) "Pidgin")
	      (equal (window-class x) "Thunderbird-bin")
	      (equal (window-class x) "Mail")
	      (equal (window-class x) "Shredder")
	      (equal (window-res x) "Navigator")
	      (equal (window-class x) "Gajim.py")
	      )
	  (list "web" "base"))
	(if (or
	      (equal (window-res x) "xterm")
	      (equal (window-res x) "urxvt")
	      (equal (window-res x) "rxvt")
              (equal (window-res x) "mlterm")
	      )
	  (list "shell" "term"))
	(if (or
	      (equal (window-title x) "su shell")
	      (equal (window-title x) "su screen")
	      )
	  (list 
	    ;"9" 
	    "root" "admin" "base" "xkbgr/0"))
	(if (or
	      (equal (window-class x) "xmoto")
	      (equalp (window-class x) "warmux")
	      (equalp (window-class x) "tbe")
	      (equalp (window-class x) "glob2")
	      (equalp (window-class x) "widelands")
	      (equalp (window-class x) "liquidwar6")
	      (equal (window-class x) "Sand")
	      (equalp (window-class x) "xpilot-ng-sdl")
	      (equalp (window-class x) "lincity-ng")
	      (equalp (window-class x) "love")
	      (equalp (window-class x) "blobby")
	      (equalp (window-class x) "blobby.bin")
	      )
	  (list "games"))
	(if (and
              (or
                (find (window-class x) 
                      '(
                        "blackbox" "bridges" "cube" "dominosa" "fifteen"
                        "filling" "flip" "galaxies" "guess" "inertia" "keen"
                        "lightup" "loopy" "magnets" "map" "mines" "net" "netslide"
                        "pattern" "pearl" "pegs" "range" "rect" "samegame" "signpost"
                        "singles" "sixteen" "slant" "solo" "tents" "towers" "twiddle"
                        "undead" "unequal" "unruly" "untangle"
                        ) :test 'equalp)
                (starts-with-subseq  "sgt-puzzle-" (window-class x))
                )
	      (equalp
		(window-class x)
		(window-res x))
	      )
	  (list "games" "sgtpuzzles" "sgt-puzzles"))
	(if (or
	      (equal (window-class x) "display")
	      (equal (window-class x) ".wrapped-evince")
	      (equal (window-class x) ".evince-wrapped")
	      (equal (window-class x) "Xpdf")
	      (equal (window-class x) "Zathura")
	      (equal (window-class x) "zathura")
	      (equal (window-class x) ".zathura-wrapped")
	      (equal (window-class x) ".zathura-wrapped_")
	      (equal (window-class x) "MuPDF")
	      (equal (window-class x) "XSane")
	      (equal (window-res x) "gv")
	      (equal (window-class x) "Djview")
	      (equal (window-class x) "Djview4")
	      (equal (window-class x) "GQview")
	      (equal (window-class x) "Geeqie")
	      (equalp (window-class x) "evince")
	      )
	  (list "viewers" "view" "base"))
	(if
	  (or
	    (equal (window-class x) "Kmplayer")
	    (equal (window-class x) "MPlayer")
	    (equalp (window-class x) ".kmplayer-wrapped")
	    )
	  (list "mplayer" "media")
	  )
	(if
	  (or
	    (equal (window-class x) "xine")
	    )
	  (list "xine" "media")
	  )
	(if
	  (or
	    (equalp (window-class x) "vlc")
	    )
	  (list "vlc" "media")
	  )
	(if
	  (or
	    (equal (window-title x) "Media screen")
	    )
	  (list "media" "xkbgr/0")
	  )
	(if
	  (or
	    (equalp (window-title x) "qemu")
	    (equalp (window-class x) "qemu")
	    (starts-with-subseq  "qemu-" (window-class x))
	    )
	  (list "qemu"))
	(if (or
	      (equal (window-res x) "VCLSalFrame")
	      (equal (window-res x) "VCLSalFrame.DocumentWindow")
	      (equal (window-res x) "libreofficedev")
	      (equal (window-res x) "libreoffice")
	      )
	  (list "ooo" "openoffice" "oo.o" "view" "base" "libreoffice"))
	(if (or
	      (equalp (window-res x) "gimp")
	      (equalp (window-class x) ".wrapped-inkscape")
	      (equalp (window-class x) ".inkscape-wrapped")
	      (equalp (window-res x) "xfig")
	      (equalp (window-res x) "drgeo")
	      (equalp (window-res x) "kig")
	      (equalp (window-res x) "openscad")
	      (equalp (window-res x) "rapcad")
	      (equalp (window-class x) "replicatorg-app-base")
	      (starts-with-subseq  "Skeiniso" (window-title x))
	      (starts-with-subseq  "Skeinlayer" (window-title x))
	      )
	  (list "graphics" "editor"))
	(if (or
	      (equalp (window-res x) "xfig")
	      (equalp (window-res x) "drgeo")
	      (equalp (window-res x) "kig")
	      )
	  (list "geom" "geometry"))
	(if (and
	      (or
		(equal (window-res x) "xterm")
		(equal (window-res x) "urxvt")
		(equal (window-res x) "rxvt")
		(equal (window-res x) "mlterm")
		)
	      (> (length (window-title x)) 12)
	      (or
		(equal (subseq (window-title x) 0 12) "ssh session:")
		(starts-with-subseq  "ssh-do-there: " (window-title x))
		)
	      )
	  (list "ssh" "base" "xkbgr/0"))
	(if (and
	      (or
		(equal (window-res x) "xterm")
		(equal (window-res x) "urxvt")
		(equal (window-res x) "rxvt")
		(equal (window-res x) "mlterm")
		)
	      (or
		(equal (window-title x) "web-streams")
		)
	      )
	  (list "web-streams" "viewers" "view" "xkbgr/0"))
	(if (and
	      (or
		(equal (window-res x) "xterm")
		(equal (window-res x) "urxvt")
		(equal (window-res x) "rxvt")
		(equal (window-res x) "mlterm")
		)
	      (or
		(equal (window-title x) "emails")
		)
	      )
	  (list "mail" "email"))
	(if (or
	      (equal (window-title x) "Gateway6 monitoring")
	      (equal (window-title x) "Local IRC ghost")
	      )
	  (list "monitor"))
	(if (or
	      (equal (window-title x) "zsh")
	      (equal (window-title x) "sh")
	      (equal (window-title x) "su shell")
	      (equal (window-title x) "bash")
	      )
	  (list "open-shell"))
	(if (or
	      (equalp (window-res x) "vncviewer")
	      (equalp (window-class x) "Vncviewer")
	      (equalp (window-class x) "Gvncviewer")
	      )
	  (list "vnc" "ssh"))
	(if (or
	      (equal (window-class x) "Linuxdcpp")
	      (ends-with-subseq  "(BitTornado)" (window-title x))
	      )
	  (list "p2p"))
	(if (or
	      (equal (window-class x) "Linuxdcpp")
	      )
	  (list "dc" "150"))
	(if (or
	      (equal (window-class x) "bittornado")
	      (ends-with-subseq  "(BitTornado)" (window-title x))
	      )
	  (list "bt" "160"))
	(if (or
	      (equal (window-title x) "input-history (~/.local/share/uzbl) - VIM")
	      (equal (window-title x) "input-history + (~/.local/share/uzbl) - VIM")
	      (ends-with-subseq  ".local/share/uzbl/forms) - VIM" (window-title x))
	      (equal (window-class x) ".uzbl-wrapped")
	      (equal (window-class x) ".uzbl-core-wrapped")
	      (equal (window-class x) ".wrapped-uzbl")
	      )
	  (list "uzbl" "light-browser"))
	(if
	  (or
	    (equalp (window-class x) "slimerjs")
	    )
	  (list "light-browser" "slimer" "slimerjs"))
	(if (or
	      (equal (window-title x) "input-history (~/.local/share/uzbl) - VIM")
	      (ends-with-subseq  ".local/share/uzbl/forms) - VIM" (window-title x))
	      (equal (window-class x) ".uzbl-wrapped")
	      (equal (window-class x) ".uzbl-core-wrapped")
	      (equal (window-class x) "uzbl")
	      (equal (window-class x) ".wrapped-uzbl")
	      (equal (window-class x) "Links")
	      (equal (window-class x) ".midori-wrapped")
	      (equal (window-class x) ".wrapped-midori")
	      (equalp (window-class x) "slimerjs")
              (equal "gvim-web" (window-role x))
              (starts-with-subseq "gvim-web-" (window-role x))
	      )
	  (list "light-browser" "browser"))
	(if (and
	      (equal (window-class x) "Lazarus")
	      (starts-with-subseq  "Lazarus IDE" (window-title x)))
	  (list "lazarus-ide-window"))
	(if (and
	      (equal (window-class x) "Lazarus")
	      (starts-with-subseq  "Messages" (window-title x)))
	  (list "lazarus-message-window"))
	(if (and
	      (equal (window-class x) "Lazarus")
	      (starts-with-subseq  "Object Inspector" (window-title x)))
	  (list "lazarus-inspector-window"))
	(if (and
	      (equal (window-class x) "Dia")
	      (equal (window-role x) "toolbox_window"))
	  (list "dia-toolbar"))
	(if (and
	      (equal (window-class x) "Gimp")
	      (equal (window-role x) "gimp-toolbox"))
	  (list "gimp-toolbar"))
	(if (or
	      (equalp (window-title x) "Limp")
	      )
	  (list "Limp")
	  )
	(if (or
	      (starts-with-subseq  "SQuirreL SQL" (window-title x))
	      )
	  (list "SquirrelSQL" "editor" "sql")
	  )
	(if
	  (equalp (window-class x) "org-hypergraphdb-viewer-hgvdesktop")
	  (list "editor" "HGDB" "HGDBViewer" "GraphDB")
	  )
	(if
	  (equalp (window-class x) "freemind-main-FreeMindStarter")
	  (list "editor" "freemind" "mindmap")
	  )
	(if
	  (equalp (window-class x) "tufts-vue-VUE")
	  (list "editor" "vue" "mindmap")
	  )
	(if
	  (equal (window-class x) "Gvim")
	  (list "editor" "gvim" "vim" "xkbgr/0")
	  )
	(if
	  (equal (window-title x) "XWatchSystem")
	  (list "xwatchsystem" "999"))
	(if
	  (equalp (window-class x) "trayer")
	  (list "trayer" "tray" "998")
	  )
	(if
	  (equal (window-title x) "Breaking News")
	  (list "breaking-news" "997"))
	(if
	  (or
	    (equalp (window-class x) "textadeptjit")
	    (equalp (window-class x) "textadept")
	    )
	  (list "editor" "textadept")
	  )
	))))

(defun 
  defzone (x tags)
  (cond
    ((find "no-auto-tags" tags :test 'equalp) nil)
    ((and
       (find "im" tags :test 'equalp)
       (or
	 (cl-ppcre:scan "@conference[.]" (window-role x))
	 )
       ) "cat/x-im-mucs")
    ((and
       (find "II-INTERACTIVE" tags :test 'equalp)
       (or
	 (cl-ppcre:scan "/#" (window-title x))
	 )
       ) "cat/w-irc-channels")
    ((subsetp '("im" "0") tags :test 'equalp) "cat/x-im-roster")
    ((find "im" tags :test 'equalp) "cat/e-im")
    ((find "email" tags :test 'equalp) "cat/em-email")
    ((find "games" tags :test 'equalp) "cat/o-games")
    ((find "media" tags :test 'equalp) "cat/i-media")
    ((find-if (lambda (x) (cl-ppcre:scan "^9[7-9][0-9]$" x)) tags) "cat/z-bars")
    ((intersection tags '("sh" "root") :test 'equalp) "cat/h-shells")
    ((intersection tags '("ssh") :test 'equalp) "cat/u-ssh")
    ((intersection tags '("browser") :test 'equalp) "cat/f-browsers")
    ((intersection tags '("editor") :test 'equalp) "cat/c-editors")
    )
  )
