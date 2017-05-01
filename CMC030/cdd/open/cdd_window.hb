	!
	! File Layout for: CDD.CDD_WINDOW on 11-Mar-99
	!
	! Window Data
	!

	RECORD CDD_WINDOW_CDD
		! Element =
		!   Description = Window identification number
		LONG IDENT
		! Element =
		!   Description = Title for top of window
		STRING DESCR = 40
		! Element =
		!   Description = Name to be used by help function
		STRING NHELP = 40
		! Element =
		!   Description = Channel that file is open on
		LONG CHAN
		! Element =
		!   Description = Width of the window
		LONG HSIZE
		! Element =
		!   Description = height of the window
		LONG VSIZE
		! Element =
		!   Description = Horozontal position of the window
		LONG HPOS
		! Element =
		!   Description = Vertical position of the window
		LONG VPOS
		! Element =
		!   Description = Number of fields in window
		LONG NITEMS
		! Element =
		!   Description = Number of keys
		WORD NKEYS
		! Element =
		!   Description = Key names
		STRING KNAME(7) = 16
		! Element =
		!   Description = Fields in keys (8 Keys, 8 fields per key
		WORD KFIELD(7, 8)
		! Element =
		!   Description = View width
		LONG HVIEW
		! Element =
		!   Description = View height
		LONG VVIEW
		! Element =
		!   Description = For those routines that need to store it
		LONG WNUMBER
		! Element =
		!   Description = For a relative file, the record number t
		LONG RELREC
		! Element =
		!   Description = For a journal, number of physical lines
		LONG LINREC
		! Element =
		!   Description = For a journal, top line available on scr
		LONG TOPLIN
		! Element =
		!   Description = For a journal, bottom line on screen ava
		LONG BOTLIN
		! Element =
		!   Description = For a journal, Currently selected record
		LONG CURREC
		! Element =
		!   Description = For a journal, Line on screen for curren
		LONG CURLIN
		! Element =
		!   Description = For a journal, record on the top of the
		LONG TOPREC
		! Element =
		!   Description = For a journal, the maximum number of rec
		LONG MAXREC
		! Element =
		!   Description = For a journal, the total number of recor
		LONG TOTREC
		! Element =
		!   Description = Place to store the window number for a p
		LONG LWINDOW
		! Element =
		!   Description = Width of the page window
		LONG LWIDTH
		! Element =
		!   Description = Height of the page window
		LONG LHEIGHT
		! Element =
		!   Description = Vertical position of the page window
		LONG LVPOS
		! Element =
		!   Description = Horozontal position of the page window
		LONG LHPOS
		! Element =
		!   Description = Last page to be usable
		LONG LLAST
		! Element =
		!   Description = Last field on each page
		LONG LPAGE(7)
		! Element =
		!   Description = Current page selected
		LONG LCURR
		! Element =
		!   Description = View horozontal position
		LONG VHPOS
		! Element =
		!   Description = View vertical position
		LONG VVPOS
		! Element =
		!   Description = Fill section
		STRING MENU = 28
		! Element =
		!   Description = Hard/soft flags for items
		WORD HFLAG(64)
		! Element =
		!   Description = Flag word
		LONG FLAGS
		! Element =
		!   Description = Titles for screen pages
		STRING LTITLE(7) = 16
	END RECORD
