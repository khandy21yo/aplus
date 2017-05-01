	!
	! File Layout for: SMG.SMG_SCROLL
	!
	! \Scroller
	!

	RECORD SMG_SCROLL_CDD
		! Element =
		!   Description = This is the window Virtual Display ID
		LONG WINDOW
		! Element =
		!   Description = The top row of the scrolling region with
		LONG SCROLL_TOP
		! Element =
		!   Description = The bottom row of the scrolling region w
		LONG SCROLL_BOT
		! Element =
		!   Description = The top element number of the array
		LONG TOP_ARRAY
		! Element =
		!   Description = The number of lines in the array
		LONG BOT_ARRAY
		! Element =
		!   Description = The array element number of the line at
		LONG TOP_LINE
		! Element =
		!   Description = The array element number of the line to
		LONG BEG_ELEMENT
		! Element =
		!   Description = The array element number of the line to
		LONG END_ELEMENT
		! Element =
		!   Description = The array element number of the current
		LONG CUR_LINE
		! Element =
		!   Description = The number of the current window COLUMN
		LONG CUR_W_COL
		! Element =
		!   Description = The number of the current window ROW - R
		LONG CUR_W_ROW
		! Element =
		!   Description = The line to put on the screen in the fin
		LONG FIND_LINE
		! Element =
		!   Description = Flag for various options
		LONG SMG_FLAG
		! Element =
		!   Description = The characters to use to point to the cu
		STRING PROMPT = 4
		! Element =
		!   Description = The list of columns that need a column l
		STRING DRAW_COLS = 160
		! Element =
		!   Description = The number of columns in the window
		LONG NUM_COLM
		! Element =
		!   Description = Video set.
		LONG VIDEO_SET
		! Element =
		!   Description = Video compare.
		LONG VIDEO_COMP
		! Element =
		!   Description = Character set type
		LONG CHARSET
		! Element =
		!   Description = Virtual selected line number for use wit
		LONG V_SELECT_LINE
	END RECORD
