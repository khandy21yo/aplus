1	%TITLE "Window Maintenance"
	%SBTTL "MAIN_PAINTSCREEN"
	%IDENT "V3.6a Calico"

	SUB MAIN_PAINTSCREEN(CDD_WINDOW_CDD SMG_WINDOW, &
		STRING MOPTION, LONG MLOOP)

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	This function is used by the maintainence functions to
	!	display the main screen, and possible one additional
	!	page.
	!
	! Parameters:
	!
	!	SMG_WINDOW
	!		The passed structure describing the current window.
	!
	!	MOPTION
	!		The function to be done by the function.
	!	.table
	!		" " - Repaint main screen, current page.
	!		"P" - Previous page
	!		"N" - Next page
	!		"C" - Choose page according to the field
	!			pointed to by MLOOP
	!		"S" - Select a specific page (MLOOP).
	!	.endtable
	!
	!	MLOOP
	!		Either a field number, or a page number
	!		depending on which function was selected.
	!
	!	Structure MAIN_WINDOW fields used:
	!
	!	IDENT
	!		Identifying number for this window assigned by
	!		the program.
	!
	!	NHELP
	!		Name to use for help in the PROGRAM field.
	!
	!	NITEMS
	!		Number of fields in the window.
	!
	!	WNUMBER
	!		The window number of the window for this screen
	!		as returned by SMG$CREATE_VIRTUAL_DISPLAY.
	!
	!	LWINDOW
	!		The number of the page window, it it exist.
	!
	!	LWIDTH
	!		The width of tha page window.
	!
	!	LHEIGHT
	!		The height of the page window.
	!
	!	LVPOS
	!		The vertical position of the page window.
	!
	!	LHPOS
	!		The horozontal position of the page window.
	!
	!	LLAST
	!		The last page used by the program.
	!
	!	LPAGE(0..7)
	!		The last fields on each page.
	!
	!	LCURR
	!		The current page on the screen.
	!
	!
	!	This function displays the main screen and one additional
	!	screen in maintenance programs.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_PAINTSCREEN/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_PAINTSCREEN
	!	$ DELETE MAIN_PAINTSCREEN.OBJ;*
	!
	! Author:
	!
	!	08/21/87 - Kevin Handy
	!
	! Modification history:
	!
	!	03/08/88 - Kevin Handy
	!		Modified to use PASTEBOARD_UPDATE mode.
	!
	!	03/13/91 - Frank F. Starman
	!		Set conditions for HFLAG.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/28/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change "<tab>" to "<space>" in ltitle, so title won't
	!		confuse line drawing.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! External declarations
	!
	EXTERNAL INTEGER FUNCTION MAINT_GROUP

	%PAGE

	TEMP.EXIT% = SCOPE::SCOPE_EXIT

	SELECT MOPTION

	!
	! Repaint main screen and current screen
	!
	CASE " "
		SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, &
			SMG_WINDOW::LCURR, 0%, "") IF MLOOP = 1%
		SMG_STATUS% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)
		GOSUB PaintMain
		GOSUB PaintCurrent
		SMG_STATUS% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	!
	! Previous screen
	!
	CASE "P"
		TEMP% = SMG_WINDOW::LCURR - 1%
		TEMP% = SMG_WINDOW::LLAST IF TEMP% < 0%

		GOSUB ChangePage

	!
	! Next screen
	!
	CASE "N"
		TEMP% = SMG_WINDOW::LCURR + 1%
		TEMP% = 0% IF TEMP% > SMG_WINDOW::LLAST

		GOSUB ChangePage

	!
	! Select a specific window
	!
	CASE "S"
		TEMP% = MLOOP

		GOSUB ChangePage

	!
	! Choose window that contains a specified field
	!
	CASE "C"
		TEMP% = 0%
		TEMP% = TEMP% + 1% &
			UNTIL (MLOOP <= SMG_WINDOW::LPAGE(TEMP%)) OR &
			(TEMP% = SMG_WINDOW::LLAST)

		GOSUB ChangePage

	END SELECT

	SCOPE::SCOPE_EXIT = TEMP.EXIT%

	EXIT SUB

	%PAGE

 ChangePage:
	!*******************************************************************
	! Change the current page to TEMP%
	!*******************************************************************

	!
	! Don't do anything if we are changing to the same page
	!
	RETURN IF TEMP% = SMG_WINDOW::LCURR

	!
	! If we are switching to zero, kill the page screen
	!
	IF TEMP% = 0%
	THEN
		!
		! Change current page number
		!
		SMG_WINDOW::LCURR = 0%

		!
		! Delete the page
		!
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::LWINDOW)

		!
		! Done
		!
		RETURN
	END IF

	!
	! Hold on until all is done
	!
	SMG_STATUS% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	!
	! If we are switching from 0, create a page
	!
	IF (SMG_WINDOW::LCURR = 0%)
	THEN
		!
		! Create the window
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
		( &
			SMG_WINDOW::LHEIGHT, &
			SMG_WINDOW::LWIDTH, &
			SMG_WINDOW::LWINDOW, &
			SMG$M_BORDER &
		)

		!
		! Paste it on
		!
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
		( &
			SMG_WINDOW::LWINDOW, &
			SCOPE::SMG_PBID, &
			SMG_WINDOW::LVPOS, &
			SMG_WINDOW::LHPOS &
		)
	END IF

	!
	! Set the current window number
	!
	SMG_WINDOW::LCURR = TEMP%

	!
	! Label screen
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_WINDOW::LWINDOW, &
		" Page " + NUM1$(TEMP% + 1%) + " " + &
		TRM$(SMG_WINDOW::LTITLE(TEMP%)) + " ") &
		IF (TEMP% <> 0%)

	!
	! Display the background
	!
	SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, TEMP%, 0%, "")

	!
	! Display the data
	!
	GOSUB PaintCurrent

	!
	! Display results
	!
	SMG_STATUS% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	RETURN

	%PAGE

 PaintMain:
	!*******************************************************************
	! Paint all information onto the screen for the current record.
	!*******************************************************************

	RETURN IF MLOOP = 1% AND SMG_WINDOW::LCURR
	!
	! Go through all fields
	!
	FOR I% = 1% TO SMG_WINDOW::LPAGE(0%)
		IF I%<=64%
		THEN
			V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "") &
					IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
		ELSE
			V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "")
		END IF
	NEXT I%

	!
	! Handle anything extra
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_DISPLAY, 0%, 0%, "") &

	RETURN

	%PAGE

 PaintCurrent:
	!*******************************************************************
	! Paint all information onto the screen for the current record.
	!*******************************************************************

	!
	! Make typing easier
	!
	TEMP1% = SMG_WINDOW::LCURR

	!
	! Exit if current screen
	!
	RETURN IF TEMP1% = 0%

	!
	! Go through all fields
	!
	FOR I% = SMG_WINDOW::LPAGE(TEMP1% - 1%) + 1% TO SMG_WINDOW::LPAGE(TEMP1%)
		IF I% <= 64%
		THEN
			V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "") &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
		ELSE
			V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "")
		END IF
	NEXT I%

	!
	! Handle anything extra
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_DISPLAY, 0%, 0%, "") &

	RETURN

32767	END SUB
