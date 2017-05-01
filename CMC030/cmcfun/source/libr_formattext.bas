1	%TITLE "Format Text to Specified Column Width"
	%SBTTL "LIBR_FORMATTEXT"
	%IDENT "V3.6a Calico"

	SUB LIBR_FORMATTEXT(INTEXT$, FORMWIDTH%, LINECOUNT%, OUTTEXT$())

	!
	!	COPYRIGHT (C) 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	!	This function is used to format text into columns
	!	less than or equal to the specified width.
	!	This function is meant to be used mostly with the
	!	text read from a text library.
	!
	! Index:
	!
	! Parameter:
	!
	!	INTEXT$
	!		Passed text string to be formatted.  Can contain
	!		CHR$(13) in the text to force a line break
	!		at a given point.
	!
	!	FORMWIDTH%
	!		Desired maximum width for the output lines.
	!
	!	LINECOUNT%
	!		Number of lines returned.
	!
	!	OUTTEXT$
	!		Returned text given back to caller.
	!
	! Example:
	!
	!	CALL LIBR_FORMATTEXT(INPUT_TEXT$, 40%, TLINE%, ARY$())
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_FORMATTEXT/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP LIBR_FORMATTEXT
	!	$ DELETE LIBR_FORMATTEXT.OBJ;*
	!
	! Author:
	!
	!	04/21/88 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Copy over whatever is necessary so original strings does not
	! get distroyed, and prime other variables as necessary.
	!
	JUNK$ = INTEXT$
	LINECOUNT% = 0%

	JUNK$ = XLATE(JUNK$, STRING$(13%, 0%) + " " + STRING$(18%, 0%) + &
		' !"#$%&' + "'()*+,-./0123456789:;<=>?@" + &
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`" + &
		"abcdefghijklmnopqrstuvwxyz{|}~" + '127'C)

	WHILE	JUNK$ <> ""
		LINECOUNT% = LINECOUNT% + 1%
		JUNK$ = EDIT$(JUNK$, 24%)
		FIND% = FORMWIDTH% - 1%

		UNTIL FIND% < 1% OR MID(JUNK$, FIND%, 1%) <> " "
			FIND% = FIND% - 1%
		NEXT

		UNTIL FIND% < 1% OR MID(JUNK$, FIND%, 1%) = " "
			FIND% = FIND% - 1%
		NEXT

		IF FIND% < 1% OR LEN(JUNK$) < FORMWIDTH%
		THEN
			OUTTEXT$(LINECOUNT%) = LEFT(JUNK$, FORMWIDTH%)
			JUNK$ = RIGHT(JUNK$, FORMWIDTH% + 1%)
		ELSE
			OUTTEXT$(LINECOUNT%) = LEFT(JUNK$, FIND% - 1%)
			JUNK$ = RIGHT(JUNK$, FIND% + 1%)
		END IF

	NEXT

	END SUB
