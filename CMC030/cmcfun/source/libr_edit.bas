1	%TITLE "Edit/Create File from Text Library"
	%SBTTL "LIBR_EDIT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG LIBR_EDIT(LIB_NAME$, KEY_NAME$)

	!
	!	COPYRIGHT (C) 1987 BY
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
	!	This function will edit text stored in a text
	!	library.  It places user into the EDT editor,
	!	and allows the text to be changed.  It will
	!	change text for all keys connected to the same
	!	text.
	!
	! Parameters:
	!
	!	LIB_NAME$
	!		The passed name of the library containing
	!		the text to be edited.
	!
	!	KEY_NAME$
	!		The passed key that points to the text in the
	!	library.
	!
	!	Returns a status code.
	!
	! Example:
	!
	!	ST% = LIBR_EDIT("HELP_GL", "ADDRESS")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_EDIT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP LIBR_EDIT
	!	$ DELETE LIBR_EDIT.OBJ;*
	!
	! Author:
	!
	!	07/06/87 - Kevin Handy
	!
	! Modification history:
	!
	!	03/14/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/25/95 - Kevin Handy
	!		Modified to use LIB$DELETE_FILE instead of kill.
	!
	!	11/15/95 - Kevin Handy
	!		Finish removing error trap, which is no longer used.
	!		Move delete from subroutine into main program,
	!		since it is only one line and only used once.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$, SS$ routines
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions
	!
	EXTERNAL STRING  FUNCTION READ_SYSJOB
	EXTERNAL LONG    FUNCTION EDT$EDIT

	EXTERNAL LONG FUNCTION LIBR_EXTRACT
	EXTERNAL LONG FUNCTION LIBR_3INSERT

	%PAGE

	!
	! Think up temp file name
	!
	TEMP.FILE$ = "TEMP" + READ_SYSJOB + ".TMP"

	!
	! Extract help from library
	!
	ST% = LIBR_EXTRACT(LIB_NAME$, TEMP.FILE$, KEY_NAME$)

3200	!
	! Call the editor EDT
	!
	CALL HELP_34MESSAGE(SCOPE, "editing...", &
		"I", "LIBR_EDIT", KEY_NAME$, "EDIT")

	ST% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%) ! Cursor On

	ST% = EDT$EDIT(TEMP.FILE$,,,,,,,)

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	SELECT ST%

	!
	! Normal exit from EDT
	!
	CASE SS$_NORMAL
		!
		! Insert it in the library
		!
		ST% = LIBR_3INSERT(LIB_NAME$, TEMP.FILE$, KEY_NAME$)

		IF (ST% AND 1%) = 0%
		THEN
			TEMP$ = "Unable to add text to file " + NUM1$(ST%)
			CALL ENTR_3MESSAGE(SCOPE, TEMP$, 0%)
		END IF

	!
	! Error using EDT
	!
	CASE ELSE
		CALL LIB$SIGNAL(SMG_STATUS% BY VALUE)

	END SELECT

	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%) ! Off

	!*******************************************************************
	! Clean exit from function
	!*******************************************************************

	!
	! Kill temp files
	!
	V% = LIB$DELETE_FILE(TEMP.FILE$ + ";*")

	END FUNCTION
