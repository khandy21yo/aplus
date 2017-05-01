1	%TITLE "Insert a Text String into a Library"
	%SBTTL "LIBR_INSERTTEXT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG LIBR_INSERTTEXT(LIB_NAME$, TEXT$, KEY_NAME$)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!	This function will insert text into a library,
	!	and append a key to that text.
	!
	! Parameters:
	!
	!	LIB_NAME$
	!		Passed name of library to insert text into.
	!
	!	TEXT$
	!		Passed text to insert into the library.
	!
	!	KEY_NAME$
	!		Passed name of key to append to text.
	!
	!	Returns a status code.
	!
	! Example:
	!
	!	ST% = LIBR_INSERT("HELP_FUN", "Happyville Party Club", "EVENT")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_INSERTTEXT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP LIBR_INSERTTEXT
	!	$ DELETE LIBR_INSERTTEXT.OBJ;*
	!
	! Author:
	!
	!	07/28/87 - Kevin Handy
	!
	! Modification history:
	!
	!	08/06/87 - Kevin Handy
	!		Modified to break long lines into shorter ones so that
	!		the librarian won't get upset.  Now breaks long lines
	!		into short ones and flags the end of the lines with
	!		"+" for continuation, and "-" for end of a line.
	!
	!	07/19/88 - Kevin Handy
	!		Removed defaulting to REF:
	!
	!	11/30/88 - Kevin Handy
	!		Modified to close LBR$INIT if open fails.
	!
	!	12/28/88 - Kevin Handy
	!		Removed error trapping, which had no meaning
	!		in this function since there is no errors that
	!		will jump to it.
	!
	!	12/29/88 - Kevin Handy
	!		Set up to close library if gets error opening it.
	!
	!	01/03/89 - Kevin Handy
	!		Modified to return LIBR_INSRTTEXT instead of
	!		LIBR_EDIT.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Added definition of SCOPE to avoid crashes.
	!
	!	04/08/99 - Kevin Handy
	!		Change trying to return through LIBR_INSERTTEXT
	!		instead of LIBR_EXTRACT.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	!
	! External functions
	!
	EXTERNAL LONG LIBR_INSERT_A

	DECLARE RFA TEST.RFA, NULL.RFA

	MAP (LBR_JUNKJUNK) C_ARRAY%(19%)
	MAP (LBR_JUNKJUNK) C_ARRAY$ = 88%

	MAP (LBR_JUNKJUNK) K.NAME%, K.NAME$(64%) = 39%

	%PAGE

	LIBR_INSERTTEXT = 1%

100	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(L.INDEX%, LBR$C_UPDATE)

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		LIBR_INSERTTEXT = ST%
		EXIT FUNCTION
	END IF

200	!
	! Open the library function
	!
	ST% = LBR$OPEN(L.INDEX%, LIB_NAME$, , ".TLB", ,C_ARRAY$)

	IF (ST% = 98962%)
	THEN
		ST% = LBR$CLOSE(L.INDEX%)
		GOSUB CreateLibrary
		GOTO 100
	END IF

	IF (ST% AND 1%) = 0%
	THEN
		ST% = LBR$CLOSE(L.INDEX%)
		CALL ENTR_3MESSAGE(SCOPE, "Unable to open library " + NUM1$(ST%) + " " + &
			C_ARRAY$, 4%)
		LIBR_INSERTTEXT = ST%
		GOTO 900
	END IF

300	!
	! Delete key if it already exists
	!
	ST% = LBR$LOOKUP_KEY(L.INDEX%, KEY_NAME$, TEST.RFA)

	K.NAME% = 0%

	IF (ST% AND 1%)
	THEN
		!
		! Delete the primary key
		!
		ST% = LBR$DELETE_KEY(L.INDEX%, KEY_NAME$) &

		!
		! Delete all records associated with this key
		!
		ST% = LBR$SEARCH(L.INDEX%, 1%, TEST.RFA, LIBR_INSERT_A)

		ST% = LBR$DELETE_KEY(L.INDEX%, K.NAME$(I%)) &
			FOR I% = 1% TO K.NAME%

		!
		! Delete the data
		!
		ST% = LBR$DELETE_DATA(L.INDEX%, TEST.RFA)
	END IF

	!
	! Load in text information
	!
	TEST.RFA = NULL.RFA

	LINE.FLAG% = 0%

	JUNK$ = TRM$(TEXT$)

310	WHILE JUNK$ <> ""
		I% = INSTR(1%, JUNK$, '13'C)
		I% = LEN(JUNK$) + 1% IF I% = 0%

		LINE.FLAG% = -1%
		JUNK1$ = LEFT(JUNK$, I% - 1%)
		JUNK$ = RIGHT(JUNK$, I% + 1%)
 Loop1:
		!
		! Break up the line if it is more than 80 characters wide.
		!
		IF LEN(JUNK1$) < 80%
		THEN
			ST% = LBR$PUT_RECORD(L.INDEX%, JUNK1$ + "-", TEST.RFA)
		ELSE
			ST% = LBR$PUT_RECORD(L.INDEX%, &
				LEFT(JUNK1$, 80%) + "+", TEST.RFA)
			JUNK1$ = RIGHT(JUNK1$, 81%)
			GOTO Loop1
		END IF
	NEXT

350	IF LINE.FLAG% = 0%
	THEN
		LIBR_INSERTTEXT = 2%
		GOTO 900
	END IF

	!
	! Mark end of text
	!
	ST% = LBR$PUT_END(L.INDEX%)

	!
	! Insert main key pointing to text
	!
	LIBR_INSERTTEXT = LBR$INSERT_KEY(L.INDEX%, KEY_NAME$, TEST.RFA)

	!
	! Insert additional keys pointing to text
	!
	ST% = LBR$INSERT_KEY(L.INDEX%, K.NAME$(I%), TEST.RFA) &
			FOR I% = 1% TO K.NAME%

900	!
	! Close all files
	!
	JUNK% = LBR$CLOSE(L.INDEX%)

	EXIT FUNCTION

	%PAGE

1000	!*******************************************************************
	! Create a library file
	!*******************************************************************
 CreateLibrary:

	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LC.INDEX%, LBR$C_CREATE, LBR$C_TYP_TXT)

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		LIBR_INSERTTEXT = ST%
		EXIT FUNCTION
	END IF

	!
	! Open the library file
	!
	C_ARRAY%( 0%) = LBR$C_TYP_TXT		! Library type
	C_ARRAY%( 1%) = 39%			! Key length
	C_ARRAY%( 2%) = 11%			! Initial allocation
	C_ARRAY%( 3%) = 1%			! Number of keys
	C_ARRAY%( 4%) = 0%			! Additional chars in header
	C_ARRAY%( 5%) = 200%			! Preallocated indexes
	C_ARRAY%( 6%) = 0%			! History records
	C_ARRAY%( 7%) = 3%			! Format of library
	C_ARRAY%( 8%) = 0%			! Index casing

	ST% = LBR$OPEN(LC.INDEX%, LIB_NAME$, C_ARRAY$ BY REF, DEF_DIR$ + ".TLB")

	!
	! Close it and return
	!
	ST% = LBR$CLOSE(LC.INDEX%)
	RETURN

	END FUNCTION
