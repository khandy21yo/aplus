1	%TITLE "Check Words in File Against Dictionary"
	%SBTTL "HELP_SPELL"
	%IDENT "V3.6a Calico"

	SUB HELP_SPELL(LIB_NAME$, KEY_NAME$)

	!
	!	COPYRIGHT (C) 1987, 1988 BY
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
	!	Checks a help message against the dictionary.
	!
	! Parameters:
	!
	!	LIB_NAME$
	!		The passed name of the library to pull the text from.
	!
	!	KEY_NAME$
	!		The passed name of the key to use to select the
	!		right text.
	!
	!
	!	Returned value
	!		Checks the help message against the dictionary
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:HELP_SPELL
	!	$ LIB FUNC_LIB:CMCFUN/REP HELP_SPELL
	!	$ DELETE HELP_SPELL.OBJ;*
	!
	! Author:
	!
	!	06/01/88 - Kevin Handy
	!
	! Modification history:
	!
	!	07/05/89 - Kevin Handy
	!		Removed map statement that declared TEXT$
	!		in (LBR_JUNKJUNK).
	!
	!	05/01/90 - Kevin Handy
	!		Complete re-write to use the SPELL program
	!		provided with TeX.  Assumes that the SPELL
	!		command is defined in startup.
	!
	!	05/10/90 - Kevin Handy
	!		Changed name from TEMPxxx.SPELL to TEMPxxx.RNO
	!		so that spelling checker will know it's working
	!		with runoff style of text.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/09/93 - Kevin Handy
	!		Changed "WHILE 1" to "WHILE 1%".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Change READ_SYSPID to READ_SYSPN, to match what
	!		all the other programs do.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/23/99 - Kevin Handy
	!		Use WHEN ERROR
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION LIBR_EXTRACT
	EXTERNAL LONG FUNCTION LIBR_3INSERT

	%PAGE

50	!*******************************************************************
	! Spell check the file
	!*******************************************************************

	ID$ = READ_SYSPN
	FILE.NAME$ = "TEMP" + ID$ + ".RNO"

	ST% = LIBR_EXTRACT(LIB_NAME$, FILE.NAME$, KEY_NAME$)

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Error in extract " + NUM1$(ST%), 0%)
		STOP
	END IF

	CALL SUBR_3SPAWN(SCOPE, "SPELL " + FILE.NAME$)

	ST% = LIBR_3INSERT(LIB_NAME$, FILE.NAME$, KEY_NAME$)

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Error in insert " + NUM1$(ST%), 0%)
		STOP
	END IF

100	!
	! Kill all versions until it won't kill no more
	!
 !	WHEN ERROR IN
 !		KILL FILE.NAME$
 !	USE
 !		CONTINUE 32767
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(FILE.NAME$ + ";*")

	GOTO 100

32767	END SUB
