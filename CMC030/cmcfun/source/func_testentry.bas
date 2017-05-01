1	%TITLE "Data Entry Testing Function"
	%SBTTL "FUNC_TESTENTRY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG FUNC_TESTENTRY(CDD_WINDOW_CDD SMG_WINDOW, &
		STRING TOTEST, RETURNED, SYSTEM, LONG FLD, STRING VARNAM, &
		DESCRIP, LONG WINDOWNUM)

	!
	! COPYRIGHT (C) 1988 BY
	! Computer Management Center, Inc.
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
	!		This function will test data given to it,
	!	in order to see if the data is defined.
	!
	! Parameters:
	!
	!	TOTEST
	!		The data to be tested.
	!
	!	RETURNED
	!		The description of the data.  If TO.TEST is
	!		defined, RETURNED will be its definition.
	!		If not, RETURNED will be question marks (??).
	!
	!	SYSTEM
	!		The system in which the data would be defined.
	!
	!	FLD
	!		The field from which calls this function.
	!
	!	VARNAM
	!		The name of the variable containing the data
	!		to be tested.
	!
	!	DESCRIP
	!		The description of the type of data that the
	!		variable stores.
	!
	!	WINDOWNUM
	!		The number used to identify the MAIN program
	!		maintaining the descriptions which might or
	!		might not define the data in question.
	!
	!	Returned value
	!		The value the MAIN program will be set to
	!		during the TESTENTRY option, where this
	!		function will be used.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_TESTENTRY/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP FUNC_TESTENTRY
	!	$ DELETE FUNC_TESTENTRY.OBJ;*
	!
	! Author:
	!
	!	07/06/88 - Aaron Redd
	!
	! Modification history:
	!
	!	09/18/91 - Frank F. Starman
	!		Return back description 'Overlay Mask' if
	!		there is an ? in the tested variable.
	!
	!	04/02/92 - Frank F. Starman
	!		Return back CMC$_UNDEFINED.
	!		Change variable PROGRM to FLD.
	!		Use READ_35SET as a long function.
	!
	!	04/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	DECLARE UTL_SET_CDD	UTL_SET_READ

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION MAIN_WINDOW
	EXTERNAL LONG		FUNCTION READ_35SET

	!
	! Declare some variables
	!
	DECLARE LONG RETURNED.VALUE

	!
	! Assume good value
	!
	RETURNED.VALUE = 0%

	!
	! Is the input defined?
	!
	IF MAIN_WINDOW(WINDOWNUM, "Q0" + TOTEST) <> 1%
	THEN
		IF INSTR(1%, TOTEST, "?")
		THEN
			RETURNED = DESCRIP + " Overlay Mask"
			GOTO EndFunc
		ELSE
			RETURNED = STRING$(LEN(RETURNED), A"?"B)
		END IF

		!
		! See if they are allowing undefined inputs
		!
		IF READ_35SET(SCOPE::PRG_PROGRAM, NUM1$(FLD), UTL_SET_READ) <> &
			CMC$_NORMAL
		THEN
			IF READ_35SET(SYSTEM + "_ALLOW", VARNAM, &
				UTL_SET_READ) <> CMC$_NORMAL
			THEN
				UTL_SET_READ::ALLOWUND = "N"
			END IF
		END IF

		IF UTL_SET_READ::ALLOWUND = "N"
		THEN
			!
			! Don't let them get past is we don't allow
			! them to enter undefined values.
			!
			RETURNED.VALUE = 1%
			CALL ENTR_3MESSAGE(SCOPE,  DESCRIP + " is undefined ", 1%)
		ELSE
			!
			! Verify that they really want to enter an
			! undefined value.
			!
			V$ = ENTR_3YESNO(SCOPE,  SMG_WINDOW::WNUMBER, "", &
				DESCRIP + " is undefined, confirm entry" + &
				" then press <DO> ", &
				"N", 0%, "", "")
			IF V$ = "N"
			THEN
				RETURNED.VALUE = 1%
			ELSE
				RETURNED.VALUE = CMC$_UNDEFINED
			END IF
		END IF
	END IF

 EndFunc:
	FUNC_TESTENTRY = RETURNED.VALUE

	END FUNCTION
