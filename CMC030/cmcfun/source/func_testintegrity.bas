1	%TITLE "Data Entry Integrity Test Function"
	%SBTTL "FUNC_TESTINTEGRITY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG FUNC_TESTINTEGRITY(CDD_WINDOW_CDD SMG_WINDOW, &
		STRING PAR_TOTEST, &
		STRING PAR_DATABASE, &
		STRING PAR_PROGRAM, &
		STRING PAR_VARID, &
		STRING PAR_DESC, &
		LONG PAR_WINDOWNUM, &
		LONG PAR_KEYNUM, &
		LONG PAR_INTEGRITYFLAG)

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
	!	SMG_WINDOW
	!		Window structure to use in test
	!
	!	PAR_TOTEST
	!		The data to be tested.
	!
	!	PAR_DATABASE
	!		The database in which the data would be defined.
	!
	!	PAR_PROGRAM
	!		The name of the program which calls this function.
	!
	!	PAR_VARID
	!		The variable identification.  This will be
	!		the structure name to test the data item
	!		against if this is a referential integrity test.
	!		This will be the data item name for any other
	!		integrity test.
	!
	!	PAR_DESC
	!		The description parameter is used to pass a
	!		message to be displayed is the integrity test
	!		is not satisfied.
	!
	!	PAR_WINDOWNUM
	!		The number used to identify the MAIN program
	!		maintaining the descriptions which might or
	!		might not define the data in question.
	!
	!	PAR_KEYNUM
	!		If this is a referential integrity test then
	!		this is the key number to do the look up on.
	!		The key number will default to zero.
	!
	!	PAR_INTEGRITYFLAG
	!		The integrity flag indicates what kind of
	!		integrity test will be performed.
	!			1% - Referential
	!
	!	Returned value
	!		The value the MAIN program will be set to
	!		during the TESTINTEGRITY option, where this
	!		function will be used.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_TESTINTEGRITY/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP FUNC_TESTINTEGRITY
	!	$ DELETE FUNC_TESTINTEGRITY.OBJ;*
	!
	! Author:
	!
	!	04/06/89 - Robert Peterson
	!
	! Modification history:
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
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! Store scope.exit because main window may lose the value
	! stored in it.
	!
	LOC_SCOPE.EXIT%	= SCOPE::SCOPE_EXIT

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION MAIN_WINDOW
	EXTERNAL STRING		FUNCTION READ_SET

	!
	! Declare some variables
	!
	DECLARE LONG LOC_RETURNEDVALUE
	DECLARE STRING LOC_ANSWER

	!
	! Select the type of integrity testing to be done
	!
	SELECT PAR_INTEGRITYFLAG

	CASE (PAR_INTEGRITYFLAG AND 1%)
		!
		! Is the input defined?
		!
		IF MAIN_WINDOW(PAR_WINDOWNUM, "Q" + NUM1$(PAR_KEYNUM) + &
			PAR_TOTEST) <> 1%
		THEN
			!
			! See if they are allowing undefined inputs
			!
			LOC_ANSWER = LEFT(READ_SET(TRM$(PAR_PROGRAM), PAR_VARID), 1%)
			LOC_ANSWER = LEFT(READ_SET(PAR_DATABASE + "_ALLOW", PAR_VARID), 1%) &
				IF LOC_ANSWER = ""

			IF LOC_ANSWER = "N"
			THEN
				!
				! Don't let them get past is we don't allow
				! them to enter undefined values.
				!
				LOC_RETURNEDVALUE = 1%
				CALL ENTR_3MESSAGE(SCOPE,  PAR_DESC + " is undefined ", 1%)
			ELSE
				!
				! Verify that they really want to enter an
				! undefined value.
				!
				LOC_ANSWER = ENTR_3YESNO(SCOPE,  SMG_WINDOW::WNUMBER, "", &
					PAR_DESC + " is undefined, confirm entry" + &
					" then press <DO> ", &
					"N", 0%, "", "")
				LOC_RETURNEDVALUE = 1% IF LOC_ANSWER = "N"
			END IF
		END IF

	END SELECT

	!
	! Restore scope exit value
	!
	SCOPE::SCOPE_EXIT = LOC_SCOPE.EXIT%

	FUNC_TESTINTEGRITY = LOC_RETURNEDVALUE

	END FUNCTION
