1	%TITLE "Country, State and County"
	%SBTTL "UTL_MAST_COUNTRY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
	!
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
	! Abstract:HELP
	!	.p
	!	The ^*Country, State and County\* option
	!	enters codes for countries, states and counties.
	!	.p
	!	The Country Codes are standard ANSI codes.  Modification should not be
	!	considered.
	!	.p
	!	The State Codes are standard Post Office codes. Modification should not be
	!	considered.
	!	.p
	!	The County Codes, if needed, are user defined.
	!
	! Index:
	!	.x ANSI Codes>Country
	!	.x Country>ANSI Codes
	!	.x Table>Country
	!	.x Table>State
	!	.x Table>County
	!	.x Country>Table
	!	.x State>Table
	!	.x County>Table
	!
	! Option:
	!
	!	UTL_MAIN_COUNTRY$HELP
	!	UTL_MAIN_STATE$HELP
	!	UTL_MAIN_COUNTY$HELP
	!	UTL_MAIN_REGION$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAST_COUNTRY/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_MAST_COUNTRY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_MAST_COUNTRY.OBJ;*
	!
	! Author:
	!
	!	02/03/88 - Aaron Redd
	!
	! Modification history:
	!
	!	08/01/89 - Aaron Redd
	!		Modified to use MAIN_WINDOW instead of MAIN_JOURNAL
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	!++
	! Abstract:COMMAND
	!	^*COUNTRY\*
	!	.p
	!	^*County\* accesses codes for countries, states, and counties.
	!	.p
	!	The Country Codes are standard ANSI codes. Modification should not be
	!	considered.
	!	.p
	!	The State Codes are standard Post Office codes. Modification should not be
	!	considered.
	!	.p
	!	The County Codes, if needed, are user defined.
	!	.p
	!	^*Format: COUNTRY\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /COUNTRY
	!	.end Literal
	!
	! Index:
	!	.x ANSI Codes>Country
	!	.x Country>ANSI Codes
	!	.x Table>Country
	!	.x Table>State
	!	.x Table>County
	!	.x Country>Table
	!	.x State>Table
	!	.x County>Table
	!	.y ANSI
	!
	! Option:
	!	UTL_MAIN_COUNTRY$HELP
	!	UTL_MAIN_STATE$HELP
	!	UTL_MAIN_COUNTY$HELP
	!	UTL_RPRT_COUNTRY$COMMAND
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initialize maintenance
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions and memory MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP	(UTL_COUNTRY)	UTL_COUNTRY_CDD	UTL_COUNTRY
	MAP	(UTL_COUNTRY_2)	UTL_COUNTRY_CDD	UTL_COUNTRY_OLD
	MAP	(UTL_COUNTRY_1)	UTL_COUNTRY_CDD	UTL_COUNTRY_ONE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP	(UTL_STATE)	UTL_STATE_CDD	UTL_STATE
	MAP	(UTL_STATE_OLD)	UTL_STATE_CDD	UTL_STATE_OLD
	MAP	(UTL_STATE_ONE)	UTL_STATE_CDD	UTL_STATE_ONE

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG	FUNCTION UTL_MAIN_STATE
	EXTERNAL LONG	FUNCTION UTL_MAIN_COUNTY
	EXTERNAL LONG	FUNCTION UTL_MAIN_REGION

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE UTL_MAIN_COUNTRY.ID

		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		!
		! Modify the menu
		!
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " State reGion"

		!
		! Optional menu items
		!
		CASE OPT_MOREMENU
			UTL_COUNTRY_ONE = UTL_COUNTRY
			SELECT SCOPE::PRG_ITEM
			!
			! Define states (provinces, etc.) within the country
			!
			CASE "State"
				!
				! Go to State description maintenance
				!
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_STATE.ID, "")

			CASE "reGion"
				!
				! Go to State description maintenance
				!
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_REGION.ID, "")

			END SELECT

		!
		! Handle finishing various options specially
		!
		CASE OPT_AFTEROPT

			SELECT MVALUE
			!
			! Change records
			!
			CASE "Change", "Blank", "Initialize"
				!
				! Change line items to match new header
				! if the key was changed.
				!
				IF (UTL_COUNTRY_OLD::COUNTRY <> UTL_COUNTRY::COUNTRY)
				THEN
					TEMP$ = UTL_COUNTRY::COUNTRY
					UTL_COUNTRY = UTL_COUNTRY_OLD
					MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_STATE.ID, "C" + TEMP$)

					TEMP$ = TEMP$ + UTL_STATE::STATE
					UTL_STATE = UTL_STATE_OLD
					MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_COUNTY.ID, "C" + TEMP$)
				END IF

			!
			! Erase record
			!
			CASE "Erase"
				!
				! Erase any line items under the header
				!
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_REGION.ID, "E")
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_STATE.ID, "E")
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_COUNTY.ID, "E")

			END SELECT

		END SELECT

	CASE UTL_MAIN_STATE.ID
		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = UTL_COUNTRY_ONE::COUNTRY
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = UTL_COUNTRY::COUNTRY
			CASE ELSE
				MVALUE = UTL_COUNTRY_ONE::COUNTRY
			END SELECT
		END SELECT

		MAINT_GROUP = UTL_MAIN_STATE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		!
		! Modify the menu
		!
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " cOunty"

		!
		! Optional menu items
		!
		CASE OPT_MOREMENU
			UTL_STATE_ONE = UTL_STATE
			SELECT SCOPE::PRG_ITEM
			!
			! Define counties (townships, etc.) within the state
			!
			CASE "cOunty"
				!
				! Go to County description maintenance
				!
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_COUNTY.ID, "")
			END SELECT

		!
		! Handle finishing various options specially
		!
		CASE OPT_AFTEROPT

			SELECT MVALUE
			!
			! Change records
			!
			CASE "Change", "Blank", "Initialize"
				!
				! Change line items to match new header
				! if the key was changed.
				!
				IF (UTL_STATE_OLD::STATE <> UTL_STATE::STATE)
				THEN
					TEMP$ = UTL_STATE::COUNTRY + UTL_STATE::STATE
					UTL_STATE = UTL_STATE_OLD
					MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_COUNTY.ID, "C" + TEMP$)
				END IF

			!
			! Erase record
			!
			CASE "Erase"
				!
				! Erase any line items under the header
				!
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_COUNTY.ID, "E")

			END SELECT

		END SELECT

	CASE UTL_MAIN_COUNTY.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = UTL_STATE_ONE::COUNTRY+UTL_STATE_ONE::STATE
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = UTL_STATE::COUNTRY+UTL_STATE::STATE
			CASE ELSE
				MVALUE = UTL_STATE_ONE::COUNTRY+UTL_STATE_ONE::STATE
			END SELECT
		END SELECT

		MAINT_GROUP = UTL_MAIN_COUNTY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_REGION.ID
		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = UTL_COUNTRY_ONE::COUNTRY
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = UTL_COUNTRY::COUNTRY
			CASE ELSE
				MVALUE = UTL_COUNTRY_ONE::COUNTRY
			END SELECT
		END SELECT

		MAINT_GROUP = UTL_MAIN_REGION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
