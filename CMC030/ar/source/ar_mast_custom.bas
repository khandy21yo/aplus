1	%TITLE "Customer Name/Address Maintenance"
	%SBTTL "AR_MAST_CUSTOM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	.b
	!	.lm +5
	!	The ^*Customer Name/Address Maintenance\* option
	!	maintains specific information relative to each
	!	customer.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Maintain Masterfile
	!	.x Maintain>Customer Masterfile
	!
	! Option:
	!	AR_MAIN_35CUSTOM$HELP
	!	AR_MAIN_CONTACT$HELP
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAST_CUSTOM/LINE
	!	$ LINK/EXEC:AR_EXE AR_MAST_CUSTOM,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_MAST_CUSTOM.OBJ;*
	!
	! Author:
	!
	!	02/10/88 - Aaron Redd
	!
	! Modification history:
	!
	!	03/20/95 - Kevin Handy
	!		(V3.6)
	!		Added ship-to address maintenance.
	!
	!	10/11/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map's
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! Commons
	!
	COM (CH_AR_CONTROL) AR_CONTROL.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

500	!
	! Open up control file and read in the control record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError


 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	RESUME ExitProgram

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Includes
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD AR_35CUSTOM
	MAP (AR_35CUSTOM_OLD)	AR_35CUSTOM_CDD AR_35CUSTOM_OLD
	MAP (AR_35CUSTOM_ONE)	AR_35CUSTOM_CDD AR_35CUSTOM_ONE

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION AR_MAIN_CONTACT
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN
	EXTERNAL LONG FUNCTION OE_MAIN_SHIPTO

	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AR_MAIN_35CUSTOM.ID
		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_ENTRY
			AR_35CUSTOM_ONE = AR_35CUSTOM
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Shipto"
		CASE OPT_MOREMENU
			AR_35CUSTOM_ONE = AR_35CUSTOM
			SELECT EDIT$(MVALUE, -1%)
			CASE "SHIPTO"
			!++
			! Abstract:SHIPTO
			!--
				MAINT_GROUP = MAIN_WINDOW(OE_MAIN_SHIPTO.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE
			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF AR_35CUSTOM::CUSNUM <> &
					AR_35CUSTOM_OLD::CUSNUM
				THEN
					AR_35CUSTOM_ONE = AR_35CUSTOM_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						OE_MAIN_SHIPTO.ID, "C")
				END IF
			!
			! Erase records in subwindow
			!
			CASE "Erase"
				AR_35CUSTOM_ONE = AR_35CUSTOM
				MAINT_GROUP = MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
					"E")

			END SELECT
		END SELECT


	CASE AR_MAIN_CONTACT.ID
		MAINT_GROUP = AR_MAIN_CONTACT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CUSTYPE.ID
		MAINT_GROUP = AR_MAIN_CUSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_CATEGORY.ID
		MAINT_GROUP = OE_MAIN_CATEGORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SALESTAX.ID
		MAINT_GROUP = OE_MAIN_SALESTAX(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID
		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_COUNTRY.ID
		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_TERMS.ID
		MAINT_GROUP = UT_MAIN_TERMS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_CARRIER.ID
		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SA_MAIN_SALESMAN.ID
		MAINT_GROUP = SA_MAIN_SALESMAN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SHIPTO.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = AR_35CUSTOM_ONE::CUSNUM

		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = AR_35CUSTOM::CUSNUM
			CASE ELSE
				MVALUE = AR_35CUSTOM_ONE::CUSNUM
			END SELECT
		END SELECT

		MAINT_GROUP = OE_MAIN_SHIPTO(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
