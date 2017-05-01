1	%TITLE "ORDER ENTRY QUERY"
	%SBTTL "OE_QURY_QUERY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	The ^*Order Entry Query\* option views the order register
	!	records. An inquiry may be made in reference to a specific order, or the user
	!	may look up an order in reference to a specific customer. Information as it
	!	pertains to each line item on an order can be obtained, i.e. quantity of an
	!	item ordered, quantity shipped on what date(s), quantity that has
	!	been canceled, quantity remaining to be shipped, etc.
	!
	! Index:
	!	.x Query
	!	.x Order Enter>Query
	!
	! Option:
	!	OE_MAIN_QUERYORDER$HELP
	!	OE_MAIN_QUERYLINE$HELP
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_QURY_QUERY/LINE
	!	$ LINK/EXE=OE_EXE: OE_QURY_QUERY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_QURY_QUERY.OBJ;*
	!
	! Author:
	!
	!	01/07/91 - Val James Allen
	!
	! Modification history:
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Define OE_MAIN_QUERYORDER.ID in OE_WINDOW.INC
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose HelpFile (Dead code)
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_OE_REGHEADER) &
		OE_REGHEADER.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION MAINT_GROUP

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(OE_MAIN_QUERYORDER.ID, "")

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

 ! HelpFile:
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !	RESUME ExitProgram

 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	RESUME ExitProgram

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD OE_REGHEADER
	MAP (OE_REGHEADER_OLD)	OE_REGHEADER_CDD OE_REGHEADER_OLD
	MAP (OE_REGHEADER_ONE)	OE_REGHEADER_CDD OE_REGHEADER_ONE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION OE_MAIN_QUERYORDER
	EXTERNAL LONG FUNCTION OE_MAIN_REGLINE
	EXTERNAL LONG FUNCTION OE_MAIN_ORDERTYPE
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION MAIN_WINDOW
	EXTERNAL LONG FUNCTION AR_MAIN_QUERYCUST
	EXTERNAL LONG FUNCTION OE_MAIN_QUERYLINE


	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE OE_MAIN_QUERYORDER.ID

		MAINT_GROUP = OE_MAIN_QUERYORDER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + &
				" Linedetail Customerlookup lineSummary"
		CASE OPT_MOREMENU
			OE_REGHEADER_ONE = OE_REGHEADER
			SELECT EDIT$(MVALUE, -1%)
			!
			! Line
			!
			CASE "LINEDETAIL"
	!++
	! Abstract:LINEDETAIL
	!--
				MAINT_GROUP = &
					MAIN_WINDOW(OE_MAIN_REGLINE.ID, "VX0")

			CASE "LINESUMMARY"
	!++
	! Abstract:LINESUMMARY
	!--
				MAINT_GROUP = &
					MAIN_WINDOW(OE_MAIN_QUERYLINE.ID, "")

			CASE "CUSTOMERLOOKUP"

				MAINT_GROUP = &
					MAIN_WINDOW(AR_MAIN_QUERYCUST.ID, "")

			END SELECT
		END SELECT

	CASE OE_MAIN_REGLINE.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = OE_REGHEADER_ONE::ORDNUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = OE_REGHEADER::ORDNUM
			CASE ELSE
				MVALUE = OE_REGHEADER_ONE::ORDNUM
			END SELECT
		END SELECT

		MAINT_GROUP = OE_MAIN_REGLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)


	CASE OE_MAIN_QUERYLINE.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = OE_REGHEADER_ONE::ORDNUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = OE_REGHEADER::ORDNUM
			CASE ELSE
				MVALUE = OE_REGHEADER_ONE::ORDNUM
			END SELECT
		END SELECT

		MAINT_GROUP = OE_MAIN_QUERYLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)


	CASE OE_MAIN_ORDERTYPE.ID

		MAINT_GROUP = OE_MAIN_ORDERTYPE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SALESTAX.ID

		MAINT_GROUP = OE_MAIN_SALESTAX(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_CATEGORY.ID

		MAINT_GROUP = OE_MAIN_CATEGORY(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_QUERYCUST.ID

		MAINT_GROUP = AR_MAIN_QUERYCUST(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_CARRIER.ID

		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_TERMS.ID

		MAINT_GROUP = UT_MAIN_TERMS(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)


	END SELECT

32767	END FUNCTION
