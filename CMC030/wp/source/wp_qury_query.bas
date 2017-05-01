1	%TITLE "Work in Process Query"
	%SBTTL "WP_QURY_QUERY"
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
	!	.lm +5
	!	.b
	!	The ^*Work in Process Query\* option views selected order
	!	register records.  The information provided in an inquiry includes:
	!	.table 3,25
	!	.te
	!	Job Number	Job Description
	!	.te
	!	Job Type	Job Class
	!	.te
	!	Job Status	Begin Date
	!	.te
	!	End Date	Location
	!	.te
	!	Operator	Reference _#
	!	.end table
	!	For each line item associated with a job, the following information can be
	!	printed:
	!	.table 3,25
	!	.te
	!	Line	Line Type
	!	.te
	!	Item Code	Description
	!	.te
	!	Quantity Ordered	Quantity Complete
	!	.te
	!	Quantity Cancelled	Balance
	!	.end table
	!	There are also ^*Linedetail\* and ^*lineSummary\* functions in the COMMAND menu
	!	of the Query screen. By accessing these functions, additional information
	!	relative to line items can be viewed.  This additional information includes:
	!	.table 3,25
	!	.te
	!	Cost	Start Date
	!	.te
	!	Completed Date	Unit Cost
	!	.te
	!	Batch Posting Reference
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Query
	!	.x Job>Query
	!	.x Line>Item>Query
	!	.x Line>Summary>Query
	!
	! Option:
	!	WP_MAIN_QUERY$HELP
	!	WP_QURY_QUERY$LINEDETAIL
	!	WP_MAIN_QUERYLINE$HELP
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_QURY_QUERY/LINE
	!	$ LINK/EXE=WP_EXE: WP_QURY_QUERY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_QURY_QUERY.OBJ;*
	!
	! Author:
	!
	!	07/30/91 - Craig Tanner
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/31/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/19/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose HelpFile (Dead Code)
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%

	COM (CH_WP_REGLINE) &
		WP_REGLINE.CH%, &
		WP_REGLINE.READONLY%

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

	V% = MAIN_WINDOW(WP_MAIN_QUERY.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	COM (CH_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%

	COM (CH_WP_REGLINE) &
		WP_REGLINE.CH%, &
		WP_REGLINE.READONLY%

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD JC_JOB
	MAP (JC_JOB_OLD)	JC_JOB_CDD JC_JOB_OLD
	MAP (JC_JOB_ONE)	JC_JOB_CDD JC_JOB_ONE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION WP_MAIN_QUERY
	EXTERNAL LONG FUNCTION WP_MAIN_QUERYLINE
	EXTERNAL LONG FUNCTION WP_MAIN_REGLINE
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION JC_MAIN_TYPE
	EXTERNAL LONG FUNCTION JC_MAIN_CLASS
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION MAIN_WINDOW


	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE WP_MAIN_QUERY.ID

		MAINT_GROUP = WP_MAIN_QUERY(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Linedetail lineSummary"
		CASE OPT_MOREMENU
			JC_JOB_ONE = JC_JOB
			SELECT EDIT$(MVALUE, -1%)
			!
			! Line
			!
			CASE "LINEDETAIL"
	!++
	! Abstract:LINEDETAIL
	!--
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_REGLINE.ID, &
					"VX")

			CASE "LINESUMMARY"
	!++
	! Abstract:LINESUMMARY
	!--
				MAINT_GROUP = &
					MAIN_WINDOW(WP_MAIN_QUERYLINE.ID, "")

			END SELECT
		END SELECT

	CASE WP_MAIN_REGLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = JC_JOB_ONE::JOB

		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = JC_JOB::JOB
			CASE ELSE
				MVALUE = JC_JOB_ONE::JOB
			END SELECT
		END SELECT

		MAINT_GROUP = WP_MAIN_REGLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)


	CASE WP_MAIN_QUERYLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = JC_JOB_ONE::JOB

		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = JC_JOB::JOB
			CASE ELSE
				MVALUE = JC_JOB_ONE::JOB
			END SELECT
		END SELECT

		MAINT_GROUP = WP_MAIN_QUERYLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_TYPE.ID

		MAINT_GROUP = JC_MAIN_TYPE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_CLASS.ID

		MAINT_GROUP = JC_MAIN_CLASS(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
