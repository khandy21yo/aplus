1	%TITLE "Accounts Payable Register Maintenance"
	%SBTTL "AP_MAST_37REG_MAINT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2001 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Accounts Payable Ledger File Maintenance\* option
	!	maintains records in either the open or closed
	!	Accounts Payable Ledger Files.
	!	^*
	!	.note
	!	#^*This option is not intended for regular or routine
	!	maintenance procedures\*, but rather as a means to make
	!	unusual types of edits in the file which would be very
	!	impractical, if not impossible to achieve, by entering
	!	transactions in the Accounts Payable journals. ^*Extreme
	!	caution must be exercised in the use of this option for
	!	any purpose other than the examples listed below.\*
	!	.end note
	!	.b
	!	Useful examples of utilizing this option are to:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Initialize open file during installation
	!	.le
	!	Change a 1099 code or amount
	!	.le
	!	Change a use tax flag or amount
	!	.le
	!	Change a discount date or due date
	!	.le
	!	Correct an invoice number or date
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x AP Ledger>Maintenance
	!	.x Maintenance>Accounts Payable Ledger
	!	.x Accounts Payable Ledger>Maintenance
	!	.x Maintenance>AP Ledger
	!
	! Option:
	!	AP_MAIN_OPEN_MAINT$HELP
	!	AP_MAIN_OPEN_DIST$HELP
	!	AP_MAIN_CLOSE_MAINT$HELP
	!
	! Author:
	!
	!	03/14/2001 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAST_37REG_MAINT/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_MAST_37REG_MAINT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_MAST_37REG_MAINT.OBJ;*
	!
	! Modification history:
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	!
	! Map of the various files
	!
	MAP (CH_AP_CONTROL) &
		AP_CONTROL.CH%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	ON ERROR GOTO 19000

	!
	! Get info required for main file
	!
720	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.CRE"
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AP_MAIN_OPEN_MAINT.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************
	!
	! Untrapped error
	!
	RESUME HelpError

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)


	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! CDD and Maps
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION AP_MAIN_1099_TABLE
	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR
	EXTERNAL LONG FUNCTION AP_MAIN_OPEN_MAINT
	EXTERNAL LONG FUNCTION AP_MAIN_OPEN_DIST
	EXTERNAL LONG FUNCTION AP_MAIN_CLOSE_MAINT

	EXTERNAL LONG   FUNCTION MAIN_JOURNAL

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_1099_TABLE.ID

		MAINT_GROUP = AP_MAIN_1099_TABLE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_VENDOR.ID

		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_OPEN_MAINT.ID	! Open window

		MAINT_GROUP = AP_MAIN_OPEN_MAINT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		!
		! Modify options
		!
		CASE OPT_OPTLIST

			MVALUE = MVALUE + " disT"

		!
		! More menu items
		!
		CASE OPT_MOREMENU
			SELECT MVALUE
			CASE "disT"
	!++
	! Abstract:DIST
	!	^*Distribution Maintenance\*
	!	.p
	!	The ^*Distribution Maintenance\* option
	!	edits the distribution of the current transaction.
	!
	! Index:
	!	.x Distribution>Open
	!
	!--
				V% = MAIN_JOURNAL(AP_MAIN_OPEN_DIST.ID, "")
			END SELECT

		END SELECT

	CASE AP_MAIN_OPEN_DIST.ID	! Distribution window

		MAINT_GROUP = AP_MAIN_OPEN_DIST(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_CLOSE_MAINT.ID	! Close window

		MAINT_GROUP = AP_MAIN_CLOSE_MAINT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
