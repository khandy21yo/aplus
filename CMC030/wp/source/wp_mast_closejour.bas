1	%TITLE "Close Journal Entry"
	%SBTTL "WP_MAST_CLOSEJOUR"
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
	!	The ^*Close Journal Entry\* option maintains
	!	entries relative to completed jobs.
	!	.b
	!	Each Journal File is assigned a batch number consisting of two (2)
	!	alphanumeric characters.
	!
	! Index:
	!	.x Journal>Close
	!	.x Close>Journal
	!
	! Option:
	!
	!	WP_MAIN_CLOSEJOUR$HELP
	!	WP_MAIN_CLOSEJOURLINE$HELP
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAST_CLOSEJOUR/LINE
	!	$ LINK/EXE=WP_EXE: WP_MAST_CLOSEJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_MAST_CLOSEJOUR.OBJ;*
	!
	! Author:
	!
	!	07/21/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	07/24/92 - Dan Perkins
	!		Added option to print report.
	!
	!	06/13/94 - Kevin Handy
	!		Modified to open chart of accounts here in a
	!		read only mode, so it will not lock out the
	!		G/L CLOSE or RESET.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter to entr_3choice
	!
	!	09/14/95 - Kevin Handy
	!		Format source closer to 80 columns.
	!
	!	10/31/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	05/21/98 - Kevin Handy
	!		Increase size of batch number from 2 to 8 characters
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/11/98 - Kevin Handy
	!		Fix 6-character file name bugs.
	!
	!	02/17/98 - Kevin Handy
	!		Added dimension statement for WP_CLOSEJOUR_FILE$
	!		so it would show more that 10 items.
	!
	!	06/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART) GL_CHART_CDD GL_CHART

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_WP_CLOSEJOUR) &
		WP_CLOSEJOUR.CH%

	COM (BATCH_WP_CLOSEJOUR) &
		BATCH_NO$ = 8%

	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	DIM WP_CLOSEJOUR_FILE$(100%)

	%PAGE

	!
	! Initilization section - Prepare to do anything
	!
	ON ERROR GOTO 19000

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(WP_CLOSEJOURLINE2.CH%, STAT%)

	!
	! Look up device
	!
	CALL READ_DEVICE("WP_CLOSEJOUR", WP_CLOSEJOUR.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(WP_CLOSEJOUR.DEV$ + "WP_CLOSEJOUR_*.JRL", &
		WP_CLOSEJOUR_FILE$(), 16%, "", "")

	WP_CLOSEJOUR_FILE% = VAL%(WP_CLOSEJOUR_FILE$(0%))

	IF WP_CLOSEJOUR_FILE%
	THEN
		WP_CLOSEJOUR_FILE$(LOOP%) = &
			RIGHT(WP_CLOSEJOUR_FILE$(LOOP%), 14%) &
			FOR LOOP% = 1% TO WP_CLOSEJOUR_FILE%

		TEMP$ = "Closing Journal Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", WP_CLOSEJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(WP_CLOSEJOUR_FILE$(X%), -1%)
			GOTO 400
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for batch
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		20%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch number:", 11%, 30%)

320	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "BATCH"

	BATCH_NO$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		11%, 43%, BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	BATCH_NO$ = EDIT$(BATCH_NO$, -1%)

	IF LEN(TRM$(BATCH_NO$)) < 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter at least 2 characters for file number", &
			0%)
		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

760	!
	! Open chart of accounts read/only
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE 800
	END WHEN

	GL_CHART.READONLY% = -1%

800	!

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(WP_MAIN_CLOSEJOUR.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

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
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.HB"
	MAP (WP_CLOSEJOUR)	WP_CLOSEJOUR_CDD WP_CLOSEJOUR
	MAP (WP_CLOSEJOUR_OLD)	WP_CLOSEJOUR_CDD WP_CLOSEJOUR_OLD
	MAP (WP_CLOSEJOUR_ONE)	WP_CLOSEJOUR_CDD WP_CLOSEJOUR_ONE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION WP_MAIN_CLOSEJOUR
	EXTERNAL LONG FUNCTION WP_MAIN_CLOSELINE
	EXTERNAL LONG FUNCTION WP_MAIN_CLOSEWIP
	EXTERNAL LONG FUNCTION WP_OUTP_CLOSEJOUR
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION JC_MAIN_JOB
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE WP_MAIN_CLOSEJOUR.ID

		MAINT_GROUP = WP_MAIN_CLOSEJOUR(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " varianceS Wip_accounts Print_report"

		CASE OPT_MOREMENU
			WP_CLOSEJOUR_ONE = WP_CLOSEJOUR

			SELECT EDIT$(MVALUE, -1%)

			!
			! Line
			!
			CASE "VARIANCES"
	!++
	! Abstract:VARIANCES
	!--
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_CLOSELINE.ID, "")

			CASE "WIP_ACCOUNTS"
	!++
	! Abstract:WIPACCOUNTS
	!--
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_CLOSEWIP.ID, "")

			!
			! Report
			!
			CASE "PRINT_REPORT"
	!++
	! Abstract:REPORT
	!--
				V% = WP_OUTP_CLOSEJOUR(WP_CLOSEJOUR::JOB)

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! keep adding
			!
			CASE "Add"
				IF WP_CLOSEJOUR_ONE::VARFLAG = "Y"
				THEN
					WP_CLOSEJOUR_ONE = WP_CLOSEJOUR
					MAINT_GROUP = MAIN_WINDOW(WP_MAIN_CLOSEWIP.ID, "A")
				END IF

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF WP_CLOSEJOUR::JOB <> WP_CLOSEJOUR_OLD::JOB
				THEN
					WP_CLOSEJOUR_ONE = WP_CLOSEJOUR_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_CLOSELINE.ID, "C")

					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_CLOSEWIP.ID, "C")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				WP_CLOSEJOUR_ONE = WP_CLOSEJOUR
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_CLOSELINE.ID, "E")
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_CLOSEWIP.ID, "E")

			END SELECT

		END SELECT

	CASE WP_MAIN_CLOSELINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = WP_CLOSEJOUR_ONE::JOB

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = WP_CLOSEJOUR::JOB

			CASE ELSE
				MVALUE = WP_CLOSEJOUR_ONE::JOB

			END SELECT

		END SELECT

			MAINT_GROUP = WP_MAIN_CLOSELINE(SMG_WINDOW, MOPTION, &
				MLOOP, MFLAG, MVALUE)

	CASE WP_MAIN_CLOSEWIP.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = WP_CLOSEJOUR_ONE::JOB

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = WP_CLOSEJOUR::JOB

			CASE ELSE
				MVALUE = WP_CLOSEJOUR_ONE::JOB

			END SELECT

		END SELECT

		MAINT_GROUP = WP_MAIN_CLOSEWIP(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_JOB.ID

		MAINT_GROUP = JC_MAIN_JOB(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
