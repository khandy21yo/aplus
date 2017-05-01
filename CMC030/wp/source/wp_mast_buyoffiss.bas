1	%TITLE "Buy off/Issue Journal Entry"
	%SBTTL "WP_MAST_BUYOFFISS"
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
	!	The ^*Buy off/Issue Journal Entry\* option
	!	maintains
	!	entries when whole quantities of a work
	!	order are completed.  For example, a
	!	work order could be for the manufacture
	!	of one hundred (100) widgets.  If
	!	twenty-five (25) of the widgets are
	!	completed, an entry in the buy off journal
	!	could be made to record the
	!	completion of twenty-five (25) units.
	!	.b
	!	Each Journal File is assigned a batch
	!	number consisting of two (2) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Buy off>Journal>Entry
	!	.x Issue>Journal>Entry
	!	.x Journal>Buy off
	!	.x Journal>Issue
	!
	! Option:
	!
	!	WP_MAIN_BUYOFF_01$HELP
	!	WP_MAIN_BUYOFFISS$HELP
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAST_BUYOFFISS/LINE
	!	$ LINK/EXE=WP_EXE: WP_MAST_BUYOFFISS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_MAST_BUYOFFISS.OBJ;*
	!
	! Author:
	!
	!	06/10/91 - Val James "Slick Stuff" Allen
	!
	! Modification history:
	!
	!	09/09/92 - Dan Perkins
	!		Added code to go to BUYOFFLINE after last field
	!		in BUYOFF.
	!
	!	06/25/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter to entr_3choice
	!
	!	06/22/95 - Kevin Handy
	!		Clean up strange formatting in source code.
	!
	!	12/22/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	12/22/95 - Kevin Handy
	!		Modified to use WP_MAIN_BUYOFF_01 instead of
	!		WP_MAIN_BUYOFF.
	!
	!	12/26/95 - Kevin Handy
	!		Lose common ares LOCATION, which wasn't
	!		being set to anything usefull.
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	02/13/98 - Kevin Handy
	!		Remove duplication of assigning REQNUM to ""
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/27/2000 - Kevin Handy
	!		Lose bogus error trap.
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
	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (CH_WP_BUYOFF) &
		WP_BUYOFF.CH%

	COM (CH_WP_BUYOFFLINE) &
		WP_BUYOFFLINE.CH%

	COM (CH_WP_BUYOFFLINE2) &
		WP_BUYOFFLINE2.CH%

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(WP_BUYOFFLINE2.CH%, STAT%)

	!
	! Look up device
	!
	CALL READ_DEVICE("WP_BUYOFF", WP_BUYOFF.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(WP_BUYOFF.DEV$ + "WP_BUYOFF_*.JRL", WP_BUYOFF_FILE$(), &
		16%, "", "")

	WP_BUYOFF_FILE% = VAL%(WP_BUYOFF_FILE$(0%))

	IF WP_BUYOFF_FILE%
	THEN
		WP_BUYOFF_FILE$(LOOP%) = &
			RIGHT(WP_BUYOFF_FILE$(LOOP%), 11%) &
			FOR LOOP% = 1% TO WP_BUYOFF_FILE%

		TEMP$ = "Buy Off Journal Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", WP_BUYOFF_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(WP_BUYOFF_FILE$(X%), -1%)
			GOTO 400
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Set up a screen and ask for batch number
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
			"Please enter at least 2 characters for batch", 0%)
		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(WP_MAIN_BUYOFF_01.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

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
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.HB"
	MAP (WP_BUYOFF)		WP_BUYOFF_CDD WP_BUYOFF
	MAP (WP_BUYOFF_OLD)	WP_BUYOFF_CDD WP_BUYOFF_OLD
	MAP (WP_BUYOFF_ONE)	WP_BUYOFF_CDD WP_BUYOFF_ONE

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.HB"
	MAP (WP_BUYOFFLINE)		WP_BUYOFFLINE_CDD WP_BUYOFFLINE
	MAP (WP_BUYOFFLINE_OLD)	WP_BUYOFFLINE_CDD WP_BUYOFFLINE_OLD
	MAP (WP_BUYOFFLINE_ONE)	WP_BUYOFFLINE_CDD WP_BUYOFFLINE_ONE

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.HB"
	MAP (WP_ISSJOUR)	WP_ISSJOUR_CDD WP_ISSJOUR

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION WP_MAIN_BUYOFF_01
	EXTERNAL LONG FUNCTION WP_MAIN_BUYOFFISS
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION JC_MAIN_JOB
	EXTERNAL LONG FUNCTION WP_MAIN_REGLINE
	EXTERNAL LONG FUNCTION WP_MAIN_ISSLINE
	EXTERNAL LONG FUNCTION WP_MAIN_REQREGISTER
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE WP_MAIN_BUYOFF_01.ID

		MAINT_GROUP = WP_MAIN_BUYOFF_01(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			WP_BUYOFF_ONE = WP_BUYOFF

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line"

		CASE OPT_MOREMENU
			WP_BUYOFF_ONE = WP_BUYOFF

			SELECT EDIT$(MVALUE, -1%)
			!
			! Line
			!
			CASE "LINE"
	!++
	! Abstract:LINE
	!--
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_BUYOFFISS.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Add records in subwindow
			!
			CASE "Add"
				WP_BUYOFF_ONE = WP_BUYOFF
				MAINT_GROUP = &
					MAIN_WINDOW(WP_MAIN_BUYOFFISS.ID, "A")

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF WP_BUYOFF::JOB <> WP_BUYOFF_OLD::JOB
				THEN
					WP_BUYOFF_ONE = WP_BUYOFF_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_BUYOFFISS.ID, "C")
					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_ISSLINE.ID, "C")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				WP_BUYOFF_ONE = WP_BUYOFF

				MAINT_GROUP = MAIN_WINDOW( &
					WP_MAIN_BUYOFFISS.ID, "E")
				MAINT_GROUP = MAIN_WINDOW( &
					WP_MAIN_ISSLINE.ID, "E")

			END SELECT

		END SELECT

	CASE WP_MAIN_BUYOFFISS.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = WP_BUYOFF_ONE::JOB

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = WP_BUYOFF::JOB

			CASE ELSE
				MVALUE = WP_BUYOFF_ONE::JOB

			END SELECT

		END SELECT

		MAINT_GROUP = WP_MAIN_BUYOFFISS(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			WP_BUYOFFLINE_ONE = WP_BUYOFFLINE

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " iSsue"

		CASE OPT_MOREMENU
			WP_BUYOFFLINE_ONE = WP_BUYOFFLINE

			SELECT EDIT$(MVALUE, -1%)
			!
			! Issue
			!
			CASE "ISSUE"
	!++
	! Abstract:ISSUE
	!--
				WP_ISSJOUR::REQNUM = ""
				WP_ISSJOUR::JOB = WP_BUYOFF::JOB
				WP_ISSJOUR::LLINE = WP_BUYOFFLINE::LLINE
 !				LOCATION$ = WP_BUYOFF::LOCATION

				MAINT_GROUP = &
					MAIN_WINDOW(WP_MAIN_ISSLINE.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				WP_BUYOFFLINE_ONE = WP_BUYOFFLINE

				MAINT_GROUP = MAIN_WINDOW( &
					WP_MAIN_ISSLINE.ID, "E")

			END SELECT

		END SELECT

	CASE WP_MAIN_ISSLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = "          " + WP_BUYOFFLINE_ONE::JOB + &
				WP_BUYOFFLINE_ONE::LLINE

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = "          " + WP_BUYOFFLINE::JOB + &
					WP_BUYOFFLINE::LLINE

			CASE ELSE
				MVALUE = "          " + &
					WP_BUYOFFLINE_ONE::JOB + &
					WP_BUYOFFLINE_ONE::LLINE

			END SELECT

		END SELECT

		MAINT_GROUP = WP_MAIN_ISSLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_JOB.ID

		MAINT_GROUP = JC_MAIN_JOB(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE WP_MAIN_REGLINE.ID

		MAINT_GROUP = WP_MAIN_REGLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE WP_MAIN_REQREGISTER.ID

		MAINT_GROUP = WP_MAIN_REQREGISTER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
