1	%TITLE "User Defined Macros List"
	%SBTTL "UTL_RPRT_MACROS"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:UT021
	!
	! Abstract:HELP
	!	.p
	!	The ^*Macro List\* provides a report
	!	which contains the user defined macros.
	!
	! Index:
	!	.x Report>User Defined Macros
	!	.x User Defined Macros>Macros
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_MACROS/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_MACROS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_MACROS.OBJ;*
	!
	! AUTHOR:
	!
	!	04/09/90 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	06/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/07/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	!++
	! Abstract:COMMAND
	!	^*MACRO/LIST\*
	!	.p
	!	The ^*Macro/List\* option prints MCL commands and
	!	the user defined macros.
	!	.p
	!	^*Format: MACRO/LIST\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /MACRO/LIST
	!	.end literal
	!
	! Index:
	!	.x MACRO/LIST
	!	.x Report>User Defined Macros
	!	.x User Defined Macros>Macros
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MACROS.HB"
	MAP (UTL_MACROS) UTL_MACROS_CDD UTL_MACROS

	DECLARE STRING TEXT, MACRO, DES, FROM_ITEM, TO_ITEM, WLDCRD, PRINT_FORM

	DECLARE BYTE J

	DECLARE LONG LOOP, TEST_LOOP, I

	DECLARE WORD CONSTANT PRINT_WIDTH = 132%, INIT_DIM = 1000%

	DIMENSION STRING CODE(INIT_DIM), DESCR(INIT_DIM)
	DIMENSION LONG COLUMN(5%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, PRINT_WIDTH)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Macro\*
	!	.p
	!	A ^*From Macro\* code causes the
	!	printing of the User Defined Macro report to begin with a
	!	selected macro command.
	!	.p
	!	A blank setting causes the report to begin with the first
	!	macro command in the file.
	!
	! Index:
	!	.x Macro Field
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Macro\*
	!	.p
	!	A ^*To Macro\* code causes the
	!	printing to end with the selected User Defined
	!	Macro command.
	!	.p
	!	A blank setting will cause the report to end with the last
	!	UOM code in the file.
	!
	! Index:
	!	.x Macro Field
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects designated
	!	user defined macro commands to be printed
	!	by entering a wildcard value.
	!
	! Index:
	!	.x Wildcard
	!
	!--

	PRINT_FORM = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Format (H,V)\*
	!	.p
	!	The ^*Format (H,V)\* field determines the
	!	format in which the User Defined Macro list will be printed.
	!	.p
	!	This field must have a value. Valid options are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	H = Horizontal
	!	.le
	!	V = Vertical
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Report Format
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_MACROS.OPN"
	USE
		FILENAME$ = "UTL_MACROS"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "MCL COMMANDS AND USER DEFINED MACROS LIST"
	TITLE$(2%) = "Utility System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890
	SELECT PRINT_FORM

	CASE "V"
		TITLE$(4%) = "Command              Description"

	CASE "H"
		TITLE$(4%) = "Command              Description" + SPACE$(30%) + &
			"Command              Description" + SPACE$(30%)

	END SELECT

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #UTL_MACROS.CH%
		ELSE
			FIND #UTL_MACROS.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
		END IF
	USE
		FILENAME$ = "UTL_MACROS"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #UTL_MACROS.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_MACROS"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (UTL_MACROS::COMMAND > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(UTL_MACROS::COMMAND, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	SELECT PRINT_FORM

	CASE "V"
		TEXT = UTL_MACROS::COMMAND + " " + &
			UTL_MACROS::DESCRIPTION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)

	CASE "H"
		LOOP = LOOP + 1%
		CODE(LOOP) = UTL_MACROS::COMMAND
		DESCR(LOOP) = UTL_MACROS::DESCRIPTION

	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	DATA	"====================", "MCL Commands", &
		"AMORTIZATION", "Amortization Schedule", &
		"BATCH", "Process Batch File", &
		"BATCH/MONITOR", "Monitoring Batch Process", &
		"BYE", "Same as LOGOUT", &
		"CALENDAR", "Calendar", &
		"COMPRESS", "Compress RMS Files", &
		"COUNTRY", "Country Definition Table", &
		"COUNTRY/LIST", "Country Definition Table List", &
		"DEVICE", "Device File Definition Table", &
		"DEVICE/LIST", "Device File Definition Table List", &
		"END", "Same as EXIT", &
		"EXIT", "Go One Level UP in the System Menu", &
		"HELP", "MCL Help Message", &
		"LOGOUT", "Logout from the User Account", &
		"LOGOUT/FULL", "Same as LOGOUT with Statistic Info", &
		"MACRO", "Define User Macro", &
		"MACRO/DELETE", "Delete User Defined Macro", &
		"MACRO/LIST", "List User Defined Macros", &
		"PROFILE", "Company Profile Set Up", &
		"PROFILE/PERIOD", "Company Accounting Period List", &
		"PROFILE/STRUCTURE", "Company Structure Report", &
		"QUIT", "Exit from the Menu and Go to DCL", &
		"REPORT", "User Reports", &
		"REPORT/DESTINATION", "User Reports Destination Table", &
		"REPORT/LIST", "User Report Settings List", &
		"REPORT/SETTINGS", "User Reports Settings File", &
		"STRING", "Set Up Report String", &
		"STRING/LIST", "Set Up Report String List", &
		"STRING/PRINT", "Print a Specific String of Reports", &
		"SYSTEM", "Go to the Top System Menu Level", &
		"SYSTEM/INSTALL", "Install or Remove System Menu", &
		"SYSTEM/LIST", "System Menu List", &
		"TIMEOUT", "Set Up Time Out Variable", &
		"", ""

	MCL_LOOP = LOOP + 1%
	READ MACRO, DES

	WHILE MACRO <> ""
		LOOP = LOOP + 1%
		CODE(LOOP) = MACRO + SPACE$(20% - LEN(MACRO))
		DESCR(LOOP) = DES + SPACE$(40% - LEN(DES))
		READ MACRO, DES
	NEXT

	IF PRINT_FORM = "H" AND LOOP > 0%
	THEN
		TEST_LOOP = LOOP

		FOR I = 2% TO 2%
			COLUMN(I) = COLUMN(I - 1%) + &
				INT(TEST_LOOP/(4.0 - I) + 0.9)
			TEST_LOOP = LOOP - COLUMN(I)
		NEXT I

		FOR I = 1% TO COLUMN(2%) - 1% STEP 1%
			TEXT = ""
			FOR J = 1% TO 2%
				TEXT = TEXT + CODE(I + COLUMN(J)) + " " + &
					DESCR(I + COLUMN(J)) + " "
			NEXT J
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)
		NEXT I

		TEXT = ""

		FOR J = 1% TO LOOP - 2% * (COLUMN(2%) - 1%)
			TEXT = TEXT + CODE(COLUMN(2%) + COLUMN(J)) + " " + &
				DESCR(COLUMN(2%) + COLUMN(J)) + " "
		NEXT J

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)

	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			CODE(J) + " " + DESCR(J), 0%) FOR J = MCL_LOOP TO LOOP

	END IF

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
