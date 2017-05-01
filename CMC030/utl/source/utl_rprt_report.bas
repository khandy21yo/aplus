1	%TITLE "User Report Settings"
	%SBTTL "UTL_RPRT_REPORT"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! ID:UT002
	!
	! Abstract:HELP
	!	.P
	!	The ^*Report Settings\* report provides a report which
	!	contains the following:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Report Number
	!	.le
	!	System
	!	.le
	!	Description
	!	.le
	!	Type
	!	.le
	!	Length
	!	.le
	!	Required
	!	.le
	!	Valid
	!	.le
	!	Description
	!	.le
	!	Type
	!	.le
	!	Length
	!	.le
	!	Required
	!	.le
	!	Valid
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Report Settings>Report
	!	.x Report>Report Settings
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_REPORT/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_REPORT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_REPORT.OBJ;*
	!
	! AUTHOR:
	!
	!	12/17/85 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	11/30/87 - Frantisek Starman
	!		Change report setting and printing format
	!
	!	10/22/90 - Kevin Handy
	!		Modified to place a flag on output if program
	!		doesn't exist.
	!
	!	10/24/90 - Kevin Handy
	!		Added Long/Short option.
	!
	!	06/16/93 - Kevin Handy
	!		Added REGARDLESS to UTL_REPORT.
	!
	!	06/17/93 - Kevin Handy
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
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	!++
	! Abstract:COMMAND
	!	^*REPORT/LIST\*
	!	.P
	!	Creates a report according to the specifications the
	!	programmer provides.
	!	.P
	!	^*Format: REPORT/LIST\*
	! Index:
	!	.x Report Settings>Report
	!	.x Report>Report Settings
	!
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT) UTL_REPORT_CDD UTL_REPORT

	%PAGE

	!
	! Main startup
	!
	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Report _#\*
	!	.p
	!	The ^*From Report _#\* field
	!	causes the printing to begin with a selected
	!	Report _#.
	!	.p
	!	A blank setting causes the report to begin with
	!	the first Report _# in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Report _#\*
	!	.p
	!	The ^*To Report _#\* causes
	!	the printing to end with a selected
	!	Report _#.
	!	.p
	!	A blank setting causes the report to end with
	!	the last Report _# in the file.
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated Report _#'s to be printed by entering a
	!	wildcard value.
	!
	! Index:
	!
	!--

	SYSWLD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) System\*
	!	.p
	!	The ^*System\* option enters the
	!	system name for which a report menu will be printed.
	!	.p
	!	The field will accommodate twenty (20) characters.
	!
	! Index:
	!
	!--

	LONGSHORT$ = LEFT$(UTL_REPORTX::OPTDEF(5%), 1%)

	!++
	! Abstract:FLD06
	!	^*(05) System\*
	!	.p
	!	The ^*System\* option enters the
	!	system name for which a report menu will be printed.
	!	.p
	!	The field will accommodate twenty (20) characters.
	!
	! Index:
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.OPN"
	USE
		FILENAME$ = "UTL_REPORT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "REPORT  SETTINGS"
	TITLE$(2%) = "Utility system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890
	TITLE$(4%) = "RepNum System Description          T Len Req Valid" + &
		"                            Description          " + &
		"T Len Req Valid"
	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #UTL_REPORT.CH%
		ELSE
			FIND #UTL_REPORT.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
	END WHEN

 GetNextRec:
17020	!
	! Get record
	!
	WHEN ERROR IN
		GET #UTL_REPORT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_REPORT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal IF (UTL_REPORT::REPNUM > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(UTL_REPORT::REPNUM, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(UTL_REPORT::SYSTEM, -1%), SYSWLD$) = 0% &
		AND SYSWLD$ <> ""

	!
	! Output inforation
	!
	TEXT$ = UTL_REPORT::REPNUM + " " + &
		UTL_REPORT::SYSTEM + " " + &
		UTL_REPORT::REPDES + " " + &
		TRM$(UTL_REPORT::PRODEV) + &
		UTL_REPORT::PRONAM + &
		FORMAT$(UTL_REPORT::REPWID, "###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)

	IF (FIND_FILEEXISTS(TRM$(UTL_REPORT::PRODEV) + &
		TRM$(UTL_REPORT::PRONAM) + ".EXE", 0%) = 0%)
	THEN
		TEXT$ = "      *** EXE missing ***"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	GOTO 17350 IF LONGSHORT$ = "S"

	FOR I% = 0% TO 4%
		TEXT$ = SPACE$(9%) + "(" + FORMAT$(I% + 1%, "<0>#") + ") " + &
			UTL_REPORT::DESCR(I%) + " " + &
			UTL_REPORT::OPTTYPE(I%) + " " + &
			FORMAT$(UTL_REPORT::OPTLEN(I%), "<%>##") + " " + &
			UTL_REPORT::REQUIRE(I%) + "   " + &
			UTL_REPORT::VALID(I%) + &
			SPACE$(8%) + &
			"(" + FORMAT$(I% + 6%, "<0>#") + ") " + &
			UTL_REPORT::DESCR(I% + 5%) + " " + &
			UTL_REPORT::OPTTYPE(I% + 5%) + " " + &
			FORMAT$(UTL_REPORT::OPTLEN(I% + 5%), "<%>##") + " " + &
			UTL_REPORT::REQUIRE(I% + 5%) + "   " + &
			UTL_REPORT::VALID(I% + 5%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT I%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 5%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

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
