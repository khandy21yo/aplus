1	%TITLE "Accounting Period"
	%SBTTL "UTL_RPRT_PERIOD"
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
	! ID:UT006
	!
	! Abstract:HELP
	!	.p
	!	The ^*Accounting Period\* option
	!	lists era information, including all applicable periods or cycles for
	!	each defined era.
	!	Report contains the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Era
	!	.le
	!	Year
	!	.le
	!	Cycle
	!	.le
	!	Description
	!	.le
	!	Status
	!	.le
	!	Beginning Date
	!	.le
	!	Ending Date
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Report>Accounting Periods
	!	.x Accounting Periods>Report
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_PERIOD/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_PERIOD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_PERIOD.OBJ;*
	!
	! AUTHOR:
	!
	!	01/18/88 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
	!
	!	06/16/93 - Kevin Handy
	!		Added REGARDLESS to UTL_PERIOD.
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
	!	11/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	!++
	! Abstract:COMMAND
	!	^*PROFILE/PERIOD\*
	!	.p
	!	^*Profile/Period\* prints accounting era
	!	information, including all applicable periods or cycles for
	!	each defined era.
	!	.p
	!	^*Format: PROFILE/PERIOD\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /PROFILE/PERIOD
	!	.end literal
	!
	! Index:
	!	.x PROFILE/PERIOD
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.HB"
	MAP (UTL_PERIOD) UTL_PERIOD_CDD UTL_PERIOD

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.HB"
	MAP (UTL_ERA) UTL_ERA_CDD UTL_ERA

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Era\*
	!	.p
	!	The ^*From Era\* field enters a selected Era from
	!	which the report will begin printing.
	!	.p
	!	A blank setting will cause the report to begin with the first
	!	Era code in the file.
	!
	! Index:
	!	.x From Era
	!	.x Era>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Era\*
	!	.p
	!	The ^*To Era\* field ends the
	!	printing with a selected Era.
	!	.p
	!	A blank setting will cause the report to end with the last
	!	Era code in the file.
	!
	! Index:
	!	.x To Era
	!	.x Era>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated Era's by entering a "Wildcard"
	!	value.
	!
	! Index:
	!	.x Wildcard
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.OPN"
	USE
		FILENAME$ = "UTL_PERIOD"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.OPN"
	USE
		FILENAME$ = "UTL_ERA"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "UTILITY  PERIOD  REPORT"
	TITLE$(2%) = "Utility system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890
	TITLE$(4%) = "Era  Description          Year  Cycle Description          Status" + &
		"     BegDate    EndDate     #Days"
	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #UTL_ERA.CH%
		ELSE
			FIND #UTL_ERA.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
	END WHEN

	DATE_F% = 1%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		RESET #UTL_PERIOD.CH%

		GET #UTL_ERA.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_PERIOD"
		CONTINUE HelpError
	END WHEN

17025	WHEN ERROR IN
		GET #UTL_PERIOD.CH%, REGARDLESS
	USE
		CONTINUE 17020
	END WHEN

	GOTO 17025 IF UTL_PERIOD::ERA <> UTL_ERA::ERA

	!
	! Check current record.
	!
17030	GOTO ExitTotal IF (UTL_ERA::ERA > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_STRING(EDIT$(UTL_ERA::ERA, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

17300	!
	! Print out one line
	!
	IF UTL_ERA::ERA<>STORE_CHECK$ AND STORE_CHECK$ <> ""
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		DATE_F% = 1%
	END IF

	IF UTL_ERA::ERA = ERA$
	THEN
		UTL_ERA::DESCRIPTION = ""
	END IF

	IF DATE_F% = 1%
	THEN
		DATE_F% = 0%
		FIRST_DATE$ = UTL_ERA::BEG_DATE
	ELSE
		FIRST_DATE$ = DATE_INVDCODE(DATE_DAYCODE(LAST_DATE$) + 1%)
	END IF

	DAYS = DATE_DAYCODE(UTL_PERIOD::END_DATE) - &
		DATE_DAYCODE(FIRST_DATE$) + 1%

	TEXT$ = UTL_ERA::ERA + "   " + &
		UTL_ERA::DESCRIPTION + " " + &
		UTL_PERIOD::YEAR + "  " + &
		UTL_PERIOD::CYCLE + "    " + &
		UTL_PERIOD::DESCRIPTION + " " + &
		UTL_PERIOD::PERIOD_STATUS + "          " + &
		PRNT_DATE(FIRST_DATE$, 8%) + " " + &
		PRNT_DATE(UTL_PERIOD::END_DATE, 8%) + " " + &
		FORMAT$(DAYS, "##,###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT
	STORE_CHECK$ = UTL_PERIOD::ERA
	LAST_DATE$ = UTL_PERIOD::END_DATE
	ERA$ = UTL_ERA::ERA

17350	!
	! Try for next record
	!
	GOTO 17025

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
