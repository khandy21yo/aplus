1	%TITLE "Operations Table Report"
	%SBTTL "PR_RPRT_CURRENTOPER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1994 BY
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
	! ID:PR067
	!
	! Abstract:HELP
	!	.p
	!	The ^*Operations Table\* option in the Master List menu provides the
	!	means to access the routine which prints a list of the contents
	!	of the Operations Table. This list contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Operation
	!	.le
	!	Effective Date
	!	.le
	!	Unit Rate
	!	.le
	!	Hour Rate
	!	.le
	!	Unit
	!	.els
	!
	!
	! Index:
	!	.x Operations Table>Report
	!	.x Report>Operations Table
	!
	! Option:
	!
	! Author:
	!
	!	07/07/94 - Kevin Handy
	!		Taken from PR_RPRT_OPER.
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_CURRENTOPER
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_CURRENTOPER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_CURRENTOPER.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	11/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP	(PR_OPER)	PR_OPER_CDD	PR_OPER
	DECLARE PR_OPER_CDD LAST_OPER

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*Item\* entered in this field causes the printing
	!	to begin from a particular
	!	item.
	!	.p
	!	A blank in this field causes the report to begin with
	!	the first item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* entered in this field causes the report
	!	to print ending with a particular item.
	!	.p
	!	A blank field causes the report to end with the last item
	!	in the file.
	!
	! Index:
	!
	!--

	BASEDATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(2%))

	!++
	! Abstract:FLD03
	!	^*(03) Base Date\*
	!
	! Index:
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.OPN"
	USE
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Current Operations Table Report"
	TITLE$(2%) = "For " + PRNT_DATE(BASEDATE$, 8%)
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Operation     Effective Date          " + &
		"Unit Rate            Hour Rate        Units/Hr"
	TITLE$(4%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$Operation:008,DEffectDate:026,VUnitRate:047," + &
		"VHourlyRate:068,VUnitsPerHour:080"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_OPER.CH%, KEY #0%
		ELSE
			FIND #PR_OPER.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

	PRINT_FLAG% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_OPER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_OPER::OPER > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Dump out this one if the operation number changes
	!
	IF (PR_OPER::OPER <> LAST_OPER::OPER)
	THEN
		GOSUB PrintOperation
		LAST_OPER = PR_OPER
		PRINT_FLAG% = -1%
		GOTO GetNextRec
	END IF

	!
	! If this date is within the right range, then select it instead
	!
	IF (PR_OPER::EFFDATE <= BASEDATE$)
	THEN
		LAST_OPER = PR_OPER
		PRINT_FLAG% = -1%
	END IF

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!

	!
	! Print out the last operation
	!
	GOSUB PrintOperation

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

 PrintOperation:
	RETURN IF PRINT_FLAG% = 0%

	!
	! Calculate Units per hour
	!
	TEMP = 0.0
	TEMP = LAST_OPER::HOUR_RATE / LAST_OPER::PIECE_RATE &
		IF LAST_OPER::PIECE_RATE <> 0.0

	!
	! Print out one line
	!
	TEXT$ = LAST_OPER::OPER + "        " + &
		PRNT_DATE(LAST_OPER::EFFDATE, 8%) + "       " + &
		FORMAT$(LAST_OPER::PIECE_RATE, "#,###,###.####") + "     " + &
		FORMAT$(LAST_OPER::HOUR_RATE, "###,###,###.####") + &
		FORMAT$(TEMP, "###,###.####")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINT_FLAG% = 0%

	RETURN

	%PAGE

  HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
