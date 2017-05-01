1	%TITLE "Payroll Net Check Audit Report"
	%SBTTL "PR_SPEC_MOVEBATCH"
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
	! ID:PR050
	!
	! Abstract:HELP
	!	.p
	!
	! Index:
	!	.x Report>Employee Check Audit
	!	.x Report>Employee Check Audit
	!	.x Employee Check Audit>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_MOVEBATCH/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_MOVEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_MOVEBATCH.OBJ;*
	!
	! Author:
	!
	!	04/25/2001 - Kevin Handy
	!
	! Modification history:
	!
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)

	!++
	! Abstract:FLD01
	!	^*(01) Start Payroll Date\*
	!	.p
	!	The ^*Start Payroll Date\* field specifies the date of
	!	the payroll folder with which the report will begin printing.
	!	A blank field will cause the report to start
	!	with the first payroll folder date in the file.
	!
	! Index:
	!	.x Start Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>Start Payroll Date
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD02
	!	^*(02) End Payroll Date\*
	!	.p
	!	The ^*End Payroll Date\* field specifies the date of
	!	the payroll folder with which the report is to end printing.
	!	A blank field will cause the report to end
	!	with the last payroll folder date in the file.
	!
	! Index:
	!	.x End Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>End Payroll Date
	!
	!--

	ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* field specifies the beginning item.
	!	The value must be in agreement with field
	!	(05).
	!	.p
	!	A blank field will cause the report to begin with the first item in
	!	the file.
	!
	! Index:
	!	.x From Item>Employee Check Audit Report
	!	.x Employee Check Audit Report>From Item
	!
	!--

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Move Batch"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" To: " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = "Batch: " + ITEM$
	TITLE$(4%) = ""

	TITLE$(6%) = ""

	!
	! Set up line layouts
	!
	LYT_LINE$ = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	!
	! Open up the source files
	!
	BATCH_NO$ = FROM_BATCH_NO$

	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.MOD"
		PR_TRN_PAY.OLD% = PR_TRN_PAY.CH%
		PR_TRN_PAY.CH% = 0%
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

17020	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.MOD"
		PR_TRN_DED.OLD% = PR_TRN_DED.CH%
		PR_TRN_DED.CH% = 0%
	USE
		PR_TRN_DED.CH% = 0%
		CONTINUE 17030 IF ERR = 5%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

17030	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.OPN"
		PR_TRN_CHECK.OLD% = PR_TRN_CHECK.CH%
		PR_TRN_CHECK.CH% = 0%
	USE
		PR_TRN_CHECK.CH% = 0%
		CONTINUE 17040 IF ERR = 5%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

17040	!*******************************************************************
	! Now open up the destination
	!*******************************************************************
	BATCH_NO$ = TO_BATCH_NO$

	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.CRE"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

17050	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.CRE"
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

17060	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.CRE"
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

17200	!*******************************************************************
	! Progess the PAY file
	!*******************************************************************

	TEXT$ = "Starting PAY folder"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)


	WHEN ERROR IN
		RESET #PR_TRN_PAY.OLD%
	USE
		CONTINUE 17300
	END WHEN

17210	!
	! Main loop starts here
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.OLD%
	USE
		CONTINUE 17300
	END WHEN

 !	CALL ENTR_3MESSAGE(SCOPE, PR_EMP_MASTER::EMPNUM, 1%)

	IF PR_TRN_PAY::BATCH = ITEM$
	THEN

		PUT #PR_TRN_PAY.CH%
		DELETE #PR_TRN_PAY.OLD%
		PR_PAY% = PR_PAY% + 1%
	END IF

	!
	! Get the next message
	!
	GOTO 17210

17300	!*******************************************************************
	! Start deduction file
	!*******************************************************************

	TEXT$ = "PR Pay Records Moved      " + &
		FORMAT$(PR_PAY%, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17400 IF PR_TRN_DED.OLD% = 0%

	TEXT$ = "Starting DED folder"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	WHEN ERROR IN
		RESET #PR_TRN_DED.OLD%
	USE
		CONTINUE 17400
	END WHEN

17310	!
	! Main loop starts here
	!
	WHEN ERROR IN
		GET #PR_TRN_DED.OLD%
	USE
		CONTINUE 17400
	END WHEN

 !	CALL ENTR_3MESSAGE(SCOPE, PR_EMP_MASTER::EMPNUM, 1%)

	IF PR_TRN_DED::BATCH = ITEM$
	THEN

		PUT #PR_TRN_DED.CH%
		DELETE #PR_TRN_DED.OLD%
		PR_DED% = PR_DED% + 1%
	END IF

	!
	! Get the next message
	!
	GOTO 17310

17400	!*******************************************************************
	! Start CHECK file
	!*******************************************************************

	TEXT$ = "PR Ded Records Moved      " + &
		FORMAT$(PR_DED%, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17500 IF PR_TRN_CHECK.OLD% = 0%

	TEXT$ = "Starting Check folder"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	WHEN ERROR IN
		RESET #PR_TRN_CHECK.OLD%
	USE
		CONTINUE 17500
	END WHEN

17410	!
	! Main loop starts here
	!
	WHEN ERROR IN
		GET #PR_TRN_CHECK.OLD%
	USE
		CONTINUE 17500
	END WHEN

 !	CALL ENTR_3MESSAGE(SCOPE, PR_EMP_MASTER::EMPNUM, 1%)

	IF PR_TRN_CHECK::BATCH = ITEM$
	THEN

		PUT #PR_TRN_CHECK.CH%
		DELETE #PR_TRN_CHECK.OLD%
		PR_CHECK% = PR_CHECK% + 1%
	END IF

	!
	! Get the next message
	!
	GOTO 17410

17500	!*******************************************************************
	! Start CHECK file
	!*******************************************************************

	TEXT$ = "PR CHECK Records Moved    " + &
		FORMAT$(PR_CHECK%, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
