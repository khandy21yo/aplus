1	%TITLE "Check Reconciliation Statement Report"
	%SBTTL "CK_RPRT_STATEMENT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:CK003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Check Reconciliation\* statement is used to
	!	reconcile the checks/deposits against the banks
	!	statement.
	!
	! Index:
	!	.x Report>Check Reconciliation
	!	.x Check Reconciliation>Report
	!	.x Statement>Check Reconciliation
	!	.x Check Reconciliation>Statement
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Author:
	!
	!	11/21/90 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_RPRT_STATEMENT/LINE
	!	$ LINK/EXECUTABLE=CK_EXE:*.EXE CK_RPRT_STATEMENT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_RPRT_STATEMENT.OBJ;*
	!
	! Modification history:
	!
	!	05/24/91 - Kevin Handy
	!		Modified to close GL_PERIOD file as soon as possible.
	!
	!	08/06/91 - Kevin Handy
	!		Fixed bug where "G" and "B" were both subtracted
	!		instead of handling them opposite.
	!
	!	11/06/91 - Kevin Handy
	!		Added "CUTOFF" period.
	!
	!	11/08/91 - Kevin Handy
	!		Fixed "Adjustments" so that it only looks at the
	!		GL side.  When it looks at both, the total is
	!		doubled.
	!
	!	12/10/92 - Kevin Handy
	!		Modified to cancle out adjustments that have
	!		been both GL'd and BANK'd.
	!
	!	03/16/93 - Kevin Handy
	!		Still getting complaints about adjustments.
	!
	!	06/16/93 - Kevin Handy
	!		Added REGARDLESS to GL_PERIOD.
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Lose extra parameter to ASSG_FREECHANNEL.
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	05/01/97 - Kevin Handy
	!		Clean up source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.HB"
	MAP	(CK_CKMNT)	CK_CKMNT_CDD	CK_CKMNT

	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.HB"
	MAP (CK_CONTROL)	CK_CONTROL_CDD		CK_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	ONLY_BANK$ = TRM$(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!	^*(01) Bank Code\*
	!	.b
	!	.lm +5
	!	The ^*Bank Code\* field enters the bank
	!	account code (set up in the Controlling File) which is to
	!	be printed in this report.
	!	.lm -5
	!
	! Index:
	!	.x Bank Code>Reconcilation Statement
	!	.x Reconciliation Statement>Bank Code
	!
	!--

	BANK_BALANCE = VAL(TRM$(UTL_REPORTX::OPTDEF(1%)))

	!++
	! Abstract:FLD02
	!	^*(02) Bank Balance\*
	!	.b
	!	.lm +5
	!	The ^*Bank Balance\* field enters the account balance
	!	as shown on the bank statement.
	!	.lm -5
	!
	! Index:
	!	.x Bank Balance>Reconciliation Statement
	!	.x Reconciliation Statement>Bank Balance
	!
	!--

	GL_BALANCE = VAL(TRM$(UTL_REPORTX::OPTDEF(2%)))

	!++
	! Abstract:FLD03
	!	^*(03) GL Balance\*
	!	.b
	!	.lm +5
	!	The ^*GL Balance\* field enters the balance of the account
	!	shown in the GL at the end of the cut off period being processed.
	!	.lm -5
	!
	! Index:
	!	.x GL Balance>Reconciliation Statement
	!	.x Reconciliation Statement>GL Balance
	!	.x General Ledger Balance>Reconciliation Statement
	!	.x Reconciliation Statement>General Ledger Balance
	!
	!--

	CUTOFF$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Cutoff Period\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Period\* field enters
	!	a cutoff period so a clean
	!	report can be printed even if future periods have been read.
	!	.b
	!	A blank setting will cause all items in the file to be
	!	printed.
	!	.lm -5
	!
	! Index:
	!	.x Cutoff>Reconciliation Statement
	!	.x Reconciliation Statement>Cutoff
	!
	!--


300	!
	! Reconcilation file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.OPN"
	USE
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

310	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

320	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.OPN"
		GET #CK_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "CK_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF CK_CONTROL::FLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "CK Purge in process", "ERR", &
			"CK_PURGE", "ERROR_PURGE")
		GOTO ExitProgram
	END IF

	CUR_PERIOD% = CK_CONTROL::PERIOD + 1%
	YEAR$ = CK_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Check Reconciliation Report"
	TITLE% = 2%

	IF ONLY_BANK$ <> ""
	THEN
		TITLE$(TITLE%) = "For Bank Code " + ONLY_BANK$
		TITLE% = TITLE% + 1%
	END IF

	TITLE$(TITLE%) = "Cutoff Period " + &
			YYYY_PP$ + " " + GL_PERIOD::PERIOD(CUR_PERIOD%)
	TITLE% = TITLE% + 1%

	TITLE$(TITLE%) = ""
	TITLE$(TITLE% + 1%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	!
	! Initilize Values
	!
	ADD_ADJUSTMENTS = 0.0
	LESS_ADJUSTMENTS = 0.0
	OUTSTANDING_CHECKS = 0.0
	OUTSTANDING_DEPOSITS = 0.0

	!
	! Search for first record
	!
	WHEN ERROR IN
		RESET #CK_CKMNT.CH%
	USE
		FILENAME$ = "CK_CKMNT"
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
		GET #CK_CKMNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check cutoff date
	!
	IF CUTOFF$ <> ""
	THEN
		GOTO 17020 IF (CK_CKMNT::GLDATE > CUTOFF$)
	END IF

	!
	! Check current record
	!
	GOTO 17020 IF (ONLY_BANK$ <> "") AND &
		(CK_CKMNT::BANK_ACCT <> ONLY_BANK$)

	!
	! Print out one line if necessary
	!
	SELECT CK_CKMNT::ETYPE

	CASE "A"
		IF CK_CKMNT::STYPE = "G"
		THEN
			IF CK_CKMNT::CKAMT < 0.0
			THEN
				ADD_ADJUSTMENTS = ADD_ADJUSTMENTS - &
					CK_CKMNT::CKAMT
			ELSE
				LESS_ADJUSTMENTS = LESS_ADJUSTMENTS - &
					CK_CKMNT::CKAMT
			END IF
		ELSE
			IF CK_CKMNT::CKAMT < 0.0
			THEN
				ADD_ADJUSTMENTS = ADD_ADJUSTMENTS + &
					CK_CKMNT::CKAMT
			ELSE
				LESS_ADJUSTMENTS = LESS_ADJUSTMENTS + &
					CK_CKMNT::CKAMT
			END IF
		END IF

	CASE  "C"

		IF CK_CKMNT::STYPE = "G"
		THEN
			OUTSTANDING_CHECKS = OUTSTANDING_CHECKS - &
				CK_CKMNT::CKAMT
		ELSE
			OUTSTANDING_CHECKS = OUTSTANDING_CHECKS + &
				CK_CKMNT::CKAMT
		END IF

	CASE "D"

		IF CK_CKMNT::STYPE = "G"
		THEN
			OUTSTANDING_DEPOSITS = OUTSTANDING_DEPOSITS - &
				CK_CKMNT::CKAMT
		ELSE
			OUTSTANDING_DEPOSITS = OUTSTANDING_DEPOSITS + &
				CK_CKMNT::CKAMT
		END IF

	END SELECT

	GOTO 17020

	%PAGE

	!*******************************************************************
	! Finish up report
	!*******************************************************************

 ExitTotal:

	!
	! Print Top Half
	!
	TEXT$ = "Balance Per Bank Statement            " + &
		FORMAT$(BANK_BALANCE, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "  Add: Deposits in Transit            " + &
		FORMAT$(OUTSTANDING_DEPOSITS, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "  Less: Outstanding Checks            " + &
		FORMAT$(OUTSTANDING_CHECKS, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "                                      --------------"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "  Adjusted Bank Statement Balance     " + &
		FORMAT$(BANK_BALANCE + OUTSTANDING_DEPOSITS + &
		OUTSTANDING_CHECKS, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "                                      =============="

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Print Bottom Half
	!
	TEXT$ = "Balance Per General Ledger            " + &
		FORMAT$(GL_BALANCE, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "  Add: Adjustments                    " + &
		FORMAT$(ADD_ADJUSTMENTS, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "  Less: Adjustments                   " + &
		FORMAT$(LESS_ADJUSTMENTS, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "                                      --------------"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)


	TEXT$ = "  Adjusted General Ledger Balance     " + &
		FORMAT$(GL_BALANCE + ADD_ADJUSTMENTS + &
		LESS_ADJUSTMENTS, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "                                      =============="

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

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
