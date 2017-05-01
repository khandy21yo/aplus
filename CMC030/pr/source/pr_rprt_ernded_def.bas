1	%TITLE "PR Pay/Ded Table Report"
	%SBTTL "PR_RPRT_ERNDED_DEF"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:PR061
	!
	! Abstract:HELP
	!	.p
	!	The ^*Pay/Accrue/Deduction Definition Table\* option
	!	accesses the routine which will print
	!	a listing of the user defined types of pay, accruals, and deductions. This
	!	list contians the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Type
	!	.le
	!	Code
	!	.le
	!	Description
	!	.le
	!	Credit Account
	!	.le
	!	Expense Account
	!	.le
	!	Sum
	!	.le
	!	Federal Withholding
	!	.le
	!	FIE
	!	.le
	!	FIR
	!	.le
	!	Federal Unemployement
	!	.le
	!	State Withholding
	!	.le
	!	State Unemployement
	!	.le
	!	Other State Tax
	!	.le
	!	CW
	!	.le
	!	DW
	!	.le
	!	EW
	!	.els
	!
	! Index:
	!	.x Report>Pay/Accrue/Deduction Definition Table
	!	.x Report>Pay Definition Table
	!	.x Report>Deduction Definition Table
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_ERNDED_DEF
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_ERNDED_DEF, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_ERNDED_DEF.OBJ;*
	!
	! Modification history:
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so the the report
	!		could be sent to either a spreadsheet or a DIF file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/12/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%PAGE

	ON ERROR GOTO 19000

	DEF TR$(XX1$, XX2$)
		XXTEMP$ = EDIT$(XX1$, -1%) + EDIT$(XX2$, -1%)

		SELECT XXTEMP$
		CASE "YY"
			YYTEMP$ = "TR"
		CASE "YN"
			YYTEMP$ = "T "
		CASE "NY"
			YYTEMP$ = " R"
		CASE ELSE
			YYTEMP$ = "  "
		END SELECT

		TR$ = YYTEMP$
	END DEF

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* field causes the
	!	report to print beginning with this particular
	!	item. A blank in this field will cause the report
	!	to begin with the first item in the file.
	!
	! Index:
	!	.x From Item>Pay/Accrue/Deduction Definition Table Report
	!	.x Pay/Accrue/Deduction Definition Table Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the
	!	report to end with this particular
	!	item. A blank field will cause the report to end with the
	!	last item in the file.
	!
	! Index:
	!	.x To Item>Pay/Accrue/Deduction Definition Table Report
	!	.x Pay/Accrue/Deduction Definition Table Report>To Item
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Pay/Ded Table Report"
	TITLE$(2%) = ""

	!
	! Column headings
	!
	TITLE$(3%) = "T Code Description                    " + &
		"Credit Account     Expense Account    Sum  FW  FIE  FIR  " + &
		"FUI  SW  SUI  OST  CW  DW  EW "
	TITLE$(4%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EType:001,$Code:005,$Descr:037,$CredAcct:056," + &
		"$ExpenseAcct:075,$Summary:078,$FW:083,$FIE:087,$FIR:092," + &
		"$FUI:097,$SW:102,$SUI:106,$OST:111,$CW:116,$DW:120,$EW:124"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_ERNDED_DEF.CH%, KEY #0%
		ELSE
			FIND #PR_ERNDED_DEF.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_ERNDED_DEF"
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
		GET #PR_ERNDED_DEF.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE > &
		TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Print out one line
	!
	TEXT$ = PR_ERNDED_DEF::ETYPE + "  " + &
		PR_ERNDED_DEF::CODE + "  " + &
		PR_ERNDED_DEF::DESCR + " " + &
		PR_ERNDED_DEF::DRCR_ACCT + " " + &
		PR_ERNDED_DEF::ACCRUAL_ACCT + "  " + &
		PR_ERNDED_DEF::SUMMARY + "   " + &
		TR$(PR_ERNDED_DEF::TAXABLE_FWH, PR_ERNDED_DEF::REPORTABLE_FWH) &
		+ "  " + &
		TR$(PR_ERNDED_DEF::TAXABLE_FIE, PR_ERNDED_DEF::REPORTABLE_FIE) &
		+ "   " + &
		TR$(PR_ERNDED_DEF::TAXABLE_FIR, PR_ERNDED_DEF::REPORTABLE_FIR) &
		+ "   " + &
		TR$(PR_ERNDED_DEF::TAXABLE_FUI, PR_ERNDED_DEF::REPORTABLE_FUI) &
		+ "   " + &
		TR$(PR_ERNDED_DEF::TAXABLE_SWH, PR_ERNDED_DEF::REPORTABLE_SWH) &
		+ "  " + &
		TR$(PR_ERNDED_DEF::TAXABLE_SUI, PR_ERNDED_DEF::REPORTABLE_SUI) &
		+ "   " + &
		TR$(PR_ERNDED_DEF::TAXABLE_OST, PR_ERNDED_DEF::REPORTABLE_OST) &
		+ "   " + &
		TR$(PR_ERNDED_DEF::TAXABLE_CWH, PR_ERNDED_DEF::REPORTABLE_CWH) &
		+ "  " + &
		TR$(PR_ERNDED_DEF::TAXABLE_DWH, PR_ERNDED_DEF::REPORTABLE_DWH) &
		+ "  " + &
		TR$(PR_ERNDED_DEF::TAXABLE_EWH, PR_ERNDED_DEF::REPORTABLE_EWH)

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
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
