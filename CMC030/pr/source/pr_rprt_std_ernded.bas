1	%TITLE "PR Employee STD Ernded Dump"
	%SBTTL "PR_RPRT_STD_ERNDED"
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
	! ID:PR020
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Standard Payments/Deductions Report\* option
	!	prints a report which lists
	!	all Masterfile records for each employee in reference to standard
	!	accruals, payments and deductions.
	!	.b
	!	The column headings are:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Type (i.e. Accrual, Payment or Deduction)
	!	.te
	!	Code
	!	.te
	!	Rate
	!	.te
	!	Limit
	!	.te
	!	To Date
	!	.te
	!	Accrued
	!	.te
	!	End Date
	!	.te
	!	Frequency
	!	.te
	!	Method
	!	.te
	!	User [Memo]
	!	.end table
	!	The report is printed in employee number order, listing accruals
	!	first, then deductions, then payments.
	!
	! Index:
	!	.x Standard Payments/Deductions>Report
	!	.x Payments>Report
	!	.x Deductions>Report
	!	.x Report>Standard Payments/Deductions
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_STD_ERNDED
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_STD_ERNDED, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_STD_ERNDED.OBJ;*
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/10/96 - Kevin Handy
	!		Reformat source code.
	!		Added Wildcard deduction code.
	!		Ability to sort by Employee/ded, Deduction/emp.
	!
	!	06/11/96 - Kevin Handy
	!		Ability to sort by WC code.
	!		Added totals for W,D sorts.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	11/27/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED) PR_EMP_STD_ERNDED_CDD PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER) PR_EMP_MASTER_CDD PR_EMP_MASTER

	!
	! Map to sort by
	!
	!
	! File Layout for: PR.PR_EMP_STD_ERNDED
	!
	! Payroll Employee Standard ERNDED File
	!
	RECORD SORT_RECORD_CDD
		STRING SORT1 = 10%
		STRING SORT2 = 10%
		STRING SORT3 = 10%
		PR_EMP_STD_ERNDED_CDD STDERNDED
	END RECORD
	MAP (SORT_RECORD) SORT_RECORD_CDD SORT_RECORD

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Employee _#\*
	!	.b
	!	.lm +5
	!	The ^*From Employee _#\* field causes the
	!	printing to begin with a particular
	!	Employee Number.
	!	.b
	!	A blank field will cause it to
	!	start at the first record in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Employee Number>Standard Payments/Deductions Report
	!	.x Standard Payments/Deductions Report>From Employee Number
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Employee _#\*
	!	.b
	!	.lm +5
	!	The ^*To Employee _#\* field causes the
	!	printing to end with a particular
	!	Employee Number.
	!	.b
	!	A blank setting will cause it to end
	!	with the last record in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Employee Number>Standard Payments/Deductions Report
	!	.x Standard Payments/Deductions Report>To Employee Number
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort by	(E, D, W)\*
	!	.b
	!	.lm +5
	!	Specifies the order in which the report will
	!	print.
	!	.B
	!	Valid values are:
	!	.b
	!	.lm +5
	!	*E Sort by employee number, then deduction code.
	!	.br
	!	*D Sort by deduction code, then employee number.
	!	.br
	!	*W Sort by Workmans Comp Code, then Deduction code,
	!	then enployee number.
	!	.lm -5
	!	.lm -5
	!
	! Index:
	!	.x Sort By>Standard Payments/Deductions Report
	!	.x Standard Payments/Deductions Report>Sort By
	!
	!--

	WILD_DED$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Deduction\*
	!	.b
	!	.lm +5
	!	This field determines which deductions will be
	!	included.
	!	.b
	!	A blank setting will include
	!	all deductions.
	!	.lm -5
	!
	! Index:
	!	.x Deduction>Standard Payments/Deductions Report
	!	.x Standard Payments/Deductions Report>Deduction
	!
	!--

	K_NUM% = 0%

300	!
	! Open standard ernded file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.OPN"
	USE
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

310	CALL ASSG_CHANNEL(PR_SORT.CH%, STAT%)

	OPEN "PR_SORT.TMP" FOR OUTPUT AS FILE PR_SORT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		MAP SORT_RECORD, &
		PRIMARY KEY (SORT_RECORD::SORT1, SORT_RECORD::SORT2, &
			SORT_RECORD::SORT3), &
		ACCESS MODIFY, &
		ALLOW NONE

320	!
	! Open employee master file if necessary
	!
	IF SORT_BY$ = "W"
	THEN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	END IF

500	!
	! Sort the file as desired
	!
	WHEN ERROR IN
		RESET #PR_EMP_STD_ERNDED.CH%, KEY #K_NUM%
	USE
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

510	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 600 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	IF WILD_DED$ <> ""
	THEN
		GOTO 510 &
			IF COMP_STRING(PR_EMP_STD_ERNDED::CODE, WILD_DED$) = 0%
	END IF

	SELECT SORT_BY$
	CASE "D"
		GOTO 510 IF PR_EMP_STD_ERNDED::CODE < FROM_ITEM$
		GOTO 510 IF PR_EMP_STD_ERNDED::CODE > TO_ITEM$ &
			AND TO_ITEM$ <> ""
		SORT_RECORD::SORT1 = PR_EMP_STD_ERNDED::CODE
		SORT_RECORD::SORT2 = PR_EMP_STD_ERNDED::EMPNUM
		SORT_RECORD::SORT3 = ""

	CASE "W"
530		IF PR_EMP_MASTER::EMPNUM <> PR_EMP_STD_ERNDED::EMPNUM
		THEN
			WHEN ERROR IN
				GET #PR_EMP_MASTER.CH%, &
					KEY #0% EQ PR_EMP_STD_ERNDED::EMPNUM, &
					REGARDLESS
			USE
				PR_EMP_MASTER::EMPNUM = PR_EMP_STD_ERNDED::EMPNUM
				PR_EMP_MASTER::WC = ""

				CONTINUE 535
			END WHEN
		END IF

535		GOTO 510 IF PR_EMP_MASTER::WC < FROM_ITEM$
		GOTO 510 IF PR_EMP_MASTER::WC > TO_ITEM$ &
			AND TO_ITEM$ <> ""
		SORT_RECORD::SORT1 = PR_EMP_MASTER::WC
		SORT_RECORD::SORT2 = PR_EMP_STD_ERNDED::CODE
		SORT_RECORD::SORT3 = PR_EMP_STD_ERNDED::EMPNUM

	CASE ELSE
		GOTO 510 IF PR_EMP_STD_ERNDED::EMPNUM < FROM_ITEM$
		GOTO 600 IF PR_EMP_STD_ERNDED::EMPNUM > TO_ITEM$ &
			AND TO_ITEM$ <> ""
		SORT_RECORD::SORT1 = PR_EMP_STD_ERNDED::EMPNUM
		SORT_RECORD::SORT2 = PR_EMP_STD_ERNDED::CODE
		SORT_RECORD::SORT3 = ""

	END SELECT

590	SORT_RECORD::STDERNDED = PR_EMP_STD_ERNDED

	WHEN ERROR IN
		PUT #PR_SORT.CH%
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO 510

600	!
 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Standard Accruals/Payments/Deductions Report"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "EmployeeNum Type  Code        Rate       Limit  " + &
		"   ToDate      Accrued  EndDate       Freq  Meth  User"
	TITLE$(4%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$RType:014,$Code:021,VRate:034," + &
		"VLimit:046,VCurBal:058,VAccrued:070,DEndDate:082," + &
		"$Frequency:090,$Method:094,$UserDefined:114"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		RESET #PR_SORT.CH%, KEY #K_NUM%
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	LAST_WC$ = "~~~~~~~~~~~~"
	TOTAL_WC = 0.0
	COUNT_WC% = 0%

	LAST_CODE$ = "~~~"
	TOTAL_CODE = 0.0
	COUNT_CODE% = 0%

	TOTAL_GRAND = 0.0
	COUNT_GRAND% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_SORT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

	SELECT SORT_BY$
	CASE "W"
		IF LAST_DED$ <> SORT_RECORD::SORT2
		THEN
			GOSUB TotalDed
			LAST_DED$ = SORT_RECORD::SORT2
		END IF

		IF LAST_WC$ <> SORT_RECORD::SORT1
		THEN
			GOSUB TotalDed
			GOSUB TotalWC
			LAST_WC$ = SORT_RECORD::SORT1
			LAST_DED$ = SORT_RECORD::SORT2

			CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), &
				"", -2%)
			TEXT$ = "Workman Comp Code:  " + LAST_WC$
			CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), &
				TEXT$, 4%)
			CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), &
				"", 0%)
		END IF

	CASE "D"
		IF LAST_DED$ <> SORT_RECORD::SORT1
		THEN
			GOSUB TotalDed
			LAST_DED$ = SORT_RECORD::SORT1
		END IF

	END SELECT

	!
	! Print out one line
	!
	TEXT$ = SORT_RECORD::STDERNDED::EMPNUM + "   " + &
		SORT_RECORD::STDERNDED::RTYPE + "     " + &
		SORT_RECORD::STDERNDED::CODE + "   " + &
		FORMAT$(SORT_RECORD::STDERNDED::RATE, "##,###.###") + "  " + &
		FORMAT$(SORT_RECORD::STDERNDED::LIMIT, "###,###.##") + "  " + &
		FORMAT$(SORT_RECORD::STDERNDED::CTDBAL, "###,###.##") + "  " + &
		FORMAT$(SORT_RECORD::STDERNDED::ACCRUED, "###,###.##") + "  " + &
		PRNT_DATE(SORT_RECORD::STDERNDED::ENDDAT, 8%) + "  " + &
		SORT_RECORD::STDERNDED::FREQ + "   " + &
		SORT_RECORD::STDERNDED::METHOD + "    " + &
		SORT_RECORD::STDERNDED::USERDEF

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL_DED = FUNC_ROUND(TOTAL_DED + SORT_RECORD::STDERNDED::RATE, 3%)
	TOTAL_WC = FUNC_ROUND(TOTAL_WC + SORT_RECORD::STDERNDED::RATE, 3%)
	TOTAL_GRAND = FUNC_ROUND(TOTAL_GRAND + SORT_RECORD::STDERNDED::RATE, 3%)

	COUNT_DED% = COUNT_DED% + 1%
	COUNT_WC% = COUNT_WC% + 1%
	COUNT_GRAND% = COUNT_WC% + 1%

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!*******************************************************************
	! Print out deduction totals
	!*******************************************************************
 TotalDed:
	IF COUNT_DED% <> 0%
	THEN
		!
		! Print out one line
		!
		TEXT$ = "  Deduction Total  " + &
			LEFT(LAST_DED$, 2%) + "   " + &
			FORMAT$(TOTAL_DED, "##,###.###")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, -2%)
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", -2%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	COUNT_DED% = 0%
	TOTAL_DED = 0.0

	RETURN

	!*******************************************************************
	! Print total for WC code
	!*******************************************************************

 TotalWC:
	IF COUNT_WC% <> 0%
	THEN
		!
		! Print out one line
		!
		TEXT$ = "     WC Total      " + &
			"  " + "   " + &
			FORMAT$(TOTAL_WC, "##,###.###")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, -2%)
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", -2%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	COUNT_WC% = 0%
	TOTAL_WC = 0.0

	RETURN


	!*******************************************************************
	! Print Grans totals
	!*******************************************************************

 TotalGrand:
	IF COUNT_GRAND% <> 0%
	THEN
		!
		! Print out one line
		!
		TEXT$ = "  Grand Total      " + &
			"  " + "   " + &
			FORMAT$(TOTAL_GRAND, "##,###.###")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	COUNT_GRAND% = 0%
	TOTAL_GRAND = 0.0

	RETURN


 ExitTotal:
	!
	! Handle end of report
	!
	SELECT SORT_BY$
	CASE "W"
		GOSUB TotalDed
		GOSUB TotalWC
		GOSUB TotalGrand

	CASE "D"
		GOSUB TotalDed
		GOSUB TotalGrand
	END SELECT

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
