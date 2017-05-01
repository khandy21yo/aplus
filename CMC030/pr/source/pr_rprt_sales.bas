1	%TITLE "Payroll Sales Report"
	%SBTTL "PR_RPRT_SALES"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! Computer Management Center, Inc..
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.p
	!	This program prints a payroll Sales report
	!
	! Index:
	!
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_SALES/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_SALES, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_SALES.OBJ;*
	!
	! Author:
	!
	!	02/24/89 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/24/2000 - Kevin Handy
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
	DECLARE	UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_SALES.HB"
	MAP	(PR_SALES)	PR_SALES_CDD		PR_SALES

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	FROM_DATE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)
	FROM_DATE$ = DATE_STOREDATE(FROM_DATE$)
	TO_DATE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)
	TO_DATE$ = DATE_STOREDATE(TO_DATE$)

	SELECT SORT_BY$
	CASE "L"
		SORT_KEY% = 0%
	CASE "D"
		SORT_KEY% = 1%
	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_SALES.OPN"
	USE
		FILENAME$ = "PR_SALES"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Sales Report"
	TITLE$(2%) = "From:  " + PRNT_DATE(FROM_DATE$, 8%) + &
		"  To:  " + PRNT_DATE(TO_DATE$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "Loc  Dept   SaleDate      Amount"
	TITLE$(5%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$Location:004,$Dept:011,DSaleDate:022,VSaleAmt:032"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_LOCATION$ = ""
		THEN
			RESET #PR_SALES.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PR_SALES.CH%, &
				KEY #SORT_KEY% GE FROM_LOCATION$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_SALES"
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
		GET #PR_SALES.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_SALES"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$
	CASE "L"
		GOTO ExitTotal IF (PR_SALES::LOCATION > TO_LOCATION$) AND &
			TO_LOCATION$ <> " "
	CASE "D"
		GOTO ExitTotal IF (PR_SALES::SALEDATE > TO_LOCATION$) AND &
			TO_LOCATION$ <> " "
	END SELECT

	!GOTO GetNextRec IF PR_SALES::SALEDATE < FROM_DATE$ OR &
	!	PR_SALES::SALEDATE > TO_DATE$

	!
	! Check and see if we print sale date totals.
	!
	GOSUB 18000 IF PR_SALES::SALEDATE <> TEST_DATE$ &
		AND FIRST_PASS% AND SORT_BY$ = "D"

	!
	! Check and see if we print department totals.
	!
	GOSUB 18100 IF PR_SALES::LOCATION + PR_SALES::DEPARTMENT <> &
		TEST_LOCATION$ + TEST_DEPT$ AND FIRST_PASS% AND &
		TEST_LOCATION$ <> " " AND SORT_BY$ = "L"

	!
	! Check and see if we print location totals.
	!
	GOSUB 18200 IF PR_SALES::LOCATION <> TEST_LOCATION$ &
		AND FIRST_PASS% AND SORT_BY$ = "L"

	TEXT$ = PR_SALES::LOCATION + " " + &
		PR_SALES::DEPARTMENT + " " + &
		PRNT_DATE(PR_SALES::SALEDATE, 8%) + " " + &
		FORMAT$(PR_SALES::AMOUNT, "######.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FIRST_PASS% = -1%

	GRAND_TOTAL = GRAND_TOTAL + PR_SALES::AMOUNT
	LOC_TOTAL = LOC_TOTAL + PR_SALES::AMOUNT
	DEPT_TOTAL = DEPT_TOTAL + PR_SALES::AMOUNT

	TEST_LOCATION$ = PR_SALES::LOCATION
	TEST_DEPT$ = PR_SALES::DEPARTMENT
	TEST_DATE$ = PR_SALES::SALEDATE

	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Check for subtotals
	!

	GOSUB 18000 IF SORT_BY$ = "D"
	GOSUB 18100 IF SORT_BY$ = "L"
	GOSUB 18200 IF SORT_BY$ = "L"
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 0%)

	TEXT$ = SPACE$(5%) + "Grand Total"

	TEXT$ = LEFT(TEXT$ + SPACE$(20%), 24%) + &
		FORMAT$(GRAND_TOTAL, "#####.##")

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

	%Page

18000	! Subtotal Checks

	TEXT$ = SPACE$(24%) + &
		FORMAT$(DEPT_TOTAL, "#####.##") + &
		SPACE$(5%) + "Date Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	DEPT_TOTAL= 0.0
	RETURN

18100	TEXT$ = SPACE$(24%) + &
		FORMAT$(DEPT_TOTAL, "#####.##") + &
		SPACE$(5%) + "Department Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	DEPT_TOTAL = 0.0
	RETURN

18200	TEXT$ = SPACE$(24%) + &
		FORMAT$(LOC_TOTAL, "#####.##") + &
		SPACE$(5%) + "Location Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	LOC_TOTAL = 0.0
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

