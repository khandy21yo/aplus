1	%TITLE "TOTAL - Total Accrual Report"
	%SBTTL "PR_RPRT_ACCRUAL_TOTAL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:PR060
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Prints the current balances for the employee accruals.
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	03/30/92 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_ACCRUAL_TOTAL
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_ACCRUAL_TOTAL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_ACCRUAL_TOTAL.OBJ;*
	!
	! Modification history:
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/04/92 - Kevin Handy
	!		Declared FUNC_ROUND as an external function.
	!
	!	12/30/92 - Kevin Handy
	!		Fixed bug in printing by location, where it would
	!		lock up on the first subtotal.
	!
	!	12/30/92 - Kevin Handy
	!		Added depaartment and hire date to report.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/28/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP	(PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	!
	! Dimension Statements
	!
	DIM SUBTOTAL(9%), DEPTTOTAL(9%), TOTAL(9%)

	%PAGE

	ON ERROR GOTO 19000


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
	!	report to begin with a selected item. The item must be in agreement
	!	with the specific type of sort selected, i.e., if the selection is
	!	made to print the report in employee number order, a value in this
	!	field must be an employee number; or if the selection is made to
	!	print the report in alphabetical order, the value in this field
	!	must be a name, last name first.
	!	.p
	!	Refer to field (03) Order of the Accrual Report report
	!	setting screen for more information on sort choices.
	!
	! Index:
	!	.x From Item>Accrual Report
	!	.x Accrual Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field specifies an ending item for this report.
	!	A blank field will cause the report
	!	to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Accrual Report
	!	.x Accrual Report>To Item
	!
	!--
	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU,NA,SN,LO,SO)\*
	!	.p
	!	The ^*Sort\* field specifies the order in
	!	which the report will print.
	!	.p
	!	The valid values and related sort orders are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	SN = Social Security Number
	!	.le
	!	LO = Location, Department, Work Center, Alpha
	!	.le
	!	SO = Alphabetical (last name first)
	!	.els
	!	.lm -10
	!	.p
	!	This field requires an entry. Only the codes listed above are
	!	valid.
	!
	! Index:
	!	.x Sort>Accrual Report
	!	.x Accrual Report>Sort
	!
	!--

	ACCCODE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Accrual Code\*
	!
	! Index:
	!
	! Datatype:TEXT
	! Size:1
	! Valid Input: L,S,l,s
	!--

	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))

	CASE "SN"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SSN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SSN))

	CASE "LO"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::LOCATION))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::LOCATION))

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))

	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

330	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
	USE
		FILENAME$ = "PR_EMP_ACCRUAL"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "EMPLOYEE ACCRUAL REPORT"
	TITLE$(2%) = "Payroll System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "                                            " + &
		"                 ---- Unavailable ----  " + &
		"  ---- Available ----      ---- Total ----"

	TITLE$(5%) = "Emp Num   Employee Name                     " + &
		"Dept HireDate   Hours  Days  Dollars   " + &
		" Hours  Days  Dollars    Hours  Days  Dollars"

	TITLE$(6%) = "."

	LYT_LINE$ = "$EMPNUM:11,$EMPNAM:42,FUNHRS:63,FUNDAY:73,FUNDOL:83," + &
		"FAVHRS:93,FAVDAY:103,FAVDOL:113,FTTHRS:123,FTTDAY:133,FTTDOL:143"
	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, &
				KEY #K_NUM%
		ELSE
			FIND #PR_EMP_MASTER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	SUBITEM$ = "--------------------"

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17025	WHEN ERROR IN
		GET #PR_EMP_ACCRUAL.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + ACCCODE$, &
			REGARDLESS
	USE
		CONTINUE 17020
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "NU"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "NA"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "SN"
		GOTO ExitTotal IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "LO"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		IF DEPT$ <> PR_EMP_MASTER::DEPT
		THEN
			SUBTITLE$ = "Department " + DEPT$ + " Subtotal"
			GOSUB Depttotal
			DEPT$ = PR_EMP_MASTER::DEPT
		END IF

		IF SUBITEM$ <> PR_EMP_MASTER::LOCATION
		THEN
			SUBTITLE$ = "Department " + DEPT$ + " Subtotal"
			GOSUB Depttotal
			DEPT$ = PR_EMP_MASTER::DEPT
			SUBTITLE$ = "Location " + SUBITEM$ + " Subtotal"
			GOSUB Subtotal
			SUBITEM$ = PR_EMP_MASTER::LOCATION
		END IF

	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	!
	! Print out one line
	!
	UNDAYS = FUNC_ROUND(PR_EMP_ACCRUAL::HOURSUNA / 8.0, 2%)
	AVDAYS = FUNC_ROUND(PR_EMP_ACCRUAL::HOURSAVA / 8.0, 2%)

	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + &
		PR_EMP_MASTER::DEPT + " " + &
		PRNT_DATE(PR_EMP_MASTER::HIREDAY, 8%) + " " + &
		FORMAT$(PR_EMP_ACCRUAL::HOURSUNA, "######.#") + &
		FORMAT$(UNDAYS, "#####.#") + &
		FORMAT$(PR_EMP_ACCRUAL::DOLLARUNA, "######.##") + &
		FORMAT$(PR_EMP_ACCRUAL::HOURSAVA, "######.#") + &
		FORMAT$(AVDAYS, "#####.#") + &
		FORMAT$(PR_EMP_ACCRUAL::DOLLARAVA, "######.##") + &
		FORMAT$(PR_EMP_ACCRUAL::HOURSUNA + PR_EMP_ACCRUAL::HOURSAVA, &
			"######.#") + &
		FORMAT$(UNDAYS + AVDAYS, "#####.#") + &
		FORMAT$(PR_EMP_ACCRUAL::DOLLARUNA + PR_EMP_ACCRUAL::DOLLARAVA, &
			"######.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Subtotal
	!
	SUBTOTAL(1%) = SUBTOTAL(1%) + PR_EMP_ACCRUAL::HOURSUNA
	SUBTOTAL(2%) = SUBTOTAL(2%) + UNDAYS
	SUBTOTAL(3%) = SUBTOTAL(3%) + PR_EMP_ACCRUAL::DOLLARUNA
	SUBTOTAL(4%) = SUBTOTAL(4%) + PR_EMP_ACCRUAL::HOURSAVA
	SUBTOTAL(5%) = SUBTOTAL(5%) + AVDAYS
	SUBTOTAL(6%) = SUBTOTAL(6%) + PR_EMP_ACCRUAL::DOLLARAVA
	SUBTOTAL(7%) = SUBTOTAL(7%) + &
		PR_EMP_ACCRUAL::HOURSUNA + PR_EMP_ACCRUAL::HOURSAVA
	SUBTOTAL(8%) = SUBTOTAL(8%) + UNDAYS + AVDAYS
	SUBTOTAL(9%) = SUBTOTAL(9%) + &
		PR_EMP_ACCRUAL::DOLLARUNA + PR_EMP_ACCRUAL::DOLLARAVA

	!
	! Subtotal
	!
	DEPTTOTAL(1%) = DEPTTOTAL(1%) + PR_EMP_ACCRUAL::HOURSUNA
	DEPTTOTAL(2%) = DEPTTOTAL(2%) + UNDAYS
	DEPTTOTAL(3%) = DEPTTOTAL(3%) + PR_EMP_ACCRUAL::DOLLARUNA
	DEPTTOTAL(4%) = DEPTTOTAL(4%) + PR_EMP_ACCRUAL::HOURSAVA
	DEPTTOTAL(5%) = DEPTTOTAL(5%) + AVDAYS
	DEPTTOTAL(6%) = DEPTTOTAL(6%) + PR_EMP_ACCRUAL::DOLLARAVA
	DEPTTOTAL(7%) = DEPTTOTAL(7%) + &
		PR_EMP_ACCRUAL::HOURSUNA + PR_EMP_ACCRUAL::HOURSAVA
	DEPTTOTAL(8%) = DEPTTOTAL(8%) + UNDAYS + AVDAYS
	DEPTTOTAL(9%) = DEPTTOTAL(9%) + &
		PR_EMP_ACCRUAL::DOLLARUNA + PR_EMP_ACCRUAL::DOLLARAVA

	!
	! Total
	!
	TOTAL(1%) = TOTAL(1%) + PR_EMP_ACCRUAL::HOURSUNA
	TOTAL(2%) = TOTAL(2%) + UNDAYS
	TOTAL(3%) = TOTAL(3%) + PR_EMP_ACCRUAL::DOLLARUNA
	TOTAL(4%) = TOTAL(4%) + PR_EMP_ACCRUAL::HOURSAVA
	TOTAL(5%) = TOTAL(5%) + AVDAYS
	TOTAL(6%) = TOTAL(6%) + PR_EMP_ACCRUAL::DOLLARAVA
	TOTAL(7%) = TOTAL(7%) + &
		PR_EMP_ACCRUAL::HOURSUNA + PR_EMP_ACCRUAL::HOURSAVA
	TOTAL(8%) = TOTAL(8%) + UNDAYS + AVDAYS
	TOTAL(9%) = TOTAL(9%) + &
		PR_EMP_ACCRUAL::DOLLARUNA + PR_EMP_ACCRUAL::DOLLARAVA

	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "LO"
		SUBTITLE$ = "Department " + DEPT$ + " Subtotal"
		GOSUB Depttotal
		DEPT$ = PR_EMP_MASTER::DEPT
		SUBTITLE$ = "Location " + SUBITEM$ + " Subtotal"
		GOSUB Subtotal
		SUBITEM$ = PR_EMP_MASTER::LOCATION

	END SELECT

	GOSUB Total

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

 Subtotal:
	!*******************************************************************
	! Print subtotals
	!*******************************************************************

	I% = 0%
	I% = -1% IF SUBTOTAL(J%) <> 0.0 FOR J% = 1% TO 9%

	IF I%
	THEN
		TEXT$ = "          " + " " + &
			FORMAT$(SUBTITLE$, "'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL") + &
			FORMAT$(SUBTOTAL(1%), "######.#") + &
			FORMAT$(SUBTOTAL(2%), "#####.#") + &
			FORMAT$(SUBTOTAL(3%), "######.##") + &
			FORMAT$(SUBTOTAL(4%), "######.#") + &
			FORMAT$(SUBTOTAL(5%), "#####.#") + &
			FORMAT$(SUBTOTAL(6%), "######.##") + &
			FORMAT$(SUBTOTAL(7%), "######.#") + &
			FORMAT$(SUBTOTAL(8%), "#####.#") + &
			FORMAT$(SUBTOTAL(9%), "######.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	SUBTOTAL(I%) = 0.0 FOR I% = 1% TO 9%

	RETURN

 Depttotal:
	!*******************************************************************
	! Print subtotals
	!*******************************************************************

	I% = 0%
	I% = -1% IF DEPTTOTAL(J%) <> 0.0 FOR J% = 1% TO 9%

	IF I%
	THEN
		TEXT$ = "          " + " " + &
			FORMAT$(DEPTTITLE$, "'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL") + &
			FORMAT$(DEPTTOTAL(1%), "######.#") + &
			FORMAT$(DEPTTOTAL(2%), "#####.#") + &
			FORMAT$(DEPTTOTAL(3%), "######.##") + &
			FORMAT$(DEPTTOTAL(4%), "######.#") + &
			FORMAT$(DEPTTOTAL(5%), "#####.#") + &
			FORMAT$(DEPTTOTAL(6%), "######.##") + &
			FORMAT$(DEPTTOTAL(7%), "######.#") + &
			FORMAT$(DEPTTOTAL(8%), "#####.#") + &
			FORMAT$(DEPTTOTAL(9%), "######.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	DEPTTOTAL(I%) = 0.0 FOR I% = 1% TO 9%

	RETURN

 Total:
	!*******************************************************************
	! Print subtotals
	!*******************************************************************

	TEXT$ = "          " + " " + &
		"Grand Total                                   " + &
		FORMAT$(TOTAL(1%), "######.#") + &
		FORMAT$(TOTAL(2%), "#####.#") + &
		FORMAT$(TOTAL(3%), "######.##") + &
		FORMAT$(TOTAL(4%), "######.#") + &
		FORMAT$(TOTAL(5%), "#####.#") + &
		FORMAT$(TOTAL(6%), "######.##") + &
		FORMAT$(TOTAL(7%), "######.#") + &
		FORMAT$(TOTAL(8%), "#####.#") + &
		FORMAT$(TOTAL(9%), "######.##")

	TOTAL(I%) = 0.0 FOR I% = 1% TO 9%

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	RETURN

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
