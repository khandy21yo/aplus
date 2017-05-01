1	%TITLE "Payroll Tax Register Report"
	%SBTTL "PR_RPRT_QTR_TAX"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:PR017
	!
	! Abstract:HELP
	!	.p
	!	The ^*Tax Register report\* provides a listing of each employee, their pay,
	!	their taxable pay, when necessary the jurisdiction, and the computed taxes on
	!	it. The report may be started, stopped, and sorted to the desires of the user.
	!	The fields included are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Name
	!	.le
	!	Pay
	!	.le
	!	Non Comp Pay
	!	.le
	!	FICA Taxable
	!	.le
	!	FICA Tax
	!	.le
	!	Federal Taxable
	!	.le
	!	Federal Tax
	!	.le
	!	State Taxable
	!	.le
	!	State Tax
	!	.le
	!	Other State Tax Taxable
	!	.le
	!	Other State Tax
	!	.le
	!	SUI Taxable
	!	.le
	!	SUI Tax
	!	.le
	!	City Taxable
	!	.le
	!	City Tax
	!	.le
	!	County Taxable
	!	.le
	!	County Tax
	!	.els
	!
	! Index:
	!	.x Tax Register >Report
	!	.x Report>Tax Register
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_QTR_TAX/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_QTR_TAX, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_QTR_TAX.OBJ;*
	!
	! Author:
	!
	!	06/29/89 - Kevin Handy
	!
	! Modification history:
	!
	!	01/26/90 - Kevin Handy
	!		Fixed FICA flag so that it will turn off if the
	!		FICA is correct, and not be left on from one
	!		employee to the next.
	!
	!	01/31/90 - Kevin Handy
	!		Completely rewrote section that calculates wages/taxes.
	!		NOTE: The variable NON_COMP is never set up.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some  filename$ in error trapping.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_REG_ERNDED.CH and PR_ERNDED_DEF.CH
	!		from PR_FUNC_READTAXES.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_ERNDED_DEF file, which is no longer
	!		needed in this program.
	!
	!	07/14/91 - Kevin Handy
	!		Removed error trapping for 17110 which doesn't exist.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to look at "P" and "D" types only in
	!		PR_REG_ERNDED file (Ignore "A" types).
	!
	!	04/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/02/93 - Kevin Handy
	!		Added HI FICA percentage to the OASDI percentage
	!		so that there will be fewer *'s in the-report.
	!
	!	07/14/94 - Kevin Handy
	!		Formatted closer to 80 columns.
	!
	!	10/06/94 - Kevin Handy
	!		Modified to test page break on first line of
	!		employees print instead of letting it break
	!		between taxable and tax.
	!
	!	10/06/94 - Kevin Handy
	!		Made sure FICA code was blank in the totals as an
	!		attempt to fix a problem with KINGB getting two
	!		total lines for FICA.
	!
	!	04/06/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	04/11/95 - Kevin Handy
	!		Removed unsolicited_input stuff.
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Handle FH taxes.
	!
	!	05/12/97 - Kevin Handy
	!		Use one more digit for fica rates
	!		Use integer for #key
	!
	!	06/26/97 - Kevin Handy
	!		Reformat source code
	!
	!	06/27/97 - Kevin Handy
	!		Fix so it doesn't total fica into 'good' and
	!		'bad' in the grand total.
	!
	!	07/24/97 - Kevin Handy
	!		Allow old FICA rates to work until I can get
	!		everyone converted over.
	!
	!	05/29/98 - Kevin Handy
	!		Modified to handle new 'F' final deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	01/20/2000 - Kevin Handy
	!		Fix "FW/FH" mixup
	!
	!	12/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_STRUCTURE.INC"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE				UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP	(PR_REG_TAXES)		PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP	(PR_REG_ERNDED)		PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP	(PR_TAX_TABLE)		PR_TAX_TABLE_CDD	PR_TAX_TABLE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION PR_FUNC_READTAXES

	RECORD TOTAL_RECORD
		REAL	TAXABLE
		REAL	TAX
		STRING	CODE = 2%
		INTEGER EMP_COUNT
	END RECORD

	!
	! Dimension
	!
	DIM EMP_TOTAL%(10%), &
		TOTAL_TOTAL%(10%), &
		LOCAL_TOTAL%(10%)

	DIM PR_TAXES_STRUCT PR_TAXES(50%)

	DIM TOTAL_RECORD &
		EMP_TOTAL(10%, 10%), &
		LOCAL_TOTAL(10%, 30%), &
		TOTAL_TOTAL(10%, 30%)

	DECLARE TOTAL_RECORD BLANK_TOTAL

	BLANK_TOTAL::TAXABLE	= 0.0
	BLANK_TOTAL::TAX	= 0.0
	BLANK_TOTAL::CODE	= ""
	BLANK_TOTAL::EMP_COUNT	= 0%

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	QTR% = VAL%(UTL_REPORTX::OPTDEF(0%))
	!++
	!
	! Abstract:FLD01
	!	^*(01) Quarter\*
	!	.p
	!	The ^*Quarter\* field enters the calendar quarter for which
	!	this report will print.
	!	.p
	!	This field requires an entry. The field will accommodate a one (1) digit
	!	number.
	!
	! Index:
	!	.x Quarter>Tax Register Report
	!	.x Tax Register Report>Quarter
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) Year\*
	!	.p
	!	The ^*Year\* field enters the year for which this report
	!	is to print.
	!	.p
	!	The format for entry is YYYY and entry is required.
	!
	! Index:
	!	.x Year>Tax Register Report
	!	.x Tax Register Report>Year
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* setting causes the printing
	!	to begin with a particular item.
	!	.p
	!	A blank field will cause the report to start with the first item in the
	!	file.
	!
	! Index:
	!	.x From Item>Tax Register Report
	!	.x Tax Register Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	!
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* field causes the printing
	!	to end with a particular item.
	!	.p
	!	A blank field causes the report to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Tax Register Report
	!	.x Tax Register Report>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	!
	! Abstract:FLD05
	!	^*(05) Sort (NU,NA,LO,SO)\*
	!	.p
	!	The ^*Sort\* field enters a code which causes
	!	the report to be sorted in the indicated manner.
	!	.p
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU=Number
	!	.le
	!	NA=Name
	!	.le
	!	SO=Alphabetical (last name first)
	!	.le
	!	LO=Location
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field and only the above codes are valid.
	!
	! Index:
	!	.x Sort>Tax Register Report
	!	.x Tax Register Report>Sort
	!
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

340	!
	! Open TaxWH register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

350	!
	! Open ERNDED register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

360	!
	! Open Tax Table file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"

		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F", REGARDLESS
	USE
		CONTINUE 370 IF ERR = 5% OR ERR = 155%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT + &
		PR_TAX_TABLE::FICA_EMPR_PCT_HI) / 10000.0
	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT + &
		PR_TAX_TABLE::FICA_EMPE_PCT_HI) / 10000.0

	!
	! Allow for an additional digit in FICA rate
	!
	IF FICA_EMPR_PCT > 0.100
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
	END IF

	CLOSE PR_TAX_TABLE.CH%

370	!

	%PAGE

 ReportTitle:
	SELECT QTR%
	CASE 1%
		TEMP$ = "1st"
	CASE 2%
		TEMP$ = "2nd"
	CASE 3%
		TEMP$ = "3rd"
	CASE ELSE
		TEMP$ = NUM1$(QTR%) + "th"
	END SELECT

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Employee Quarterly Payroll Taxes Report"
	TITLE$(2%) = "For the " + TEMP$ + " Quarter of " + YYYY$

	SELECT SORTBY$
	CASE "NU"
		TITLE$(3%) = "By Employee Number"

	CASE "NA"
		TITLE$(3%) = "By Employee Name"

	CASE "SN"
		TITLE$(3%) = "By Soc. Sec. Number"

	CASE "LO"
		TITLE$(3%) = "By Location"

	CASE ELSE
		TITLE$(3%) = "By Alpha Sort"

	END SELECT

	TITLE$(4%) = ""

	TITLE$(5%) = "                                    Pay/          FICA" + &
		"     Federal        State          OST         SUI         " + &
		"City       County"

	TITLE$(6%) = "Emp #      Name                  NonComp   Taxable/Tax" + &
		" Taxable/Tax  Taxable/Tax  Taxable/Tax  Taxable/Tax  Taxable/Tax" + &
		"  Taxable/Tax"

	TITLE$(7%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_EMP_MASTER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	LOCAL_FLAG% = 0%

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

		GOSUB LocalTotal IF (PR_EMP_MASTER::LOCATION <> THIS_LOCATION$)

	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

17100	!*******************************************************************
	! Get tax detail information
	!*******************************************************************

	PAY, NON_COMP = 0.0
	EMP_TOTAL%(I%) = 0% FOR I% = 0% TO 10%

	X% = PR_FUNC_READTAXES(PR_EMP_MASTER::EMPNUM, &
		PR_REG_TAXES.CH%, &
		PR_TAXES%, &
		PR_TAXES())

	FOR LOOP% = 1% TO PR_TAXES%

		SELECT PR_TAXES(LOOP%)::TTYPE

		CASE "FI", "FH"
			ITEM% = 1%
		CASE "FW"
			ITEM% = 2%
		CASE "SW"
			ITEM% = 3%
		CASE "SX", "SI"
			ITEM% = 4%
		CASE "SU"
			ITEM% = 5%
		CASE "CW"
			ITEM% = 6%
		CASE "DW"
			ITEM% = 7%
		CASE ELSE
			ITEM% = 0%
		END SELECT

		IF ITEM%
		THEN
			EMP_TOTAL%(ITEM%) = EMP_TOTAL%(ITEM%) + 1%

			EMP_TOTAL(ITEM%, EMP_TOTAL%(ITEM%))::TAXABLE = &
				PR_TAXES(LOOP%)::TAXABLE(QTR% - 1%)
			EMP_TOTAL(ITEM%, EMP_TOTAL%(ITEM%))::TAX = &
				PR_TAXES(LOOP%)::TAX(QTR% - 1%)
			IF (ITEM% < 3%)
			THEN
				EMP_TOTAL(ITEM%, EMP_TOTAL%(ITEM%))::CODE = ""
			ELSE
				EMP_TOTAL(ITEM%, EMP_TOTAL%(ITEM%))::CODE = &
					PR_TAXES(LOOP%)::CODE
			END IF
			EMP_TOTAL(ITEM%, EMP_TOTAL%(ITEM%))::EMP_COUNT = 1%
		END IF

	NEXT LOOP%

17200	!*******************************************************************
	! Get tax detail information
	!*******************************************************************

	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17400
	END WHEN

17210	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 17400
	END WHEN

	GOTO 17400 IF (PR_EMP_MASTER::EMPNUM <> PR_REG_ERNDED::EMPNUM)

	GOTO 17210 IF PR_REG_ERNDED::ETYPE <> "P" AND &
		PR_REG_ERNDED::ETYPE <> "D" OR &
		PR_REG_ERNDED::ETYPE <> "F"

	!
	! Decide if we want to keep this one
	!
	PAY = FUNC_ROUND(PAY + PR_REG_ERNDED::QTR_DOLL(QTR% - 1%), 2%)

	GOTO 17210


17400	!*******************************************************************
	! Print out this employee
	!*******************************************************************

	IF (PAY <> 0.0) OR (NON_COMP <> 0.0)
	THEN
		EMP_TOTAL%(0%) = 1%

		EMP_TOTAL(0%, 1%)::TAXABLE = PAY
		EMP_TOTAL(0%, 1%)::TAX = NON_COMP
		EMP_TOTAL(0%, 1%)::CODE = ""
		EMP_TOTAL(0%, 1%)::EMP_COUNT = 1%
	ELSE
		EMP_TOTAL%(0%) = 0%
	END IF

	WORK_LOOP% = 0%
	WORK_LOOP% = EMP_TOTAL%(I%) &
		IF WORK_LOOP% < EMP_TOTAL%(I%) &
		FOR I% = 0% TO 10%

	GOTO 17450 IF WORK_LOOP% = 0%

	FOR LOOP% = 1% TO WORK_LOOP%
		TAX_TEXT$ = SPACE$(31%)

		IF LOOP% = 1%
		THEN
			!
			! Tax Basis
			!
			TAXABLE_TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
				LEFT(PR_EMP_MASTER::EMPNAME, 20%)

			FICA_TEST = FUNC_ROUND(EMP_TOTAL(1%, 1%)::TAXABLE * &
				FICA_EMPE_PCT, 2%)

			IF FICA_TEST - .02 > EMP_TOTAL(1%, 1%)::TAX OR &
				EMP_TOTAL(1%, 1%)::TAX > FICA_TEST + .02
			THEN
				EMP_TOTAL(1%, 1%)::CODE = "*"
			ELSE
				EMP_TOTAL(1%, 1%)::CODE = " "
			END IF
		ELSE
			TAXABLE_TEXT$ = SPACE$(31%)
		END IF

		FOR TAX_TYPE% = 0% TO 7%
			!
			! Taxes
			!
			IF EMP_TOTAL%(TAX_TYPE%) >= LOOP%
			THEN
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$( &
					EMP_TOTAL(TAX_TYPE%, LOOP%)::TAXABLE, &
					"######.##") + &
					EMP_TOTAL(TAX_TYPE%, LOOP%)::CODE + "  "

				TAX_TEXT$ = TAX_TEXT$ + &
					FORMAT$(EMP_TOTAL(TAX_TYPE%, LOOP%)::TAX, &
					"######.##") + &
					"    "
			ELSE
				TAX_TEXT$ = TAX_TEXT$ + "             "
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + "             "
			END IF

		NEXT TAX_TYPE%

		!
		! Print Taxes
		!
		LOCAL_FLAG% = -1%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			LEFT(TAXABLE_TEXT$, 132%), 3%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			LEFT(TAX_TEXT$, 132%), 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

17420	NEXT LOOP%

	!
	! Total codes
	!
	EMP_TOTAL(1%, 1%)::CODE = " "
	FOR TAX_TYPE% = 0% TO 7%

		FOR LOOP% = 1% TO EMP_TOTAL%(TAX_TYPE%)

			GOTO 17430 IF EMP_TOTAL(TAX_TYPE%, LOOP%)::CODE = &
				TOTAL_TOTAL(TAX_TYPE%, I%)::CODE &
				FOR I% = 1% TO TOTAL_TOTAL%(TAX_TYPE%)

			TOTAL_TOTAL%(TAX_TYPE%), I% = &
				TOTAL_TOTAL%(TAX_TYPE%) + 1%
			TOTAL_TOTAL(TAX_TYPE%, I%) = BLANK_TOTAL
			TOTAL_TOTAL(TAX_TYPE%, I%)::CODE = &
				EMP_TOTAL(TAX_TYPE%,LOOP%)::CODE

17430			TOTAL_TOTAL(TAX_TYPE%,I%)::TAXABLE = &
				TOTAL_TOTAL(TAX_TYPE%,I%)::TAXABLE + &
				EMP_TOTAL(TAX_TYPE%, LOOP%)::TAXABLE

			TOTAL_TOTAL(TAX_TYPE%,I%)::TAX = &
				TOTAL_TOTAL(TAX_TYPE%,I%)::TAX + &
				EMP_TOTAL(TAX_TYPE%, LOOP%)::TAX

			TOTAL_TOTAL(TAX_TYPE%, I%)::EMP_COUNT = &
				TOTAL_TOTAL(TAX_TYPE%, I%)::EMP_COUNT + 1% &
				IF EMP_TOTAL(TAX_TYPE%, LOOP%)::TAXABLE <> 0.0

		NEXT LOOP%

	NEXT TAX_TYPE%

	!
	! Total codes
	!
	FOR TAX_TYPE% = 0% TO 7%

		FOR LOOP% = 1% TO EMP_TOTAL%(TAX_TYPE%)

			GOTO 17440 IF EMP_TOTAL(TAX_TYPE%, LOOP%)::CODE = &
				LOCAL_TOTAL(TAX_TYPE%, I%)::CODE &
				FOR I% = 1% TO LOCAL_TOTAL%(TAX_TYPE%)

			LOCAL_TOTAL%(TAX_TYPE%), I% = &
				LOCAL_TOTAL%(TAX_TYPE%) + 1%
			LOCAL_TOTAL(TAX_TYPE%, I%) = BLANK_TOTAL
			LOCAL_TOTAL(TAX_TYPE%, I%)::CODE = &
				EMP_TOTAL(TAX_TYPE%, LOOP%)::CODE

17440			LOCAL_TOTAL(TAX_TYPE%,I%)::TAXABLE = &
				LOCAL_TOTAL(TAX_TYPE%, I%)::TAXABLE + &
				EMP_TOTAL(TAX_TYPE%, LOOP%)::TAXABLE

			LOCAL_TOTAL(TAX_TYPE%,I%)::TAX = &
				LOCAL_TOTAL(TAX_TYPE%, I%)::TAX + &
				EMP_TOTAL(TAX_TYPE%, LOOP%)::TAX

			LOCAL_TOTAL(TAX_TYPE%, I%)::EMP_COUNT = &
				LOCAL_TOTAL(TAX_TYPE%, I%)::EMP_COUNT + 1% &
				IF EMP_TOTAL(TAX_TYPE%, LOOP%)::TAXABLE <> 0.0

		NEXT LOOP%

	NEXT TAX_TYPE%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17450	!
	! Next employee
	!
	GOTO 17020

	%Page

 ExitTotal:
	GOSUB LocalTotal IF (SORTBY$ = "LO")

	!
	! Handle end of report
	!
	WORK_LOOP% = 1%

	WORK_LOOP%  = TOTAL_TOTAL%(LOOP%) &
		IF WORK_LOOP% < TOTAL_TOTAL%(LOOP%) &
		FOR LOOP% = 0% TO 7%

	FOR LOOP% = 1% TO WORK_LOOP%
		TAXABLE_TEXT$ = "Total Taxable                  "
		TAX_TEXT$     = "      Tax                      "
		COUNT_TEXT$   = "      Employee Count           "


		FOR TAX_TYPE% = 0% TO 7%
			IF TOTAL_TOTAL%(TAX_TYPE%) >= LOOP%
			THEN
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$( &
					TOTAL_TOTAL(TAX_TYPE%, LOOP%)::TAXABLE, &
					"######.##") + &
					LEFT(TOTAL_TOTAL(TAX_TYPE%, LOOP%)::CODE + &
					SPACE$(4%), 4%)

				TAX_TEXT$ = TAX_TEXT$ + &
					FORMAT$(TOTAL_TOTAL(TAX_TYPE%, LOOP%)::TAX, &
					"######.##")
				TAX_TEXT$ = TAX_TEXT$ + "    "
			ELSE
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + "             "
				TAX_TEXT$ = TAX_TEXT$ + "             "
			END IF
		NEXT TAX_TYPE%

		!
		! Print Taxes
		!
		FOR TAX_TYPE% = 0% TO 7%
			IF TOTAL_TOTAL(TAX_TYPE%,LOOP%)::EMP_COUNT <> 0%
			THEN
				COUNT_TEXT$ = COUNT_TEXT$ + &
					FORMAT$(TOTAL_TOTAL(TAX_TYPE%, LOOP%)::EMP_COUNT, &
					"#########    ")
			ELSE
				COUNT_TEXT$ = COUNT_TEXT$ + "             "
			END IF

		NEXT TAX_TYPE%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			LEFT(TAXABLE_TEXT$, 132%), 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			LEFT(TAX_TEXT$, 132%), 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			LEFT(COUNT_TEXT$, 132%), 0%)

	NEXT LOOP%

	!
	! Print federal deposit
	!
	FICA_EMPE = TOTAL_TOTAL(1%, 1%)::TAX
	FICA_EMPR = 0.0
	FICA_EMPR = FUNC_ROUND(TOTAL_TOTAL(1%, 1%)::TAX / FICA_EMPE_PCT * &
		FICA_EMPR_PCT, 2%) IF FICA_EMPE_PCT <> 0.0
	FED_TAX   = TOTAL_TOTAL(2%, 1%)::TAX

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 5%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Fica Employee       " + &
		FORMAT$(FICA_EMPE, "###,###.##"), 0%)

	IF FICA_EMPE_PCT <> 0.0
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Fica Employer       " + &
			FORMAT$(FICA_EMPR, "###,###.##"), 0%)
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Fica Employer            N/A  ", 0%)
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Federal Withholding " + &
		FORMAT$(FED_TAX, "###,###.##"), 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Total Tax Deposit   " + &
		FORMAT$(FICA_EMPE + FICA_EMPR + FED_TAX, &
		"###,###.##"), 0%)

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

 LocalTotal:
	GOTO 18590 IF LOCAL_FLAG% = 0%

	WORK_LOOP% = 1%

	WORK_LOOP%  = LOCAL_TOTAL%(LOOP%) &
		IF WORK_LOOP% < LOCAL_TOTAL%(LOOP%) &
		FOR LOOP% = 3% TO 7%

	FOR LOOP% = 1% TO WORK_LOOP%
		TAXABLE_TEXT$ = "Total Local Taxable            "
		TAX_TEXT$     = "      Local Tax                "
		COUNT_TEXT$   = "      Employee Count           "


		FOR TAX_TYPE% = 0% TO 7%
			IF LOCAL_TOTAL%(TAX_TYPE%) >= LOOP%
			THEN
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$( &
					LOCAL_TOTAL(TAX_TYPE%, LOOP%)::TAXABLE, &
					"######.##") + &
					LEFT(LOCAL_TOTAL(TAX_TYPE%, LOOP%)::CODE + &
					SPACE$(4%), 4%)

				TAX_TEXT$ = TAX_TEXT$ + &
					FORMAT$(LOCAL_TOTAL(TAX_TYPE%, LOOP%)::TAX, &
					"######.##")
				TAX_TEXT$ = TAX_TEXT$ + "    "
			ELSE
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + "             "
				TAX_TEXT$ = TAX_TEXT$ + "             "
			END IF
		NEXT TAX_TYPE%

		!
		! Print Taxes
		!
		FOR TAX_TYPE% = 0% TO 7%
			IF LOCAL_TOTAL(TAX_TYPE%, LOOP%)::EMP_COUNT <> 0%
			THEN
				COUNT_TEXT$ = COUNT_TEXT$ + &
					FORMAT$(LOCAL_TOTAL(TAX_TYPE%, LOOP%)::EMP_COUNT, &
					"#########    ")
			ELSE
				COUNT_TEXT$ = COUNT_TEXT$ + "             "
			END IF

		NEXT TAX_TYPE%

		IF EDIT$(TAXABLE_TEXT$, -1%) <> "" OR &
			EDIT$(TAX_TEXT$, -1%) <> "" OR &
			EDIT$(COUNT_TEXT$, -1%) <> ""
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				LEFT(TAXABLE_TEXT$, 132%), 0%)


			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				LEFT(TAX_TEXT$, 132%), 0%)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				LEFT(COUNT_TEXT$, 132%), 0%)

		END IF

	NEXT LOOP%

	TITLE$(3%) = "For Location " + PR_EMP_MASTER::LOCATION
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

18590	!
	! Set up for next one
	!
	LOCAL_FLAG% = 0%

	TITLE$(3%) = "For Location " + PR_EMP_MASTER::LOCATION
	THIS_LOCATION$ = PR_EMP_MASTER::LOCATION

	LOCAL_TOTAL(I%, J%) = BLANK_TOTAL FOR J% = 0% TO 30% FOR I% = 0% TO 10%
	LOCAL_TOTAL%(I%) = 0% FOR I% = 0% TO 10%

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
