1	%TITLE "Payroll 941 Report"
	%SBTTL "PR_RPRT_941"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988, 1991 BY
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
	! ID:PR040
	!
	! Abstract:HELP
	!	.p
	!	The ^*941 Report\* option
	!	prints a report which provides the
	!	information needed to prepare a Form 941 - Employer's Quarterly
	!	Federal Tax Return. This report includes the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Year to Date Federal Earnings
	!	.le
	!	Year to Date FICA Earnings
	!	.le
	!	Year to Date Federal Tax
	!	.le
	!	Year to Date FICA Tax
	!	.le
	!	Quarter to Date Federal Earning
	!	.le
	!	Quarter to Date FICA Earning
	!	.le
	!	Quarter to Date Federal Tax
	!	.le
	!	Quarter to Date FICA Tax
	!	.els
	!
	! Index:
	!	.x Report>941
	!	.x 941 Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_941/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_941, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_941.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/14/90 - Aaron Redd
	!		Added line layout information so that the report can
	!		be sent to a spreadsheet.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	01/23/91 - Kevin Handy
	!		Removed code that is no longer necessary in trying
	!		to calculate federal/fica earnings.
	!
	!	02/01/91 - Kevin Handy
	!		Modified to show Taxable instead of Reportable
	!		amounts.
	!
	!	04/16/91 - Kevin Handy
	!		Modified to split OASDI and HI on totals.
	!		Reworked the code that determines when to put a '*'
	!		on the 'FICA Tax' fields.
	!
	!	06/03/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/13/91 - Kevin Handy
	!		Fixed to print total(10) instead of total(6)
	!		for employee HI earnings%. Some monor mods to
	!		correctly calculate HI QTD.
	!
	!	12/31/91 - Kevin Handy
	!		Modified to show reportable amounts instead of
	!		taxable amounts. (Reverse 02/01/91 change).
	!		Don't you wish people could make up their minds?
	!
	!	10/02/92 - Kevin Handy
	!		Changed "Number or employees" to "number of
	!		employees"
	!
	!	04/08/93 - Kevin Handy
	!		Adjust grand total by one character so it lines
	!		up with detail lines.
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/21/94 - Kevin Handy
	!		Rewrite FICA limit calculation so that negative
	!		values in a quarter will not become zero when
	!		they shouldn't.
	!
	!	07/14/94 - Kevin Handy
	!		Modified to be closer to 80 columns.
	!
	!	10/21/94 - Kevin Handy
	!		Added handling of deduction coded as "FD" in the
	!		W2 location field.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/27/95 - Kevin Handy
	!		Remove check for ::W2_1099 flag, so we don't get
	!		any magic happening.
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/12/97 - Kevin Handy
	!		Modified for FH code.
	!
	!	05/12/97 - Kevin Handy
	!		Use one more digit for fica rates
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	07/24/97 - Kevin Handy
	!		Allow the old FICA rates to work until we
	!		get everyone to switch over.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	06/21/98 - Kevin Handy
	!		Fix a bug with the FICA splitup, was only
	!		showing HI fica in QTD column.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/27/2000 - Kevin Handy
	!		Add code to handle an AEIC line 17.
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES) PR_REG_TAXES_CDD PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED) PR_REG_ERNDED_CDD PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP (PR_TAX_TABLE) PR_TAX_TABLE_CDD PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

	!
	! Dimensions
	!
	! EMP_TOTAL, GRAND_TOTAL
	!	1 - YTD Fed Earn
	!	2 - YTD FICA Earn (OASDI)
	!	3 - YTD Fed Tax
	!	4 - YTD FICA Tax (OASDI+HI)
	!	5 - QTD Fed Earn
	!	6 - QTD FICA Earn (OASDI)
	!	7 - QTD Fed Tax
	!	8 - QTD FICA Tax (OASDI + HI)
	!	9 - YTD FICA Earn (HI)
	!	10 - QTD FICA Earn (HI)
	!
	DIM EMP_TOTAL(10%), GRAND_TOTAL(10%)

	%PAGE

	!
	! Minimum value function to make reading thing easier
	!
	DEF FNMIN(A, B)

		IF (A < B)
		THEN
			FNMIN = A
		ELSE
			FNMIN = B
		END IF

	FNEND

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* field specifies the
	!	beginning item to include.
	!	The value must be in agreement with the ^*Sort By\* value
	!	entered in field (03).
	!	.p
	!	A blank in this field will cause the report to begin
	!	with the first item in the file.
	!
	! Index:
	!	.x From Item>941 Report
	!	.x 941 Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field the ending item for this report.
	!	The value
	!	must be in agreement with the ^*Sort By\* field (03).
	!	.p
	!	A blank field will cause the report to end with the last item
	!	in the file.
	!
	! Index:
	!	.x To Item>941 Report
	!	.x 941 Report>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU,SO,NA)\*
	!	.p
	!	The ^*Sort by\* field specifies the sort order for the report.
	!	.p
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	SO = Alphabetical (last name first)
	!	.le
	!	NA = Name
	!	.lm -10
	!	.p
	!	.els
	!	This field requires an entry and only the entries listed above
	!	are valid.
	!
	! Index:
	!	.x Sort>941 Report
	!	.x 941 Report>Sort
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

	CASE "DP"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::DEPT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::DEPT))

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))

	END SELECT

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Report Year (YYYY)\*
	!	.p
	!	The ^*Report Year\* field specifies the
	!	year for which this report is to print.
	!	.p
	!	The format for entry is YYYY.
	!
	! Index:
	!	.x Report Year>941 Report
	!	.x 941 Report>Report Year
	!
	!--


	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%))

	!++
	! Abstract:FLD05
	!	^*(05) Quarter\*
	!	.p
	!	The ^*Quarter\* field specifies the payroll
	!	quarter for which this report is to be printed.
	!	.p
	!	The field will accommodate a one digit entry and requires an entry.
	!
	! Index:
	!	.x Quarter>941 Report
	!	.x 941 Report>Quarter
	!
	!--

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Tax withholding register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

335	!
	! Open Tax earnings register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

340	!
	! Open Tax Table
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"

		FIND #PR_TAX_TABLE.CH%, KEY #0% EQ "F", REGARDLESS

		GET #PR_TAX_TABLE.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 5% OR ERR = 155%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT) / 10000.0
	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT) / 10000.0
	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT

	FICA_EMPR_PCT_HI = (PR_TAX_TABLE::FICA_EMPR_PCT_HI) / 10000.0
	FICA_EMPE_PCT_HI = (PR_TAX_TABLE::FICA_EMPE_PCT_HI) / 10000.0
	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI

	!
	! Give the additional digit when possible
	!
	IF FICA_EMPR_PCT > 0.100
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
		FICA_EMPR_PCT_HI = FICA_EMPR_PCT_HI / 10.0
		FICA_EMPE_PCT_HI = FICA_EMPE_PCT_HI / 10.0
	END IF

	CLOSE PR_TAX_TABLE.CH%

350	!
	! Load up all codes that have special meaning
	!
	DISABILITY_CODES$ = ""
	DISABILITY = 0.0
	AEIC_CODES$ = ""
	AEIC = 0.0

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"

		RESET #PR_ERNDED_DEF.CH%
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, REGARDLESS
	USE
		CONTINUE 370 IF ERR = 11%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	SELECT PR_ERNDED_DEF::W2LOCATION

	CASE "AE  "
		AEIC_CODES$ = AEIC_CODES$ + "," + &
			PR_ERNDED_DEF::ETYPE + "-" + &
			PR_ERNDED_DEF::CODE

	CASE "FD  "
		DISABILITY_CODES$ = DISABILITY_CODES$ + "," + &
			PR_ERNDED_DEF::ETYPE + "-" + &
			PR_ERNDED_DEF::CODE

	END SELECT

	GOTO 360

370	CLOSE PR_ERNDED_DEF.CH%

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "941 QUARTERLY FEDERAL REPORT"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = ""

	!
	! Define headings
	!
	TITLE$(4%) = "                                   ------------------" + &
		"Year to Date------------------!--------------Quarter " + &
		NUM1$(QTR%) + " to date---------------"
	TITLE$(5%) = "Emp          Name                     Fed Earn.   " + &
		"FICA Earn.   Fed. Tax   FICA Tax !   Fed Earn.   FICA " + &
		"Earn.   Fed. Tax   FICA Tax"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:034,VYTDFedErn:047," + &
		"VYTDFICAErn:060,VYTDFedTax:071,VYTDFICATax:082," + &
		"$YTDCorr:083,VQTDFedErn:096,VQTDFICAErn:109," + &
		"VQTDFedTax:120,VQTDFICATax:131,$QTDCorr:132"

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
		FILENAME$ = "PR_EMP_MASTER"
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
	CASE "DP"
		GOTO ExitTotal IF (PR_EMP_MASTER::DEPT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%

17040	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

17050	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 17060 IF PR_REG_TAXES::EMPNUM <> PR_EMP_MASTER::EMPNUM

	IF INSTR(1%, DISABILITY_CODES$, "P-" + PR_REG_TAXES::TTYPE)
	THEN
		DISABILITY = FUNC_ROUND(DISABILITY + &
			PR_REG_TAXES::TAX(QTR% - 1%), 2%)
	END IF

	IF INSTR(1%, AEIC_CODES$, "P-" + PR_REG_TAXES::TTYPE)
	THEN
		AEIC = FUNC_ROUND(AEIC + &
			PR_REG_TAXES::TAX(QTR% - 1%), 2%)
	END IF

	SELECT PR_REG_TAXES::TTYPE

	CASE "FW"
		EMP_TOTAL(1%) = FUNC_ROUND(EMP_TOTAL(1%) + &
			PR_REG_TAXES::REPORTABLE(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%
		EMP_TOTAL(3%) = FUNC_ROUND(EMP_TOTAL(3%) + &
			PR_REG_TAXES::TAX(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%

		EMP_TOTAL(5%) = EMP_TOTAL(5%) + &
			PR_REG_TAXES::REPORTABLE(QTR% - 1%)
		EMP_TOTAL(7%) = EMP_TOTAL(7%) + &
			PR_REG_TAXES::TAX(QTR% - 1%)

	CASE "FI"
		EMP_TOTAL(2%) = FUNC_ROUND(EMP_TOTAL(2%) + &
			PR_REG_TAXES::REPORTABLE(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%
		EMP_TOTAL(4%) = FUNC_ROUND(EMP_TOTAL(4%) + &
			PR_REG_TAXES::TAX(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%

		EMP_TOTAL(6%) = EMP_TOTAL(6%)  + &
			PR_REG_TAXES::REPORTABLE(QTR% - 1%)
		EMP_TOTAL(8%) = EMP_TOTAL(8%) + &
			PR_REG_TAXES::TAX(QTR% - 1%)

	CASE "FH"
		EMP_TOTAL(4%) = FUNC_ROUND(EMP_TOTAL(4%) + &
			PR_REG_TAXES::TAX(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%
		EMP_TOTAL(9%) = FUNC_ROUND(EMP_TOTAL(9%) + &
			PR_REG_TAXES::REPORTABLE(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%

		EMP_TOTAL(10%) = EMP_TOTAL(10%) + &
			PR_REG_TAXES::REPORTABLE(QTR% - 1%)
		EMP_TOTAL(8%) = EMP_TOTAL(8%) + &
			PR_REG_TAXES::TAX(QTR% - 1%)

	END SELECT

	GOTO 17050

17060	!
	! Look in pay file for any extra stuff
	!
	GOTO 17100 IF DISABILITY_CODES$ = "" AND AEIC_CODES$ = ""

	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

17070	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 17100 IF PR_REG_ERNDED::EMPNUM <> PR_EMP_MASTER::EMPNUM

	IF INSTR(1%, DISABILITY_CODES$, &
		PR_REG_ERNDED::ETYPE + "-" + PR_REG_ERNDED::CODE) <> 0%
	THEN
		DISABILITY = FUNC_ROUND(DISABILITY + &
			PR_REG_ERNDED::QTR_DOLL(QTR% - 1%), 2%)
	END IF

	IF INSTR(1%, AEIC_CODES$, &
		PR_REG_ERNDED::ETYPE + "-" + PR_REG_ERNDED::CODE) <> 0%
	THEN
		AEIC = FUNC_ROUND(AEIC + &
			PR_REG_ERNDED::QTR_DOLL(QTR% - 1%), 2%)
	END IF

	GOTO 17070

17100	!
	! Print total for employee
	!
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + EMP_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 10%

	IF SUM <> 0.0
	THEN
		!
		! Calculate FICA OASDI limits for this quarter
		!
		YD_FICA = FNMIN(EMP_TOTAL(2%), FICA_LIMIT)
		Y1_FICA = FNMIN(EMP_TOTAL(2%) - EMP_TOTAL(6%), FICA_LIMIT)

		EMP_TOTAL(2%) = YD_FICA
		EMP_TOTAL(6%) = YD_FICA - Y1_FICA

		!
		! Calculate FICA HI limits for this quarter
		!
		YD_FICA = FNMIN(EMP_TOTAL(9%), FICA_LIMIT_HI)
		Y1_FICA = FNMIN(EMP_TOTAL(9%) - EMP_TOTAL(10%), FICA_LIMIT_HI)

		EMP_TOTAL(9%) = YD_FICA
		EMP_TOTAL(10%) = YD_FICA - Y1_FICA

		!
		! Check fica calculations
		!
		YTD_CORR$, QTD_CORR$ = " "

		TEST = (EMP_TOTAL(2%) * FICA_EMPE_PCT) + &
			(EMP_TOTAL(9%) * FICA_EMPE_PCT_HI)
		YTD_CORR$ = "*" IF ABS(TEST - EMP_TOTAL(4%)) > .04

		TEST = (EMP_TOTAL(6%) * FICA_EMPE_PCT) + &
			(EMP_TOTAL(10%) * FICA_EMPE_PCT_HI)
		QTD_CORR$ = "*" IF ABS(TEST - EMP_TOTAL(8%)) > .04

		GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 10%

		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 23%) + " " + &
			FORMAT$(EMP_TOTAL(1%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(2%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(3%), "###,###.## ") + &
			FORMAT$(EMP_TOTAL(4%), "###,###.##") + &
			YTD_CORR$ + " " + &
			FORMAT$(EMP_TOTAL(5%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(6%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(7%), "###,###.## ") + &
			FORMAT$(EMP_TOTAL(8%), "###,###.##") + &
			QTD_CORR$

		EMP_COUNT% = EMP_COUNT% + 1%

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "         Grand Total               " + &
		FORMAT$(GRAND_TOTAL(1%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "###,###.## ") + &
		FORMAT$(GRAND_TOTAL(4%), "###,###.##  ") + &
		FORMAT$(GRAND_TOTAL(5%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(7%), "###,###.## ") + &
		FORMAT$(GRAND_TOTAL(8%), "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	EMPR = FUNC_ROUND(GRAND_TOTAL(6%) * FICA_EMPR_PCT, 2%)
	EMPE = FUNC_ROUND(GRAND_TOTAL(6%) * FICA_EMPE_PCT, 2%)

	EMPR_HI = FUNC_ROUND(GRAND_TOTAL(10%) * FICA_EMPR_PCT_HI, 2%)
	EMPE_HI = FUNC_ROUND(GRAND_TOTAL(10%) * FICA_EMPE_PCT_HI, 2%)

	TEXT$ = "     Employe(r) OASDI Tax liability = Total FICA " + &
		"Earnings X OASDI employe(r) percentage = " + &
		FORMAT$(GRAND_TOTAL(6%), "###,###,###.##") + &
		" X " + &
		FORMAT$(FICA_EMPR_PCT, "##.#####") + &
		" = " + &
		FORMAT$(EMPR, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "     Employe(r) HI Tax liability    = Total FICA " + &
		"Earnings X HI employe(r) percentage    = " + &
		FORMAT$(GRAND_TOTAL(10%), "###,###,###.##") + &
		" X " + &
		FORMAT$(FICA_EMPR_PCT_HI, "##.#####") + &
		" = " + &
		FORMAT$(EMPR_HI, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "     Total FICA tax liability       = Employe(r) " + &
		"portion                                = " + SPACE$(28%) + &
		FORMAT$(EMPR+EMPR_HI, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "     Employe(e) OASDI Tax liability = Total FICA " + &
		"Earnings X OASDI employe(e) percentage = " + &
		FORMAT$(GRAND_TOTAL(6%), "###,###,###.##") + &
		" X " + &
		FORMAT$(FICA_EMPE_PCT, "##.#####") + &
		" = " + &
		FORMAT$(EMPE, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "     Employe(e) HI Tax liability    = Total FICA " + &
		"Earnings X HI employe(e) percentage    = " + &
		FORMAT$(GRAND_TOTAL(10%), "###,###,###.##") + &
		" X " + &
		FORMAT$(FICA_EMPE_PCT_HI, "##.#####") + &
		" = " + &
		FORMAT$(EMPE_HI, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Total FICA tax liability       = Employe(e) " + &
		"portion                                = " + SPACE$(28%) + &
		FORMAT$(EMPE+EMPE_HI, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "     Total FICA tax liability       = Employe(r) " + &
		"portion + Employe(e) portion           = " + SPACE$(28%) + &
		FORMAT$(EMPR+EMPE+EMPR_HI+EMPE_HI, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"        1 a Number of employess             " + &
		FORMAT$(EMP_COUNT%, "#####"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"        2   Total wages and tips    " + &
		FORMAT$(GRAND_TOTAL(5%), "##,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"        3   Total income taxes      " + &
		FORMAT$(GRAND_TOTAL(7%), "##,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"        6 a Taxable ss wages        " + &
		FORMAT$(GRAND_TOTAL(6%), "##,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"        6 b Taxable tips reported   " + &
		FORMAT$(GRAND_TOTAL(0%), "##,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"        7   Taxable Medicare wages  " + &
		FORMAT$(GRAND_TOTAL(10%), "##,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"        9 a Disability wages        " + &
		FORMAT$(DISABILITY, "##,###,###.##"), 0%) &
		IF DISABILITY <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"       17 Adv Earned Inc Credit     " + &
		FORMAT$(AEIC, "##,###,###.##"), 0%) &
		IF AEIC <> 0.0

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
