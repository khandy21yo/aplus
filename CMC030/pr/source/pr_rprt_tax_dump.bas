1	%TITLE "Payroll Tax Register Dump"
	%SBTTL "PR_RPRT_TAX_DUMP"
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
	! ID:PR044
	!
	! Abstract:HELP
	!	.p
	!	The ^*Tax Ledger Dump\* option
	!	prints a dump of
	!	all the data related to payroll taxes. This report includes the following
	!	fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Name
	!	.le
	!	Withholding Type
	!	.le
	!	Year to Date Wages
	!	.le
	!	Year to Date Taxes
	!	.le
	!	Year to Date Weeks Worked
	!	.le
	!	Quarter to Date Wages
	!	.le
	!	Quarter to Date Taxes
	!	.le
	!	Quarter to Date Weeks Worked
	!	.els
	!
	! Index:
	!	.x Tax Ledger Dump>Report
	!	.x Report>Tax Ledger Dump
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TAX_DUMP/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TAX_DUMP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TAX_DUMP.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	02/17/89 - Kevin Handy
	!		Modified to print totals at the bottom.
	!
	!	03/27/89 - Kevin Handy
	!		Fixed bug where it used CHW instead of CWH for
	!		the city tax table.
	!
	!	04/03/89 - Kevin Handy
	!		Added SI Tax type.
	!
	!	09/19/89 - Kevin Handy
	!		Fixed problem where variable names were being
	!		mixed (PRETBL_TYPE_TABLE$ and SUBJECT_TYPE_TABLE$).
	!
	!	01/31/90 - Kevin Handy
	!		Completely rewrote section that calculates
	!		wages/taxes.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some filenames in the error trapping.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_ERNDED_DEF.CH and PR_REG_ERNDED.CH
	!		from PR_FUNC_READTAXES.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_ERNDED_DEF file which is no longer
	!		used in this program.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_REG_ERNDED file which is no longer
	!		used in this program.
	!
	!	05/15/92 - Kevin Handy
	!		Modified to use PRNT_NUMBERITH function.
	!
	!	05/15/92 - Kevin Handy
	!		Fixed so only prints YTD up to current quarter.
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	11/22/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:PR_STRUCTURE.INC"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES) PR_REG_TAXES_CDD PR_REG_TAXES

	!
	! Define record structures
	!
	RECORD TOTAL_STRUCTURE
		STRING CODE = 4
		REAL YTDWAGES
		REAL YTDTAXES
		REAL YTDWEEKS
		REAL QTDWAGES
		REAL QTDTAXES
		REAL QTDWEEKS
	END RECORD

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION PR_FUNC_READTAXES

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	!
	! Dimension arrays and tables
	!
	DIM PR_TAXES_STRUCT PR_TAXES(50%)
	DIM TOTAL_STRUCTURE TOTALS(100%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* field causes the printing
	!	to begin with a particular item.
	!	The value must be in agreement with field
	!	(03).
	!	.p
	!	A blank will cause the report to begin with the first item in
	!	the file.
	!
	! Index:
	!	.x From Item>Tax Ledger Dump
	!	.x Tax Ledger Dump>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the printing
	!	to end with a particular item.
	!	The value must be in agreement with field
	!	(03).
	!	.p
	!	A blank field will cause the report to end with the last item
	!	in the file.
	!
	! Index:
	!	.x To Item>Tax Ledger Dump
	!	.x Tax Ledger Dump>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU, SN, NA, SO)\*
	!	.p
	!	The ^*Sort\* field enters a code which
	!	causes the report to be sorted in the indicated manner.
	!	.p
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	SN = Social Security Number
	!	.le
	!	NA = Name
	!	.le
	!	SO = Alphabetical (last name first)
	!	.lm -10
	!	.p
	!	.els
	!	An entry is required in this field and only the above codes
	!	are valid.
	!
	! Index:
	!	.x Sort>Tax Ledger Dump
	!	.x Tax Ledger Dump>Sort
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
	!	^*(04) Year (YYYY)\*
	!	.p
	!	The ^*Year\* field enters the year for which
	!	this report is to be printed.
	!	.p
	!	This field requires and entry. The format for entry is YYYY.
	!
	! Index:
	!	.x Year>Tax Ledger Dump
	!	.x Tax Ledger Dump>Year
	!
	!--


	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%))

	!++
	! Abstract:FLD05
	!	^*(05) Quarter\*
	!	.p
	!	The ^*Quarter\* field enters the payroll
	!	quarter for which this report will print.
	!	.p
	!	This field requires an entry. The field will accommodate
	!	a one digit number.
	!
	! Index:
	!	.x Quarter>Tax Ledger Dump
	!	.x Tax Ledger Dump>Quarter
	!
	!--


	!TAX_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Tax Type\*
	!	.p
	!	The ^*Wildcard Tax Type\* enters the specific tax(es) to
	!	which earnings may be subject and any consequential taxes withheld from
	!	the employee's earnings. By using the wildcarding techniques, a number of
	!	different types may be entered at one time. Valid types are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	FW = Federal Withholding Tax
	!	.le
	!	SW = State Withholding Tax
	!	.le
	!	CW = City Withholding Tax
	!	.le
	!	DW = County Withholding Tax
	!	.le
	!	EW = School District Withholding Tax
	!	.els
	!
	! Index:
	!	.x Wildcard Tax Type>Tax Ledger Dump
	!	.x Tax Ledger Dump>Wildcard Tax Type
	!
	!--


	!TAX_CODE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Wildcard tax code\*
	!	.p
	!	The ^*Wildcard Tax Code\* field identifies the specific State, city,
	!	county, or school district for corresponding tax types. Codes must be equal to
	!	the codes established in the Tax Table file. State codes are equal to the State
	!	Post Office codes. Local tax codes must be equal to the codes for the related
	!	jurisdictions as they exist in the Tax Tables. The code field for Federal
	!	Withholding Tax type would be left blank. The Wildcarding Techniques may
	!	be used to identify more than one code at a single time.
	!
	! Index:
	!	.x Wildcard tax code>Tax Ledger Dump
	!	.x Tax Ledger Dump>Wildcard Tax Code
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

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Tax Withholding Dump - " + &
		NUM1$(QTR%) + PRNT_NUMBERITH(QTR%) + " Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "                                          W/H       " + &
		"  -------------Year To Date-------------" + &
		"    ----------Quarter to Date-----------"
	TITLE$(5%) = "Emp #      Name                          Type  Code " + &
		"          Wages         Taxes    Wks Wkd" + &
		"          Wages         Taxes    Wks Wkd"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:041,$TType:045,$Code:049," + &
		"VYTDWages:069,VYTDTaxes:083,VYTDWeeksWorked:094," + &
		"VQTDWages:109,VQTDTaxes:123,VQTDWeeksWorked:134"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	TOTALS% = 0%

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

	X% = PR_FUNC_READTAXES(PR_EMP_MASTER::EMPNUM, &
		PR_REG_TAXES.CH%, &
		PR_TAXES%, &
		PR_TAXES())

	%Page

17100	!
	! Print fed total
	!
	NAME_TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 30%) + "  "
	GROUP_TEXT$ = ""

	FOR LOOP% = 1% TO PR_TAXES%

		IF GROUP_TEXT$ <> PR_TAXES(LOOP%)::TTYPE
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) &
				IF GROUP_TEXT$ <> ""
			GROUP_TEXT$ = PR_TAXES(LOOP%)::TTYPE
		END IF

		YTDTAX = 0.0
		YTDREPORT = 0.0
		YTDWKWRK = 0.0
		FOR I% = 0% TO QTR% - 1%
			YTDTAX = YTDTAX + PR_TAXES(LOOP%)::TAX(I%)
			YTDREPORT = YTDREPORT + PR_TAXES(LOOP%)::REPORTABLE(I%)
			YTDWKWRK = YTDWKWRK + PR_TAXES(LOOP%)::WKWRK(I%)
		NEXT I%

		TEXT$ = NAME_TEXT$ + &
			PR_TAXES(LOOP%)::TTYPE + "     " + &
			PR_TAXES(LOOP%)::CODE + "  " + &
			FORMAT$(YTDREPORT, "##,###,###.## ") + &
			FORMAT$(YTDTAX, "##,###,###.## ") + &
			FORMAT$(YTDWKWRK, "   ###,###") + &
			"  " + &
			FORMAT$(PR_TAXES(LOOP%)::REPORTABLE(QTR% - 1%), "##,###,###.## ") + &
			FORMAT$(PR_TAXES(LOOP%)::TAX(QTR% - 1%), "##,###,###.##  ") + &
			FORMAT$(PR_TAXES(LOOP%)::WKWRK(QTR% - 1%), "  ###,###")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		LSET NAME_TEXT$ = ""

		CALL PRTAX_ADDTOTAL(TOTALS%, TOTALS(), &
			PR_TAXES(LOOP%)::TTYPE + PR_TAXES(LOOP%)::CODE, &
			PR_TAXES(LOOP%)::REPORTABLE(4%), &
			PR_TAXES(LOOP%)::TAX(4%), &
			PR_TAXES(LOOP%)::WKWRK(4%), &
			PR_TAXES(LOOP%)::REPORTABLE(QTR% - 1%), &
			PR_TAXES(LOOP%)::TAX(QTR% - 1%), &
			PR_TAXES(LOOP%)::WKWRK(QTR% - 1%))

		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) &
		IF GROUP_TEXT$ <> ""

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Grand Totals:", 10%)

	FOR LOOP% = 1% TO TOTALS%
		TEXT$ = "         " + &
			"                                  " + &
			LEFT(TOTALS(LOOP%)::CODE, 2%) + "     " + &
			RIGHT(TOTALS(LOOP%)::CODE, 3%) + "  " + &
			FORMAT$(TOTALS(LOOP%)::YTDWAGES, "##,###,###.## ") + &
			FORMAT$(TOTALS(LOOP%)::YTDTAXES, "##,###,###.## ") + &
			FORMAT$(TOTALS(LOOP%)::YTDWEEKS, "   ###,###") + &
			"  " + &
			FORMAT$(TOTALS(LOOP%)::QTDWAGES, "##,###,###.## ") + &
			FORMAT$(TOTALS(LOOP%)::QTDTAXES, "##,###,###.##   ") + &
			FORMAT$(TOTALS(LOOP%)::QTDWEEKS, " ###,###")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT LOOP%

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

19999	END


20000	SUB PRTAX_ADDTOTAL(LONG TOTALS, TOTAL_STRUCTURE TOTALS(), STRING CODE, &
		REAL YTDWAGES, REAL YTDTAXES, REAL YTDWEEKS, &
		REAL QTDWAGES, REAL QTDTAXES, REAL QTDWEEKS)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! This function is used to accumulate the totals for the
	! total at the bottom of the report.
	!
	RECORD TOTAL_STRUCTURE
		STRING CODE = 4
		REAL YTDWAGES
		REAL YTDTAXES
		REAL YTDWEEKS
		REAL QTDWAGES
		REAL QTDTAXES
		REAL QTDWEEKS
	END RECORD

	FOR I% = 1% TO TOTALS

		IF TOTALS(I%)::CODE = CODE
		THEN
			TOTALS(I%)::YTDWAGES = TOTALS(I%)::YTDWAGES + YTDWAGES
			TOTALS(I%)::YTDTAXES = TOTALS(I%)::YTDTAXES + YTDTAXES
			TOTALS(I%)::YTDWEEKS = TOTALS(I%)::YTDWEEKS + YTDWEEKS
			TOTALS(I%)::QTDWAGES = TOTALS(I%)::QTDWAGES + QTDWAGES
			TOTALS(I%)::QTDTAXES = TOTALS(I%)::QTDTAXES + QTDTAXES
			TOTALS(I%)::QTDWEEKS = TOTALS(I%)::QTDWEEKS + QTDWEEKS
			EXIT SUB
		END IF

	NEXT I%

	I%, TOTALS = TOTALS + 1%

	TOTALS(I%)::CODE = CODE
	TOTALS(I%)::YTDWAGES = YTDWAGES
	TOTALS(I%)::YTDTAXES = YTDTAXES
	TOTALS(I%)::YTDWEEKS = YTDWEEKS
	TOTALS(I%)::QTDWAGES = QTDWAGES
	TOTALS(I%)::QTDTAXES = QTDTAXES
	TOTALS(I%)::QTDWEEKS = QTDWEEKS

32767	END SUB
