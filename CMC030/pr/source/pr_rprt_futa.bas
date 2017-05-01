1	%TITLE "Payroll FUTA Report"
	%SBTTL "PR_RPRT_FUTA"
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
	! ID:PR041
	!
	! Abstract:HELP
	!	.p
	!	The ^*FUTA Report\* option
	!	prints a report for each calendar quarter.
	!	Though the Form 940 is completed only annually, it is recommended
	!	that the FUTA Report be printed quarterly and reconciled with the
	!	entries which have accrued in the FUTA liability account.
	!	.P
	!	The FUTA Report for each calendar quarter will provide
	!	information needed to complete the annual Form 940 and include it in the
	!	following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	FUTA Limit
	!	.le
	!	FUTA Percentage
	!	.le
	!	Employer Tax Number
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Social Security Number
	!	.le
	!	Start Date
	!	.le
	!	Year to Date Wages
	!	.le
	!	Year to Date Excess
	!	.le
	!	Year to Date FUTA Wages
	!	.le
	!	Quarter to Date Wages
	!	.le
	!	Quarter to Date Excess
	!	.le
	!	Quarter to Date FUTA Wages
	!	.els
	!
	! Index:
	!	.x FUTA Report
	!	.x Report>FUTA
	!	.x Report>940
	!	.x 940 Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_FUTA/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_FUTA, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_FUTA.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	01/07/91 - Kevin Handy
	!		Added line in title about the sort order.
	!
	!	01/15/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to only look at "D" and "P" types from
	!		PR_REG_ERNDED. (Ignore "A" types)
	!
	!	06/24/92 - Kevin Handy
	!		Added wildcard location.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Updated for V3.6 Calico coding standards.
	!
	!	06/27/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	06/27/95 - Kevin Handy
	!		Lose check for 1099 flag, so we don't have any
	!		magic happening.
	!
	!	07/01/96 - Kevin Handy
	!		Reformat source code.
	!		Loose lots of zero total pages when printing by
	!		department.
	!
	!	07/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	05/29/98 - Kevin Handy
	!		Modified to handle 'F' final deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/17/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED) PR_REG_ERNDED_CDD PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

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
	!	The ^*From Item\* field causes the printing
	!	to begin with a particular item.
	!	The value must be in agreement with field
	!	(03).
	!	.p
	!	A blank will cause the report to start with the first item in
	!	the file.
	!
	! Index:
	!	.x From Item>FUTA Report
	!	.x FUTA Report>From Item
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
	!	.x To Item>FUTA Report
	!	.x FUTA Report>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU, SN, NA, AL, DP)\*
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
	!	AL = Alphabetical (last name first)
	!	.le
	!	DP = Department
	!	.lm -10
	!	.p
	!	.els
	!	An entry is required in this field and only the above codes
	!	are valid.
	!
	! Index:
	!	.x Sort>FUTA Report
	!	.x FUTA Report>Sort
	!
	!--

	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%
		SB_TEXT$ = "By Employee Number"
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))

	CASE "NA"
		K_NUM% = 1%
		SB_TEXT$ = "By Employee Name"
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))

	CASE "SN"
		K_NUM% = 3%
		SB_TEXT$ = "By Social Security Number"
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SSN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SSN))

	CASE "DP"
		K_NUM% = 4%
		SB_TEXT$ = "By Department"
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::DEPT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::DEPT))

	CASE ELSE
		K_NUM% = 2%
		SB_TEXT$ = "By Alpha Key"
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
	!	This field requires an entry. The format for entry is YYYY.
	!
	! Index:
	!	.x Year>FUTA Report
	!	.x FUTA Report>Year
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
	!	This field requires an entry and will accommodate
	!	a one digit number.
	!
	! Index:
	!	.x Quarter>FUTA Report
	!	.x FUTA Report>Quarter
	!
	!--

	WILDLOC$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Location\*
	!
	! Index:
	!	.x Location>SUTA Report
	!	.x SUTA Report>Location
	!
	! Datatype:TEXT
	! Size:15
	! Required:N
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Earnings and deduction definition
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Earnings and Deduction register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

330	!
	! Open Tax Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"

		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F  ", REGARDLESS

		REPNO$ = PR_TAX_PROFILE_F::REPNO
		FUI_PCT = PR_TAX_PROFILE_F::FUI_PCT/100.
		FUI_MAX = PR_TAX_PROFILE_F::FUI_MAX

		CLOSE PR_TAX_PROFILE.CH%
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Federal Unemployment Tax Act Report - " + &
		NUM1$(QTR%) + MID("stndrdth", QTR% * 2% - 1%, 2%) + " Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = SB_TEXT$
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "Employer Tax # " + REPNO$ + "  FUTA Percent: " + &
		NUM1$(FUI_PCT * 100.0) + "%   FUTA Limit: " + NUM1$(FUI_MAX)
	TITLE$(6%) = "Emp #      Name                  SSN         " + &
		"St Date      YTD Wages   YTD Excess  YTD FUTA Wg    " + &
		"QTD Wages   QTD Excess  QTD FUTA Wg"
	TITLE$(7%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:032,$SSN:044,DHireDate:053," + &
		"VYTDWage:067,VYTDExcess:080,VYTDFUTAWage:093,VQTDWage:106," + &
		"VQTDExcess:119,VQTDFUTAWage:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	THIS_LOC$ = ""
	LOCATION_COUNT% = 0%

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
	! Check location wildcard
	!
	IF WILDLOC$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(PR_EMP_MASTER::LOCATION, WILDLOC$) = 0%
	END IF

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

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 6%
	LIA_WAGES(I%) = 0.0 FOR I% = 1% TO 4%

17060	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
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

	IF PR_REG_ERNDED::EMPNUM <> PR_EMP_MASTER::EMPNUM
	THEN
		GOTO 17100
	END IF

	GOTO 17070 &
		IF PR_REG_ERNDED::ETYPE <> "P" AND &
		PR_REG_ERNDED::ETYPE <> "D" AND &
		PR_REG_ERNDED::ETYPE <> "F"

	!
	! Calculate ytd and qtd amounts
	!
	YTD_TOTAL, QTD_TOTAL = 0.0

	YTD_TOTAL = FUNC_ROUND(YTD_TOTAL + PR_REG_ERNDED::QTR_DOLL(LOOP%), 2%) &
		FOR LOOP% = 0% TO QTR% - 1%

	TEMP_LIA(LOOP%) = FUNC_ROUND(PR_REG_ERNDED::QTR_DOLL(LOOP% - 1%), 2%) &
		FOR LOOP% = 1% TO QTR%

	QTD_TOTAL = PR_REG_ERNDED::QTR_DOLL(QTR% - 1%)

	SUBJECT_CODE$ = "FUI"

	!
	! See if subject to federal withholding
	!
	CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
		SUBJECT_CODE$, &
		PR_REG_ERNDED::ETYPE, &
		PR_REG_ERNDED::CODE, &
		TAXABLE%, &
		REPORTABLE%)

	PTYPE$ = PR_REG_ERNDED::ETYPE

	EMP_TOTAL(1%) = FUNC_ROUND(EMP_TOTAL(1%) + YTD_TOTAL, 2%) &
		IF PTYPE$ = "P" AND REPORTABLE% = 0%

	EMP_TOTAL(1%) = FUNC_ROUND(EMP_TOTAL(1%) - YTD_TOTAL, 2%) &
		IF (PTYPE$ = "D" OR PTYPE$ = "F") AND REPORTABLE%

	EMP_TOTAL(4%) = FUNC_ROUND(EMP_TOTAL(4%) + QTD_TOTAL, 2%) &
		IF PTYPE$ = "P" AND REPORTABLE% = 0%

	EMP_TOTAL(4%) = FUNC_ROUND(EMP_TOTAL(4%) - QTD_TOTAL, 2%) &
		IF (PTYPE$ = "D"  OR PTYPE$ = "F") AND REPORTABLE%

	LIA_WAGES(LOOP%) = FUNC_ROUND(LIA_WAGES(LOOP%) + TEMP_LIA(LOOP%), 2%) &
		IF PTYPE$ = "P" AND REPORTABLE% = 0% &
			FOR LOOP% = 1% TO QTR%

	LIA_WAGES(LOOP%) = FUNC_ROUND(LIA_WAGES(LOOP%) - TEMP_LIA(LOOP%), 2%) &
		IF (PTYPE$ = "D"  OR PTYPE$ = "F") AND REPORTABLE% &
			FOR LOOP% = 1% TO QTR%

	GOTO 17070

	%PAGE

17100	!*******************************************************************
	! Print total for employee
	!*******************************************************************

	!
	! Print department totals?
	!
	IF SORTBY$ = "DP" AND &
		((THIS_LOC$ <> PR_EMP_MASTER::LOCATION + &
		PR_EMP_MASTER::DEPT + &
		PR_EMP_MASTER::WORK_CENTER) OR (THIS_LOC$ == ""))
	THEN
		GOSUB LocationTotal
	END IF

	SUM = 0.0
	SUM = FUNC_ROUND(SUM + EMP_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 6%

	IF SUM <> 0.0
	THEN
		EMP_TOTAL(2%) = EMP_TOTAL(1%) - FUI_MAX
		EMP_TOTAL(2%) = 0.0 IF EMP_TOTAL(2%) < 0.0
		EMP_TOTAL(2%) = EMP_TOTAL(1%) IF EMP_TOTAL(2%) >= EMP_TOTAL(1%)
		EMP_TOTAL(3%) = EMP_TOTAL(1%) - EMP_TOTAL(2%)

		EMP_TOTAL(5%) = EMP_TOTAL(1%) - FUI_MAX
		EMP_TOTAL(5%) = 0.0 IF EMP_TOTAL(5%) < 0.0
		EMP_TOTAL(5%) = EMP_TOTAL(4%) IF EMP_TOTAL(5%) >= EMP_TOTAL(4%)
		EMP_TOTAL(6%) = EMP_TOTAL(4%) - EMP_TOTAL(5%)

		TEMP_WAGES = 0.0
		FOR LOOP% = 1% TO QTR%
			TEMP_WAGES = TEMP_WAGES + LIA_WAGES(LOOP%)
			TEMP_FUTA_EXCESS = TEMP_WAGES - FUI_MAX
			TEMP_FUTA_EXCESS = 0.0 IF TEMP_FUTA_EXCESS < 0.0
			TEMP_FUTA_EXCESS = LIA_WAGES(LOOP%) &
				IF TEMP_FUTA_EXCESS >= LIA_WAGES(LOOP%)
			TEMP_FUTA_WAGES = LIA_WAGES(LOOP%) - TEMP_FUTA_EXCESS
			FUTA_WAGES(LOOP%) = FUTA_WAGES(LOOP%) + TEMP_FUTA_WAGES
		NEXT LOOP%

		GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 6%
		SUB_TOTAL(LOOP%) = SUB_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 6%

		IF (SORTBY$ = "DP") AND (LOCATION_COUNT% = 0%)
		THEN
			TITLE$(3%) = "Loc: " + PR_EMP_MASTER::LOCATION + &
				" Dep: " + PR_EMP_MASTER::DEPT + &
				" W/C: " + PR_EMP_MASTER::WORK_CENTER

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
		END IF

		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 21%) + " " + &
			LEFT(PRNT_SSN(PR_EMP_MASTER::SSN, 0%), 11%) + " " + &
			PRNT_DATE(PR_EMP_MASTER::HIREDAY, 10%) + "  " + &
			FORMAT$(EMP_TOTAL(1%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(2%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(3%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(4%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(5%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(6%), "#,###,###.## ")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		LOCATION_COUNT% = LOCATION_COUNT% + 1%

	END IF

17350	!
	! Try for next record
	!
	GOTO 17020

	%PAGE

 LocationTotal:
	!*******************************************************************
	! Handle end of report
	!*******************************************************************

	IF (LOCATION_COUNT% <> 0%)
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEMP$ = "      Location Total" + SPACE$(45%)
		TEXT$ = LEFT(TEMP$, 56%) + &
			FORMAT$(SUB_TOTAL(1%), "#,###,###.## ") + &
			FORMAT$(SUB_TOTAL(2%), "#,###,###.## ") + &
			FORMAT$(SUB_TOTAL(3%), "#,###,###.## ") + &
			FORMAT$(SUB_TOTAL(4%), "#,###,###.##") + &
			FORMAT$(SUB_TOTAL(5%), "#,###,###.## ") + &
			FORMAT$(SUB_TOTAL(6%), "#,###,###.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEXT$ = "       Quarter " + NUM1$(QTR%) + &
			" FUTA Tax Liagility = " + &
			"QTD FUTA Wages X FUTA Percentage = " + &
			FORMAT$(SUB_TOTAL(6%), "###,###,###.##") + &
			" X " + &
			FORMAT$(FUI_PCT, ".#####") + &
			"  =  " + &
			FORMAT$(SUB_TOTAL(6%) * FUI_PCT, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "       YTD       FUTA Tax Liagility = " + &
			"YTD FUTA Wages X FUTA Percentage = " + &
			FORMAT$(SUB_TOTAL(3%), "###,###,###.##") + &
			" X " + &
			FORMAT$(FUI_PCT, ".#####") + &
			"  =  " + &
			FORMAT$(SUB_TOTAL(3%) * FUI_PCT, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	THIS_LOC$ = PR_EMP_MASTER::LOCATION + &
		PR_EMP_MASTER::DEPT + &
		PR_EMP_MASTER::WORK_CENTER

	SUB_TOTAL(I%) = 0.0 FOR I% = 1% TO 6%
	LOCATION_COUNT% = 0%

	RETURN

	%PAGE

 ExitTotal:
	!
	! Handle end of report
	!
	IF SORTBY$ = "DP"
	THEN
		GOSUB LocationTotal
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEMP$ = "         Grand Total" + SPACE$(45%)
	TEXT$ = LEFT(TEMP$, 56%) + &
		FORMAT$(GRAND_TOTAL(1%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(4%), "#,###,###.##") + &
		FORMAT$(GRAND_TOTAL(5%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "#,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)

	TEXT$ = "       Quarter " + NUM1$(QTR%) + " FUTA Tax Liagility = " + &
		"QTD FUTA Wages X FUTA Percentage = " + &
		FORMAT$(GRAND_TOTAL(6%), "###,###,###.##") + &
		" X " + &
		FORMAT$(FUI_PCT, ".#####") + &
		"  =  " + &
		FORMAT$(GRAND_TOTAL(6%) * FUI_PCT, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "       YTD       FUTA Tax Liagility = " + &
		"YTD FUTA Wages X FUTA Percentage = " + &
		FORMAT$(GRAND_TOTAL(3%), "###,###,###.##") + &
		" X " + &
		FORMAT$(FUI_PCT, ".#####") + &
		"  =  " + &
		FORMAT$(GRAND_TOTAL(3%) * FUI_PCT, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "       Quarter               First        Second         " + &
		"Third        Fourth"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "       Subject Wage   " + &
		FORMAT$(FUTA_WAGES(1%), "#,###,###.##  ") + &
		FORMAT$(FUTA_WAGES(2%), "#,###,###.##  ") + &
		FORMAT$(FUTA_WAGES(3%), "#,###,###.##  ") + &
		FORMAT$(FUTA_WAGES(4%), "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "       Liability      " + &
		FORMAT$(FUTA_WAGES(1%) * FUI_PCT, "#,###,###.##  ") + &
		FORMAT$(FUTA_WAGES(2%) * FUI_PCT, "#,###,###.##  ") + &
		FORMAT$(FUTA_WAGES(3%) * FUI_PCT, "#,###,###.##  ") + &
		FORMAT$(FUTA_WAGES(4%) * FUI_PCT, "#,###,###.##")

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
