1	%TITLE "Payroll Audit Quarter Report"
	%SBTTL "PR_RPRT_AUDT_QUARTER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho  83402
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
	! ID:PR050
	!
	! Abstract:HELP
	!	.p
	!	This report is used to compare the reportable amounts from
	!	the individual folders and the QTD payroll.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_QUARTER/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_AUDT_QUARTER, -
	!	FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_QUARTER.OBJ;*
	!
	! Author:
	!
	!	03/22/93 - Kevin Handy
	!		Based upon PR_RPRT_AUDT_CHECK.
	!
	! Modification history:
	!
	!	03/25/93 - Kevin Handy
	!		Lose line between employee total and reg total
	!		if only printing totals. (Save paper)
	!
	!	03/29/93 - Kevin Handy
	!		Fix rounding error where was rounding to one
	!		digit instead of two.
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/17/94 - Kevin Handy
	!		Fix handling of deductions to watch out for extra
	!		taxes witheld as deductions.
	!
	!	01/25/94 - Kevin Handy
	!		Added summary at end for YTD deductions that are
	!		not taxable.
	!
	!	01/25/94 - Kevin Handy
	!		Moved location totals back two spaces so they would
	!		line up correctly.
	!
	!	02/08/94 - Kevin Handy
	!		Reformatted so more lines would be less than 80.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Convert source to V3.6 Calico coding standards.
	!		Remove unsolicited input stuff.
	!
	!	05/10/95 - Kevin Handy
	!		Added ability to page after each employee.
	!
	!	05/12/95 - Kevin Handy
	!		Modified open of PR_TRN_CHECK and PR_HIS_CHECK to
	!		.OPN instead of .CRE.
	!		Trap error on PR_TRN_CHECK and skip if unable to
	!		find file.
	!
	!	07/10/95 - Kevin Handy
	!		Fixed bug with counting employees.
	!
	!	07/20/95 - Kevin Handy
	!		Print difference when quarterly total does not
	!		balance properly.
	!
	!	09/26/95 - Kevin Handy
	!		Added several comments
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/31/96 - Kevin Handy
	!		Stagger grand total into two lines so that
	!		it could handle larger numbers (LL)
	!
	!	03/12/97 - Kevin Handy
	!		Handle FH code
	!		Use integer for #key
	!
	!	06/17/97 - Kevin Handy
	!		Fix bug with FH doubling taxable amount.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	10/06/97 - Kevin Handy
	!		Fix another bug with FH taxable doubling.
	!
	!	05/29/98 - Kevin Handy
	!		Handle new 'F' final deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/22/99 - Kevin Handy
	!		Added abailaty to check a whole year at once
	!		by entering a quarter of zero '0'
	!
	!	11/29/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED
	MAP (PR_HIS_DED)	PR_TRN_DED_CDD	PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK
	MAP (PR_HIS_CHECK)	PR_TRN_CHECK_CDD	PR_HIS_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	RECORD PR_TEMP_RECORD
		STRING	SORT = 26%
		STRING	EMPLOYEE = 10%
		STRING	PRDATE = 8%
		GFLOAT	AMOUNT(8%)		! 0 - Pay
						! 1 - Deduction
						! 2 - Other
						! 3 - Federal tax
						! 4 - Federal reportable
						! 5 - Fica tax
						! 6 - Fica reportable
						! 7 - State tax
						! 8 - State reportable
		STRING	CHECK = 6%
	END RECORD

	MAP (PR_TEMP)	PR_TEMP_RECORD	PR_TEMP

	RECORD NONTAX_LIST_RECORD
		STRING PDCODE = 3%
		GFLOAT FEDERAL
		GFLOAT FICA
		GFLOAT STATE
	END RECORD

	DIM NONTAX_LIST_RECORD NONTAX_LIST(200%)

	DECLARE INTEGER CONSTANT FILE_MAX = 2000%

	!
	! Dimension
	!
	DIM DATA_FILE$(FILE_MAX), &
		GRAND_TOTAL(FILE_MAX, 8%), &
		EMP_TOTAL(8%), &
		LINE_TOTAL(8%), &
		SD_TOTAL(8%), &
		SL_TOTAL(8%), &
		SS_TOTAL(8%), &
		TOTAL(8%), &
		QTOTAL(8%)

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
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* field specifies the report is
	!	to begin with a particular item.
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

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* field specifies the report is
	!	to end with a particular item.
	!	The value must be in agreement with field
	!	(05).
	!	.p
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!
	! Index:
	!	.x To Item>Employee Check Audit Report
	!	.x Employee Check Audit Report>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort (NU, NA, LO, SO, SD)\*
	!	.p
	!	The ^*Sort\* field specifies the order in which the report will print.
	!	.p
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	LO = Location
	!	.le
	!	SO = Alphabetical (last name first)
	!	.le
	!	SN = Social Security Number
	!	.le
	!	SD = State/Location/Department/Alpha
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field an only the above entries
	!	are valid.
	!
	! Index:
	!	.x Sort>Employee Check Audit Report
	!	.x Employee Check Audit Report>Sort
	!
	!--

	SSNFLAG$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(5%), -1%), 1%)

	!++
	! Abstract:FLD06
	!	^*(05) Print Social Security Numbers\*
	!	.p
	!	This field determines if the employee's
	!	social security number should be printed on the report. An answer of ^*Y\*
	!	would cause the numbers to be printed, while a ^*N\* answer of no would not.
	!
	! Index:
	!	.x Print Social Security Numbers>Employee Check Audit Report
	!	.x Employee Check Audit Report>Print Social Security Numbers
	!
	! Datatype:YN
	! Size:1
	! Valid Input: Y,N
	!--

	TOTALFLAG$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(6%), -1%), 1%)

	!++
	! Abstract:FLD07
	!	^*(07) Print Totals Only\*
	!	.p
	!	Only print totals (*Y), or print out detail for each pay
	!	date (*N).
	!
	! Index:
	!	.x Print Totals>Employee Check Audit Report
	!	.x Employee Check Audit Report>Print Totals
	!
	! Datatype:YN
	! Size:1
	! Valid Input: Y,N
	!--

	YYYY$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(7%), -1%), 4%)

	!++
	! Abstract:FLD08
	!	^*(08) Year\*
	!	.b
	!	This field specifies which payroll year is to be compared with.
	!	This field and the next are used to pull up a comparison with
	!	the quarterly totals.
	!
	! Index:
	!
	!--

	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)) - 1%

	!++
	! Abstract:FLD09
	!	^*(09) Quarter\*
	!	.b
	!	This field specifies which quarter from the quarterly
	!	totals is to be compared with.
	!	.b
	!	Valid values are:
	!	.lm +5
	!	.b
	!	*0 All 4 Quarters added together
	!	.br
	!	*1 1st quarter
	!	.br
	!	*2 2nd quarter
	!	.br
	!	*3 3rd quarter
	!	.br
	!	*4 4th quarter
	!	.lm -5
	!
	! Index:
	!
	!--

	DOPAGE$ = LEFT$(UTL_REPORTX::OPTDEF(9%), 1%)

	!++
	! Abstract:FLD10
	!	^*(09) Page After Employee\*
	!	.b
	!	This field specifies if a page break should occur between
	!	each employee.
	!
	! Index:
	!
	!--

	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))
		SB_TEXT$ = "By Employee Number"

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))
		SB_TEXT$ = "By Employee Name"

	CASE "SN"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SSN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SSN))
		SB_TEXT$ = "By Social Security Number"

	CASE "LO"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::LOCATION))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::LOCATION))
		SB_TEXT$ = "By Location"

	CASE "SD"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, 2%)
		TO_ITEM$ = LEFT(TO_ITEM$, 2%)
		SB_TEXT$ = "By State, Location, Department, Alpha"

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))
		SB_TEXT$ = "By Alpha Key"

	END SELECT

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

290	!
	! Create a temporary file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)
	OPEN "PR_TEMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		BUFFER 32%, &
		MAP PR_TEMP, &
		PRIMARY KEY (PR_TEMP::SORT, &
			PR_TEMP::EMPLOYEE, &
			PR_TEMP::PRDATE, &
			PR_TEMP::CHECK) DUPLICATES, &
		ACCESS MODIFY, &
		ALLOW NONE

300	!
	! Open up master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open up ERNDED file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED"
		CONTINUE HelpError
	END WHEN

320	!
	! Open up TAXES file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

330	!
	! Open deduction definition file, and remember those that can
	! alter the reportable amounts for federal/fica/state
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"

		RESET #PR_ERNDED_DEF.CH%
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	NONTAX_FEDERAL$ = ""
	NONTAX_FICA$ = ""
	NONTAX_STATE$ = ""

335	!
	! Figure out which deductions need to be subtracted from the gross
	! earnings to make the reportable match the gross.
	!
	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%
	USE
		CONTINUE 340
	END WHEN

	IF (PR_ERNDED_DEF::ETYPE = "P")
	THEN
		NONTAX_FEDERAL$ = NONTAX_FEDERAL$ + "," + &
			PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE &
			IF PR_ERNDED_DEF::REPORTABLE_FWH = "N"
		NONTAX_FICA$ = NONTAX_FICA$ + "," + &
			PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE &
			IF PR_ERNDED_DEF::REPORTABLE_FIR = "N"
		NONTAX_STATE$ = NONTAX_STATE$ + "," + &
			PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE &
			IF PR_ERNDED_DEF::REPORTABLE_SWH = "N"
	ELSE
		IF (PR_ERNDED_DEF::ETYPE = "D") OR (PR_ERNDED_DEF::ETYPE = "F")
		THEN
			NONTAX_FEDERAL$ = NONTAX_FEDERAL$ + "," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE &
				IF PR_ERNDED_DEF::REPORTABLE_FWH = "N"
			NONTAX_FICA$ = NONTAX_FICA$ + "," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE &
				IF PR_ERNDED_DEF::REPORTABLE_FIR = "N"
			NONTAX_STATE$ = NONTAX_STATE$ + "," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE &
				IF PR_ERNDED_DEF::REPORTABLE_SWH = "N"
		END IF
	END IF

	GOTO 335

340	!
	! Finish up deduction lookup
	!
	CLOSE PR_ERNDED_DEF.CH%

	CALL ASSG_FREECHANNEL(PR_ERNDED_DEF.CH%)

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Net Check Audit Report"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + " To: " + &
		PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = SB_TEXT$
	TITLE$(4%) = ""
	!
	! Define headers
	!
	TITLE$(5%) = "                                    " + &
		"                                       ---Federal---     " + &
		"---Fica---        ---State---"
	TITLE$(6%) = "Emp #      Name                     " + &
		"Date        Pay     Ded     Other      Tax   Report      " + &
		"Tax   Report      Tax   Report  Check#"

	TITLE$(7%) = ""

	!
	! Set up line layouts
	!
	LYT_LINE$ = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	THIS_SD$ = "^&#*&#*(*@&#&@*"
	SD_EMP_COUNT% = 0%

	THIS_SL$ = "^&#*&#*(*@&#&@*"
	SL_EMP_COUNT% = 0%

	THIS_SS$ = "^&#*&#*(*@&#&@*"
	SS_EMP_COUNT% = 0%

	NUMBER_OF_LINES% = 0%

	FOR PR_LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)
		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 17010 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%
		USE_HISTORY% = 0%

		GOTO 17020

17010		!
		! Open pay history folder if journal not there
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%
		USE_HISTORY% = -1%

17020		!
		! Open Deduction folder
		!
		WHEN ERROR IN
			IF USE_HISTORY% = 0%
			THEN
				%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
			ELSE
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
				PR_TRN_DED.CH% = PR_HIS_DED.CH%
			END IF
		USE
			CONTINUE 17350 IF ERR = 5%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

17030		!
		! Open Check folder
		!
		WHEN ERROR IN
			IF USE_HISTORY% = 0%
			THEN
				%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.OPN"
			ELSE
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.OPN"
				PR_TRN_CHECK.CH% = PR_HIS_CHECK.CH%
			END IF
		USE
			CONTINUE 17100 IF ERR = 5%
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

17100		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_EMP_MASTER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF

 GetNextRec:
17200		!
		! Main loop starts here
		!
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Get next record
		!
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, REGARDLESS
		USE
			IF ERR = 11%
			THEN
				CONTINUE 17370
			END IF
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

 !		CALL ENTR_3MESSAGE(SCOPE, PR_EMP_MASTER::EMPNUM, 1%)
		FLAG_END% = 0%

		!
		! Check current record
		!
		SELECT SORTBY$

		CASE "NU"
			GOTO 17370 IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
				TO_ITEM$ <> ""

		CASE "NA"
			GOTO 17370 IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
				TO_ITEM$ <> ""

		CASE "SN"
			GOTO 17370 IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
				TO_ITEM$ <> ""

		CASE "DP"
			GOTO 17370 IF (PR_EMP_MASTER::DEPT > TO_ITEM$) AND &
				TO_ITEM$ <> ""

		CASE "LO"
			GOTO 17370 IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
				TO_ITEM$ <> ""

		CASE "SD"
			GOTO 17200 IF (PR_EMP_MASTER::TAX_PKG > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			GOTO 17200 IF (PR_EMP_MASTER::TAX_PKG < FROM_ITEM$)

		CASE ELSE
			GOTO 17370 IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
				TO_ITEM$ <> ""
		END SELECT

		WORK_EMPNUM$ = PR_EMP_MASTER::EMPNUM

17300		!
		! Get pay detail information
		!
		LINE_TOTAL(I%) = 0.0 FOR I% = 0% TO 8%

		WHEN ERROR IN
			FIND #PR_TMP_PAY.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 17200
		END WHEN

		TEST_PR_END_DATE$ = ""

17310		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			FLAG_END% = -1%
			CONTINUE 17330
		END WHEN

		!
		! If history then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		GOTO 17330 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM) OR &
			(PR_TRN_PAY::PR_END_DATE <> TEST_PR_END_DATE$ AND &
			TEST_PR_END_DATE$ <> "")

		GOTO 17310 IF PR_TRN_PAY::PTYPE = "A"

17320		TEST_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

		!
		! Su,,arize by Pay, Deduction and Other
		!
		SELECT PR_TRN_PAY::PTYPE
		CASE "A"
			! Ignore
		CASE "P", "O"
			LINE_TOTAL(0%) = FUNC_ROUND(LINE_TOTAL(0%) + &
				PR_TRN_PAY::GROSS, 2%)
		CASE "D", "F"
			LINE_TOTAL(1%) = FUNC_ROUND(LINE_TOTAL(1%) + &
				PR_TRN_PAY::GROSS, 2%)
		CASE ELSE
			LINE_TOTAL(2%) = FUNC_ROUND(LINE_TOTAL(2%) + &
				PR_TRN_PAY::GROSS, 2%)
		END SELECT

		GOTO 17310

17330		WHEN ERROR IN
			FIND #PR_TRN_DED.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM + &
				TEST_PR_END_DATE$, REGARDLESS
		USE
			CONTINUE 17350 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

17340		WHEN ERROR IN
			GET #PR_TRN_DED.CH%, REGARDLESS
		USE
			CONTINUE 17350 IF ERR = 11%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		!
		! If journal net there the set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_DED = PR_HIS_DED
		END IF

		GOTO 17350 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM) OR &
			(PR_TRN_DED::PR_END_DATE <> TEST_PR_END_DATE$ AND &
			TEST_PR_END_DATE$ <> "")

		SELECT PR_TRN_DED::DTYPE
		CASE "D", "F"
			!
			! Handle special deductions for extra taxes
			!
			SELECT PR_TRN_DED::CODE

			CASE "FI", "FH"
				LINE_TOTAL(5%) = FUNC_ROUND(LINE_TOTAL(5%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE "FW"
				LINE_TOTAL(3%) = FUNC_ROUND(LINE_TOTAL(3%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE "SW"
				LINE_TOTAL(7%) = FUNC_ROUND(LINE_TOTAL(7%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE ELSE
				LINE_TOTAL(1%) = FUNC_ROUND(LINE_TOTAL(1%) + &
					PR_TRN_DED::AMOUNT, 2%) &

			END SELECT


		CASE "C"
			SELECT PR_TRN_DED::CODE

			CASE "FI"
				LINE_TOTAL(5%) = FUNC_ROUND(LINE_TOTAL(5%) + &
					PR_TRN_DED::AMOUNT, 2%)
				LINE_TOTAL(6%) = FUNC_ROUND(LINE_TOTAL(6%) + &
					PR_TRN_DED::REPORTABLE, 2%)

			CASE "FH"
				LINE_TOTAL(5%) = FUNC_ROUND(LINE_TOTAL(5%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE "FW"
				LINE_TOTAL(3%) = FUNC_ROUND(LINE_TOTAL(3%) + &
					PR_TRN_DED::AMOUNT, 2%)
				LINE_TOTAL(4%) = FUNC_ROUND(LINE_TOTAL(4%) + &
					PR_TRN_DED::REPORTABLE, 2%)

			CASE "SW"
				LINE_TOTAL(7%) = FUNC_ROUND(LINE_TOTAL(7%) + &
					PR_TRN_DED::AMOUNT, 2%)
				LINE_TOTAL(8%) = FUNC_ROUND(LINE_TOTAL(8%) + &
					PR_TRN_DED::REPORTABLE, 2%)
			END SELECT

		CASE ELSE
			LINE_TOTAL(2%) = FUNC_ROUND(LINE_TOTAL(2%) + &
				PR_TRN_DED::AMOUNT, 2%) &

		END SELECT

		GOTO 17340

17350		!
		! Get the check number now
		!
		WHEN ERROR IN
			GET #PR_TRN_CHECK.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM + &
				TEST_PR_END_DATE$, REGARDLESS
		USE
			PR_TRN_CHECK::PR_END_DATE	= ""
			PR_TRN_CHECK::CHECK		= ""
			PR_TRN_CHECK::CHECK_DATE	= ""
			PR_TRN_CHECK::PAYFREQ		= 0%

			CONTINUE 17360 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

		!
		! If history then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_CHECK = PR_HIS_CHECK
		END IF

17360		!
		! Print employee total and loop back for next employee
		!
		SELECT SORTBY$
		CASE "NU"
			PR_TEMP::SORT = PR_EMP_MASTER::EMPNUM
		CASE "NA"
			PR_TEMP::SORT = PR_EMP_MASTER::EMPNAME
		CASE "SN"
			PR_TEMP::SORT = PR_EMP_MASTER::SSN
		CASE "DP"
			PR_TEMP::SORT = PR_EMP_MASTER::DEPT
		CASE "LO"
			PR_TEMP::SORT = PR_EMP_MASTER::LOCATION
		CASE "SD"
			PR_TEMP::SORT = PR_EMP_MASTER::TAX_PKG + &
				PR_EMP_MASTER::LOCATION + &
				PR_EMP_MASTER::DEPT + &
				PR_EMP_MASTER::SORT
		CASE ELSE
			PR_TEMP::SORT = PR_EMP_MASTER::SORT
		END SELECT

		PR_TEMP::EMPLOYEE = WORK_EMPNUM$
		PR_TEMP::PRDATE = BATCH_NO$
		PR_TEMP::AMOUNT(X%) = LINE_TOTAL(X%) FOR X% = 0% TO 8%
		PR_TEMP::CHECK = PR_TRN_CHECK::CHECK

		PUT #PR_TEMP.CH%

		GRAND_TOTAL(PR_LOOP%, LOOP%) = GRAND_TOTAL(PR_LOOP%, LOOP%) + &
			LINE_TOTAL(LOOP%) FOR LOOP% = 0% TO 8%

		GOTO ExitTotal IF UTL_REPORTX::STAT

		LINE_TOTAL(I%) = 0.0 FOR I% = 0% TO 8%

		IF (PR_TRN_PAY::EMPNUM = PR_EMP_MASTER::EMPNUM) AND &
			(FLAG_END% = 0%)
		THEN
			GOTO 17320
		ELSE
			GOTO 17200
		END IF

17370		!
		! Make sure it doesn't close down files on us
		!
		CLOSE PR_TRN_CHECK.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_CHECK.CH%)
		PR_TRN_CHECK.CH% = 0%
		CLOSE PR_TRN_DED.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_DED.CH%)
		PR_TRN_DED.CH% = 0%
		CLOSE PR_TRN_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		PR_TRN_PAY.CH% = 0%

	!
	! Get data from next payroll folder
	!
	NEXT PR_LOOP%


17400	RESET #PR_TEMP.CH%
	WORK_EMPNUM$ = "%^&#%^&@%#^&@%#^"
	PAGE_FLAG% = 0%

17410	GOTO ExitTotal IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #PR_TEMP.CH%
	USE
		CONTINUE ExitTotal
	END WHEN

	IF WORK_EMPNUM$ <> PR_TEMP::EMPLOYEE
	THEN
		GOSUB PrintEmployeeTotal

		IF DOPAGE$ = "Y" AND PAGE_FLAG%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
			PAGE_FLAG% = 0%
		END IF

		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_TEMP::EMPLOYEE, &
				REGARDLESS &
				IF PR_EMP_MASTER::EMPNUM <> PR_TEMP::EMPLOYEE
		USE
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN
	END IF

17420	SELECT SORTBY$

	CASE "SD"
		IF THIS_SS$ <> PR_EMP_MASTER::TAX_PKG
		THEN
			GOSUB SDTotal
			GOSUB SLTotal
			GOSUB SSTotal
		END IF
		IF (THIS_SS$ + THIS_SL$) <> &
			(PR_EMP_MASTER::TAX_PKG + PR_EMP_MASTER::LOCATION)
		THEN
			GOSUB SDTotal
			GOSUB SLTotal
		END IF
		GOSUB SDTotal IF (THIS_SS$ + THIS_SL$ + THIS_SD$) <> &
			(PR_EMP_MASTER::TAX_PKG + &
			PR_EMP_MASTER::LOCATION + &
			PR_EMP_MASTER::DEPT)

	CASE "LO"
		IF THIS_SL$ <> PR_EMP_MASTER::LOCATION
		THEN
			GOSUB SLTotal
		END IF

	END SELECT

	GOSUB PrintEmployeeLine

	!
	! Go to next record
	!
	GOTO 17410


 ExitTotal:
17500	!
	! Handle end of report
	!
	GOSUB PrintEmployeeTotal

	SELECT SORTBY$

	CASE "SD"
		GOSUB SDTotal
		GOSUB SLTotal
		GOSUB SSTotal

	CASE "LO"
		THATSIT$ = "END"
		GOSUB SLTotal

	END SELECT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

	TEMP$ = SPACE$(20%) + "Grand Total"

	TOTAL(LOOP%) = 0.0 FOR LOOP% = 0% TO 8%

	FOR PR_LOOP% = 1% TO DATA_FILE%

		TEXT$ = LEFT(TEMP$ + SPACE$(34%), 34%) + &
			PRNT_DATE(DATA_FILE$(PR_LOOP%), 6%) + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 0%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 1%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 2%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 3%), "#######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 4%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 5%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 6%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 7%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 8%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEMP$ = ""

		TOTAL(LOOP%) = TOTAL(LOOP%) + GRAND_TOTAL(PR_LOOP%, LOOP%) &
			FOR LOOP% = 0% TO 8%

	NEXT PR_LOOP%

	!
	! Print final total
	!
	TEMP$ = SPACE$(20%) + "All Payrolls"

	TEXT$ = LEFT(TEMP$ + SPACE$(43%), 40%) + &
		FORMAT$(TOTAL(0%), "########.##") + &
		"     " + &
		FORMAT$(TOTAL(2%), "##########.##") + &
		"      " + &
		FORMAT$(TOTAL(4%), "##########.##") + &
		"     " + &
		FORMAT$(TOTAL(6%), "##########.##") + &
		"     " + &
		FORMAT$(TOTAL(8%), "##########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

	TEMP$ = SPACE$(20%) + "All Payrolls"

	TEXT$ = LEFT(TEMP$ + SPACE$(43%), 41%) + &
		"      " + &
		FORMAT$(TOTAL(1%), "##########.##") + &
		"     " + &
		FORMAT$(TOTAL(3%), "###########.##") + &
		"     " + &
		FORMAT$(TOTAL(5%), "##########.##") + &
		"     " + &
		FORMAT$(TOTAL(7%), "##########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "Total of " + NUM1$(TT_EMP_COUNT%) + " employees paid"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF (NONTAX_LIST% <> 0%)
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = "Nontaxable from gross (From Quartly Totals)"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		FOR I% = 1% TO NONTAX_LIST%

			TEXT$ = "   Code " + &
				LEFT(NONTAX_LIST(I%)::PDCODE, 1%) + "-" + &
				RIGHT(NONTAX_LIST(I%)::PDCODE, 2%) + &
				"                            " + &
				"          " + &
				"          " + &
				"         " + &
				"          " + &
				FORMAT$(NONTAX_LIST(I%)::FEDERAL, "######.##") + &
				"         " + &
				FORMAT$(NONTAX_LIST(I%)::FICA, "######.##") + &
				"         " + &
				FORMAT$(NONTAX_LIST(I%)::STATE, "######.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		NEXT I%
	END IF

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

 PrintEmployeeLine:
18200	!******************************************************************
	! Print one line of employee history
	!******************************************************************


18210	EMP_TOTAL(LOOP%) = FUNC_ROUND(EMP_TOTAL(LOOP%) + &
		PR_TEMP::AMOUNT(LOOP%), 2%) &
		FOR LOOP% = 0% TO 8%
	SD_TOTAL(LOOP%) = FUNC_ROUND(SD_TOTAL(LOOP%) + &
		PR_TEMP::AMOUNT(LOOP%), 2%) &
		FOR LOOP% = 0% TO 8%
	SL_TOTAL(LOOP%) = FUNC_ROUND(SL_TOTAL(LOOP%) + &
		PR_TEMP::AMOUNT(LOOP%), 2%) &
		FOR LOOP% = 0% TO 8%
	SS_TOTAL(LOOP%) = FUNC_ROUND(SS_TOTAL(LOOP%) + &
		PR_TEMP::AMOUNT(LOOP%), 2%) &
		FOR LOOP% = 0% TO 8%

	IF (NUMBER_OF_LINES% = 0%) OR (SSNFLAG$ <> "Y")
	THEN
		TEXT$ = PR_TEMP::EMPLOYEE + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME + SPACE$(22%), 22%) + " "
	ELSE
		IF (NUMBER_OF_LINES% = 1%)
		THEN
			TEXT$ = SPACE$(14%) + &
				PRNT_SSN(PR_EMP_MASTER::SSN, 0%) + &
				SPACE$(8%)
		ELSE
			TEXT$ = SPACE$(34%)
		END IF
	END IF

	IF TOTALFLAG$ <> "Y"
	THEN
		TEXT$ = TEXT$ + &
			PRNT_DATE(PR_TEMP::PRDATE, 6%) + &
			FORMAT$(PR_TEMP::AMOUNT(0%), "######.##") + &
			FORMAT$(PR_TEMP::AMOUNT(1%), "######.##") + &
			FORMAT$(PR_TEMP::AMOUNT(2%), "######.##") + &
			FORMAT$(PR_TEMP::AMOUNT(3%), "#######.##") + &
			FORMAT$(PR_TEMP::AMOUNT(4%), "######.##") + &
			FORMAT$(PR_TEMP::AMOUNT(5%), "######.##") + &
			FORMAT$(PR_TEMP::AMOUNT(6%), "######.##") + &
			FORMAT$(PR_TEMP::AMOUNT(7%), "######.##") + &
			FORMAT$(PR_TEMP::AMOUNT(8%), "######.## ") + &
			PR_TEMP::CHECK

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		NUMBER_OF_LINES% = NUMBER_OF_LINES% + 1%
	ELSE
		NUMBER_OF_LINES% = 1%
	END IF

	PAGE_FLAG% = -1%	! Something has been printed for employee

	RETURN

	%Page

 PrintEmployeeTotal:
	!******************************************************************
	! Print Total for employee
	!******************************************************************

	GOTO PrintEmployeeTotal1 IF (NUMBER_OF_LINES% = 0%)

	!
	! Print SSN if haven't printed it yet for this employee
	!
	IF TOTALFLAG$ = "Y"
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME + SPACE$(22%), 22%) + " "
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	END IF

	IF (NUMBER_OF_LINES% = 1%) AND (SSNFLAG$ = "Y")
	THEN
		TEXT$ = SPACE$(14%) + &
			PRNT_SSN(PR_EMP_MASTER::SSN, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	END IF

	IF (NUMBER_OF_LINES% > 1%) OR (TOTALFLAG$ = "Y")
	THEN
		TEXT$ = SPACE$(16%) + "Employee Total" + SPACE$(12%) + &
			FORMAT$(EMP_TOTAL(0%), "######.##") + &
			FORMAT$(EMP_TOTAL(1%), "######.##") + &
			FORMAT$(EMP_TOTAL(2%), "######.##") + &
			FORMAT$(EMP_TOTAL(3%), "#######.##") + &
			FORMAT$(EMP_TOTAL(4%), "######.##") + &
			FORMAT$(EMP_TOTAL(5%), "######.##") + &
			FORMAT$(EMP_TOTAL(6%), "######.##") + &
			FORMAT$(EMP_TOTAL(7%), "######.##") + &
			FORMAT$(EMP_TOTAL(8%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	END IF

	IF (NUMBER_OF_LINES% >= 1%)
	THEN
		SD_EMP_COUNT% = SD_EMP_COUNT% + 1%
		SL_EMP_COUNT% = SL_EMP_COUNT% + 1%
		SS_EMP_COUNT% = SS_EMP_COUNT% + 1%
		TT_EMP_COUNT% = TT_EMP_COUNT% + 1%
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%) &
		IF TOTALFLAG$ <> "Y"

18350	!
	! Load in Qtr totals
	!
	QTOTAL(I%) = 0.0 FOR I% = 0% TO 8%

	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, KEY #0% EQ WORK_EMPNUM$, REGARDLESS
	USE
		CONTINUE 18370
	END WHEN

18360	WHILE (WORK_EMPNUM$ = PR_REG_ERNDED::EMPNUM)

		SELECT PR_REG_ERNDED::ETYPE
		CASE "A"
			! Ignore
		CASE "P"
			IF QTR% = -1%
			THEN
				QTOTAL(0%) = FUNC_ROUND(QTOTAL(0%) + &
					PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
					FOR I% = 0% TO 3%
			ELSE
				QTOTAL(0%) = FUNC_ROUND(QTOTAL(0%) + &
					PR_REG_ERNDED::QTR_DOLL(QTR%), 2%)
			END IF
		CASE "D", "F"
			IF QTR% = -1%
			THEN
				QTOTAL(1%) = FUNC_ROUND(QTOTAL(1%) + &
					PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
					FOR I% = 0% TO 3%
			ELSE
				QTOTAL(1%) = FUNC_ROUND(QTOTAL(1%) + &
					PR_REG_ERNDED::QTR_DOLL(QTR%), 2%)
			END IF
		CASE ELSE
			IF QTR% = -1%
			THEN
				QTOTAL(2%) = FUNC_ROUND(QTOTAL(2%) + &
					PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
					FOR I% = 0% TO 3%
			ELSE
				QTOTAL(2%) = FUNC_ROUND(QTOTAL(2%) + &
					PR_REG_ERNDED::QTR_DOLL(QTR%), 2%)
			END IF
		END SELECT

		!
		! Summarize this item if non-taxable
		!
		NONTAX_CODE$ = PR_REG_ERNDED::ETYPE + PR_REG_ERNDED::CODE
		FEDERAL_AMOUNT = 0.0
		FICA_AMOUNT = 0.0
		STATE_AMOUNT = 0.0

		IF QTR% = -1%
		THEN
			FOR I% = 0% TO 3%
				FEDERAL_AMOUNT = PR_REG_ERNDED::QTR_DOLL(I%) &
					IF INSTR(1%, NONTAX_FEDERAL$, NONTAX_CODE$)
				FICA_AMOUNT = PR_REG_ERNDED::QTR_DOLL(I%) &
					IF INSTR(1%, NONTAX_FICA$, NONTAX_CODE$)
				STATE_AMOUNT = PR_REG_ERNDED::QTR_DOLL(I%) &
					IF INSTR(1%, NONTAX_STATE$, NONTAX_CODE$)
			NEXT I%
		ELSE
			FEDERAL_AMOUNT = PR_REG_ERNDED::QTR_DOLL(QTR%) &
				IF INSTR(1%, NONTAX_FEDERAL$, NONTAX_CODE$)
			FICA_AMOUNT = PR_REG_ERNDED::QTR_DOLL(QTR%) &
				IF INSTR(1%, NONTAX_FICA$, NONTAX_CODE$)
			STATE_AMOUNT = PR_REG_ERNDED::QTR_DOLL(QTR%) &
				IF INSTR(1%, NONTAX_STATE$, NONTAX_CODE$)
		END IF

		IF (FEDERAL_AMOUNT <> 0.0) OR (FICA_AMOUNT <> 0.0) OR &
			(STATE_AMOUNT <> 0.0)
		THEN
			GOSUB SummarizeNonTax
		END IF

		WHEN ERROR IN
			GET #PR_REG_ERNDED.CH%, REGARDLESS
		USE
			CONTINUE 18370
		END WHEN
	NEXT

18370	!
	! Taxes
	!
	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, KEY #0% EQ WORK_EMPNUM$, REGARDLESS
	USE
		CONTINUE 18390
	END WHEN

18380	WHILE (WORK_EMPNUM$ = PR_REG_TAXES::EMPNUM)

		SELECT PR_REG_TAXES::TTYPE
		CASE "FW"
			IF QTR% = -1%
			THEN
				QTOTAL(3%) = FUNC_ROUND(QTOTAL(3%) + &
					PR_REG_TAXES::TAX(I%), 2%) &
					FOR I% = 0% TO 3%
				QTOTAL(4%) = FUNC_ROUND(QTOTAL(4%) + &
					PR_REG_TAXES::REPORTABLE(I%), 2%) &
					FOR I% = 0% TO 3%
			ELSE
				QTOTAL(3%) = FUNC_ROUND(QTOTAL(3%) + &
					PR_REG_TAXES::TAX(QTR%), 2%)
				QTOTAL(4%) = FUNC_ROUND(QTOTAL(4%) + &
					PR_REG_TAXES::REPORTABLE(QTR%), 2%)
			END IF
		CASE "FI"
			IF QTR% = -1%
			THEN
				FOR I% = 0% TO 3%
					QTOTAL(5%) = FUNC_ROUND(QTOTAL(5%) + &
						PR_REG_TAXES::TAX(I%), 2%)
					QTOTAL(6%) = FUNC_ROUND(QTOTAL(6%) + &
						PR_REG_TAXES::REPORTABLE(I%), 2%)
				NEXT I%
			ELSE
				QTOTAL(5%) = FUNC_ROUND(QTOTAL(5%) + &
					PR_REG_TAXES::TAX(QTR%), 2%)
				QTOTAL(6%) = FUNC_ROUND(QTOTAL(6%) + &
					PR_REG_TAXES::REPORTABLE(QTR%), 2%)
			END IF
		CASE "FH"
			IF QTR% = -1%
			THEN
				QTOTAL(5%) = FUNC_ROUND(QTOTAL(5%) + &
					PR_REG_TAXES::TAX(I%), 2%) &
					FOR I% = 0% TO 3%
			ELSE
				QTOTAL(5%) = FUNC_ROUND(QTOTAL(5%) + &
					PR_REG_TAXES::TAX(QTR%), 2%)
			END IF
		CASE "SW"
			IF QTR% = -1%
			THEN
				FOR I% = 0% TO 3%
					QTOTAL(7%) = FUNC_ROUND(QTOTAL(7%) + &
						PR_REG_TAXES::TAX(I%), 2%)
					QTOTAL(8%) = FUNC_ROUND(QTOTAL(8%) + &
						PR_REG_TAXES::REPORTABLE(I%), 2%)
				NEXT I%
			ELSE
				QTOTAL(7%) = FUNC_ROUND(QTOTAL(7%) + &
					PR_REG_TAXES::TAX(QTR%), 2%)
				QTOTAL(8%) = FUNC_ROUND(QTOTAL(8%) + &
					PR_REG_TAXES::REPORTABLE(QTR%), 2%)
			END IF
		END SELECT

		WHEN ERROR IN
			GET #PR_REG_TAXES.CH%, REGARDLESS
		USE
			CONTINUE 18390
		END WHEN
	NEXT

18390	!
	! Output QTR totals
	!
	FLAG$ = "      "
	FLAG$ = "******" &
		IF QTOTAL(I%) <> EMP_TOTAL(I%) FOR I% = 0% TO 8%

	TEXT$ = FLAG$ + SPACE$(10%) + "Quarterly Total" + SPACE$(11%) + &
		FORMAT$(QTOTAL(0%), "######.##") + &
		FORMAT$(QTOTAL(1%), "######.##") + &
		FORMAT$(QTOTAL(2%), "######.##") + &
		FORMAT$(QTOTAL(3%), "#######.##") + &
		FORMAT$(QTOTAL(4%), "######.##") + &
		FORMAT$(QTOTAL(5%), "######.##") + &
		FORMAT$(QTOTAL(6%), "######.##") + &
		FORMAT$(QTOTAL(7%), "######.##") + &
		FORMAT$(QTOTAL(8%), "######.## ") + &
		FLAG$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	IF FLAG$ <> ""
	THEN
		TEXT$ = FLAG$ + SPACE$(10%) + &
			"Difference     " + SPACE$(11%) + &
			FORMAT$(EMP_TOTAL(0%) - QTOTAL(0%), "######.##") + &
			FORMAT$(EMP_TOTAL(1%) - QTOTAL(1%), "######.##") + &
			FORMAT$(EMP_TOTAL(2%) - QTOTAL(2%), "######.##") + &
			FORMAT$(EMP_TOTAL(3%) - QTOTAL(3%), "#######.##") + &
			FORMAT$(EMP_TOTAL(4%) - QTOTAL(4%), "######.##") + &
			FORMAT$(EMP_TOTAL(5%) - QTOTAL(5%), "######.##") + &
			FORMAT$(EMP_TOTAL(6%) - QTOTAL(6%), "######.##") + &
			FORMAT$(EMP_TOTAL(7%) - QTOTAL(7%), "######.##") + &
			FORMAT$(EMP_TOTAL(8%) - QTOTAL(8%), "######.## ") + &
			FLAG$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)


 PrintEmployeeTotal1:
	WORK_EMPNUM$ = PR_TEMP::EMPLOYEE
	NUMBER_OF_LINES% = 0%
	EMP_TOTAL(I%) = 0.0 FOR I% = 0% TO 8%

	RETURN

	%Page

 SDTotal:
	!******************************************************************
	! Print Total for employee
	!******************************************************************

	IF SD_EMP_COUNT% <> 0%
	THEN
		TEXT$ = SPACE$(10%) + "Department " + THIS_SD$ + " Total" + &
			FORMAT$(SD_EMP_COUNT%, "#### Emps") + &
			SPACE$(4%) + &
			FORMAT$(SD_TOTAL(0%), "######.##") + &
			FORMAT$(SD_TOTAL(1%), "######.##") + &
			FORMAT$(SD_TOTAL(2%), "######.##") + &
			FORMAT$(SD_TOTAL(3%), "#######.##") + &
			FORMAT$(SD_TOTAL(4%), "######.##") + &
			FORMAT$(SD_TOTAL(5%), "######.##") + &
			FORMAT$(SD_TOTAL(6%), "######.##") + &
			FORMAT$(SD_TOTAL(7%), "######.##") + &
			FORMAT$(SD_TOTAL(8%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

	THIS_SD$ = PR_EMP_MASTER::DEPT
	SD_EMP_COUNT% = 0%
	SD_TOTAL(I%) = 0.0 FOR I% = 0% TO 8%
	SD_EMP_COUNT% = 0%

	RETURN

	%Page

 SLTotal:
	!******************************************************************
	! Print Total for Location
	!******************************************************************

	IF SL_EMP_COUNT% <> 0%
	THEN
		TEXT$ = SPACE$(8%) + "Location " + THIS_SL$ + " Total" + &
			FORMAT$(SL_EMP_COUNT%, "#### Emps") + &
			SPACE$(6%) + &
			FORMAT$(SL_TOTAL(0%), "######.##") + &
			FORMAT$(SL_TOTAL(1%), "######.##") + &
			FORMAT$(SL_TOTAL(2%), "######.##") + &
			FORMAT$(SL_TOTAL(3%), "#######.##") + &
			FORMAT$(SL_TOTAL(4%), "######.##") + &
			FORMAT$(SL_TOTAL(5%), "######.##") + &
			FORMAT$(SL_TOTAL(6%), "######.##") + &
			FORMAT$(SL_TOTAL(7%), "######.##") + &
			FORMAT$(SL_TOTAL(8%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	END IF

	IF SORTBY$ = "LO"
	THEN
		IF THATSIT$ = "END"
		THEN
			TITLE$(3%) = SB_TEXT$
		ELSE
			IF SL_EMP_COUNT% <> 0%
			THEN
				TITLE$(3%) = "For Location: " + &
					PR_EMP_MASTER::LOCATION
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					"", 999%)
			ELSE
				TITLE$(3%) = "For Location: " + &
					PR_EMP_MASTER::LOCATION
			END IF
		END IF
	END IF

	THIS_SL$ = PR_EMP_MASTER::LOCATION
	SL_EMP_COUNT% = 0%
	SL_TOTAL(I%) = 0.0 FOR I% = 0% TO 8%
	SL_EMP_COUNT% = 0%

	RETURN

	%Page

 SSTotal:
	!******************************************************************
	! Print Total for state
	!******************************************************************

	IF SS_EMP_COUNT% <> 0%
	THEN
		TEXT$ = SPACE$(10%) + "State " + THIS_SS$ + " Total" + &
			FORMAT$(SS_EMP_COUNT%, "#### Emps") + &
			SPACE$(11%) + &
			FORMAT$(SS_TOTAL(0%), "######.##") + &
			FORMAT$(SS_TOTAL(1%), "######.##") + &
			FORMAT$(SS_TOTAL(2%), "######.##") + &
			FORMAT$(SS_TOTAL(3%), "#######.##") + &
			FORMAT$(SS_TOTAL(4%), "######.##") + &
			FORMAT$(SS_TOTAL(5%), "######.##") + &
			FORMAT$(SS_TOTAL(6%), "######.##") + &
			FORMAT$(SS_TOTAL(7%), "######.##") + &
			FORMAT$(SS_TOTAL(8%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

	THIS_SS$ = PR_EMP_MASTER::TAX_PKG
	SS_EMP_COUNT% = 0%
	SS_TOTAL(I%) = 0.0 FOR I% = 0% TO 8%

	RETURN

	%Page

18600	!*******************************************************************
	! Summarize non-taxable amounts
	!*******************************************************************
 SummarizeNonTax:

	!
	! Look for existing code
	!
	GOTO 18610 &
		IF NONTAX_LIST(Z%)::PDCODE = NONTAX_CODE$ &
		FOR Z% = 1% TO NONTAX_LIST%

	!
	! Create a new one
	!
	Z%, NONTAX_LIST% = NOTAX_LIST% + 1%
	NONTAX_LIST(Z%)::PDCODE = NONTAX_CODE$
	NONTAX_LIST(Z%)::FEDERAL = FEDERAL_AMOUNT
	NONTAX_LIST(Z%)::FICA = FICA_AMOUNT
	NONTAX_LIST(Z%)::STATE = STATE_AMOUNT

	GOTO 18690

18610	NONTAX_LIST(Z%)::FEDERAL = NONTAX_LIST(Z%)::FEDERAL + FEDERAL_AMOUNT
	NONTAX_LIST(Z%)::FICA = NONTAX_LIST(Z%)::FICA + FICA_AMOUNT
	NONTAX_LIST(Z%)::STATE = NONTAX_LIST(Z%)::STATE + STATE_AMOUNT

18690	RETURN

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
