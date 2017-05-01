1	%TITLE "Payroll Time Audit Report"
	%SBTTL "PR_RPRT_AUDT_TIMEBL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:PR052
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee Time/Rate/Unit Audit report\* shows how the company time is
	!	distributed and gives the gross totals for the time, rate, and units.
	!	.b
	!	The following fields are included:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Payroll Date
	!	.te
	!	Account
	!	.te
	!	Sub Account
	!	.te
	!	Operation
	!	.te
	!	Earnings Code
	!	.te
	!	Pay Type
	!	.te
	!	Rate Code
	!	.te
	!	Hours Rate
	!	.te
	!	Regular Hours
	!	.te
	!	Overtime Hours
	!	.te
	!	Overtime Factor
	!	.te
	!	Unit Rate
	!	.te
	!	Unit Quantity
	!	.te
	!	Gross
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Employee Time/Rate/Unit Audit
	!	.x Report>Employee Time/Rate/Unit Audit
	!	.x Employee Time/Rate/Unit Audit>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_TIMEBL/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_AUDT_TIMEBL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_TIMEBL.OBJ;*
	!
	! Author:
	!
	!	03/02/2000 - Kevin Handy
	!
	! Modification history:
	!
	!	12/01/2000 - Kevin Handy
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
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	RECORD PR_TEMPORARY_CDD
		STRING	SORTBY = 16%
		STRING	EMPNUM = 10%
		STRING	EMPNAME = 30%
		STRING	PR_END_DATE = 8%
		STRING	ACCT = 18%
		STRING	SUBACC = 10%
		STRING	OPER = 8%
		STRING	CODE = 2%
		STRING	PTYPE = 1%
		STRING	RTYPE = 1%
		REAL	HOUR_RATE
		REAL	REG_HR
		REAL	OVT_HR
		INTEGER	FACTOR
		REAL	PIECE_RATE
		REAL	PIECE
		REAL	GROSS
	END RECORD

	MAP (PR_TEMPORARY) PR_TEMPORARY_CDD PR_TEMPORARY

	DECLARE INTEGER CONSTANT FILE_MAX = 2000

	!
	! Dimension
	!
	DIM DATA_FILE$(FILE_MAX)

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
	!	.ts 55
	!	^*(01) From Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*From Payroll Date\* field enters
	!	the payroll date with which the report is to begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	date in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Payroll Date>Employee Time/Rate/Unit Audit Report
	!	.x Employee Time/Rate/Unit Audit Report>From Payroll Date
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(02) To Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*To Payroll Date\* field enters the date
	!	with which the report is to end printing.
	!	.b
	!	A blank field will
	!	cause the report to print to the end of the file.
	!	.lm -5
	!
	! Index:
	!	.x To Payroll Date>Employee Time/Rate/Unit Audit Report
	!	.x Employee Time/Rate/Unit Audit Report>To Payroll Date
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters a
	!	item from which the report will start printing.
	!	The value must be in agreement with
	!	field (05).
	!	.b
	!	A blank field will cause the report to begin with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Employee Time/Rate/Unit Audit Report
	!	.x Employee Time/Rate/Unit Audit Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters a
	!	item at which the report will end printing.
	!	The value must be in agreement with
	!	field (05).
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Employee Time/Rate/Unit Audit Report
	!	.x Employee Time/Rate/Unit Audit Report>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Sort by	NU, NA, LO\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field enters a code which
	!	causes the report to be sorted by the indicated manner.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*NU\*	Number
	!	.te
	!	^*NA\*	Name
	!	.te
	!	^*LO\*	Location
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Employee Time/Rate/Unit Audit Report
	!	.x Employee Time/Rate/Unit Audit Report>Sort By
	!
	!--

	WILDCARD$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	.ts 55
	!	^*(06) Wildcard pay Code\*
	!	.lm -5
	!
	! Index:
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

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

	GOTO ExitProgram IF DATA_FILE% = 0%

300	!
	! Open employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open temporary work file
	!
	CALL ASSG_CHANNEL(PR_TEMPORARY.CH%, STATUS%)
	OPEN "PR_TEMPORARY.TMP" FOR OUTPUT AS FILE PR_TEMPORARY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TEMPORARY, &
		PRIMARY KEY (PR_TEMPORARY::SORTBY, PR_TEMPORARY::PR_END_DATE) &
			DUPLICATES, &
		TEMPORARY, &
		BUFFER 32%, &
		ACCESS MODIFY, &
		ALLOW NONE

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Time/Unit Audit Report"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + " To: " + &
		PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Set the headings
	!
	TITLE$(4%) = SPACE$(70%) + &
		"--------------Hour-------------  -------Units-------"
	TITLE$(5%) = "Emp#  Payroll Date  Account            SubAcct    " + &
		"Oper     EC  PT  RC     Rate  Reg Hrs  Ovt Hrs Fac        " + &
		"Rate       Qty     Gross"
	TITLE$(6%) = ""

	!
	! Define line layouts
	!
	LYT_LINE$ = "DPRDate:016,$Acct:038,$SubAcct:049,$Oper:058," + &
		"$Code:061,$PType:064,$RType:068,VHrRate:078,VRegHrs:087," + &
		"VOvtHrs:096,VOvtFactor:100,VPcRate:112,VPcQty:122,VGross:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	!
	! Look up employee in all payroll files selected
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

		CALL ENTR_3MESSAGE(SCOPE, &
			"Processing folder " + &
			PRNT_DATE(BATCH_NO$, 8%), 1%)

17030		!
		! Open Pay folder
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			CONTINUE 17040 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

		GOTO 17100

17040		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

17100		!
		! Get pay detail information
		!
		WHEN ERROR IN
			IF (FROM_ITEM$ <> "") AND (SORTBY$ = "NU")
			THEN
				FIND #PR_TMP_PAY.CH%, &
					KEY #0% GE FROM_ITEM$
			ELSE
				RESET #PR_TMP_PAY.CH%
			END IF
		USE
			CONTINUE 17140 IF ERR = 155%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

17110		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 17140 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		!
		! If history then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		GOTO 17110 IF PR_TRN_PAY::PTYPE = "A"

17120		IF PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM
		THEN
			WHEN ERROR IN
				GET #PR_EMP_MASTER.CH%, &
					KEY #0% EQ PR_TRN_PAY::EMPNUM, &
					REGARDLESS
			USE
				IF ERR = 155%
				THEN
					PR_EMP_MASTER::EMPNUM = PR_TRN_PAY::EMPNUM
					PR_EMP_MASTER::EMPNAME = ""
					PR_EMP_MASTER::SSN = ""
					PR_EMP_MASTER::DEPT = ""
					PR_EMP_MASTER::SORT = ""
					CONTINUE 17125
				END IF
				FILENAME$ = "PR_EMP_MASTER"
				CONTINUE HelpError
			END WHEN
		END IF

17125		!
		! Check current record
		!
		SELECT SORTBY$
		CASE "NU"
			GOTO 17140 IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			SORTS$ = PR_EMP_MASTER::EMPNUM

		CASE "NA"
			GOTO 17110 IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			GOTO 17110 IF (PR_EMP_MASTER::EMPNAME < FROM_ITEM$)
			SORTS$ = PR_EMP_MASTER::EMPNAME

		CASE "SN"
			GOTO 17110 IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			GOTO 17110 IF (PR_EMP_MASTER::SSN < FROM_ITEM$)
			SORTS$ = PR_EMP_MASTER::SSN

		CASE "DP"
			GOTO 17110 IF (PR_EMP_MASTER::DEPT > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			GOTO 17110 IF (PR_EMP_MASTER::DEPT < FROM_ITEM$)
			SORTS$ = PR_EMP_MASTER::DEPT

		CASE ELSE
			GOTO 17110 IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			GOTO 17110 IF (PR_EMP_MASTER::SORT < FROM_ITEM$)
			SORTS$ = PR_EMP_MASTER::SORT

		END SELECT

		IF (WILDCARD$ <> "")
		THEN
			GOTO 17110 &
				IF COMP_STRING(PR_TRN_PAY::CODE, WILDCARD$) = 0%
		END IF

		!
		! Create temporary record
		!
		PR_TEMPORARY::SORTBY	= SORTS$
		PR_TEMPORARY::EMPNUM	= PR_TRN_PAY::EMPNUM
		PR_TEMPORARY::EMPNAME	= PR_EMP_MASTER::EMPNAME
		PR_TEMPORARY::PR_END_DATE = PR_TRN_PAY::PR_END_DATE
		PR_TEMPORARY::ACCT	= PR_TRN_PAY::ACCT
		PR_TEMPORARY::SUBACC	= PR_TRN_PAY::SUBACC
		PR_TEMPORARY::OPER	= PR_TRN_PAY::OPER
		PR_TEMPORARY::CODE	= PR_TRN_PAY::CODE
		PR_TEMPORARY::PTYPE	= PR_TRN_PAY::PTYPE
		PR_TEMPORARY::RTYPE	= PR_TRN_PAY::RTYPE
		PR_TEMPORARY::HOUR_RATE	= PR_TRN_PAY::HOUR_RATE
		PR_TEMPORARY::REG_HR	= PR_TRN_PAY::REG_HR
		PR_TEMPORARY::OVT_HR	= PR_TRN_PAY::OVT_HR
		PR_TEMPORARY::FACTOR	= PR_TRN_PAY::FACTOR
		PR_TEMPORARY::PIECE_RATE= PR_TRN_PAY::PIECE_RATE
		PR_TEMPORARY::PIECE	= PR_TRN_PAY::PIECE
		PR_TEMPORARY::GROSS	= PR_TRN_PAY::GROSS

		PUT #PR_TEMPORARY.CH%

		GOTO 17110

17140		CLOSE #PR_TMP_PAY.CH%

	NEXT PR_LOOP%

17150	!
	! Now, print report from temporary file
	!
	RESET #PR_TEMPORARY.CH%

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%
	EMP_LINE_COUNTER% = 0%
	THIS_EMPLOYEE$ = ""

17160	WHEN ERROR IN
		GET #PR_TEMPORARY.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF (THIS_EMPLOYEE$ <> PR_TEMPORARY::EMPNUM)
	THEN
		GOSUB EmployeeTotal

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			PR_TEMPORARY::EMPNUM + " " + &
			PR_TEMPORARY::EMPNAME, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		THIS_EMPLOYEE$ = PR_TEMPORARY::EMPNUM
	END IF

	!
	! Print employee record
	!
	TEXT$ = "      " + &
		PRNT_DATE(PR_TEMPORARY::PR_END_DATE, 8%) + "    " + &
		PR_TEMPORARY::ACCT + " " + &
		PR_TEMPORARY::SUBACC + " " + &
		PR_TEMPORARY::OPER + " " + &
		PR_TEMPORARY::CODE + "  " + &
		PR_TEMPORARY::PTYPE + "   " + &
		PR_TEMPORARY::RTYPE + "  " + &
		"         " + &
		FORMAT$(PR_TEMPORARY::REG_HR, "#####.## ") + &
		FORMAT$(PR_TEMPORARY::OVT_HR, "#####.## ") + &
		FORMAT$(PR_TEMPORARY::FACTOR, "###%  ") + &
		"          " + &
		FORMAT$(PR_TEMPORARY::PIECE, "#####.### ")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	EMP_LINE_COUNTER% = EMP_LINE_COUNTER% + 1%

	GOTO ExitProgram IF UTL_REPORTX::STAT

	EMP_TOTAL(1%) = EMP_TOTAL(1%) + PR_TEMPORARY::REG_HR
	EMP_TOTAL(2%) = EMP_TOTAL(2%) + PR_TEMPORARY::OVT_HR
	EMP_TOTAL(3%) = EMP_TOTAL(3%) + PR_TEMPORARY::PIECE
	EMP_TOTAL(4%) = EMP_TOTAL(4%) + PR_TEMPORARY::GROSS

17350	!
	! Go to next record
	!
	GOTO 17160

 EmployeeTotal:
	!
	! Print employee total
	!
	IF EMP_LINE_COUNTER% > 1%
	THEN
		TEXT$ = SPACE$(55%) + "Employee Total          " + &
			FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
			FORMAT$(EMP_TOTAL(2%), "#####.##                 ") + &
			FORMAT$(EMP_TOTAL(3%), "#####.### ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Print extra line if employee counter if greater than 0
	!
	IF EMP_LINE_COUNTER% > 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + EMP_TOTAL(I%) &
		FOR I% = 1% TO 4%

	EMP_TOTAL(I%) = 0.0 FOR I% = 0% TO 4%

	EMP_LINE_COUNTER% = 0%

	RETURN

 ExitTotal:
	GOSUB EmployeeTotal

	!
	! Handle end of report
	!
	TEXT$ = SPACE$(55%) + "Grand Total             " + &
		FORMAT$(GRAND_TOTAL(1%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(GRAND_TOTAL(3%), "#####.### ")

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
