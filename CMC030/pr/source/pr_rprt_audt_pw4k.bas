1	%TITLE "Payroll Net Check Audit Report"
	%SBTTL "PR_RPRT_AUDT_PW4K"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2003 by
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
	! ID:PR050
	!
	! Abstract:HELP
	!	.p
	!	The ^*Employee Check Audit report\* may be composed for any time period. It
	!	contains the period hours, gross pay, other pay, tax and non-tax with holdings,
	!	and totals for employees and periods. The columnar headings include the
	!	following:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Name
	!	.le
	!	Date
	!	.le
	!	Hours
	!	.le
	!	Units
	!	.le
	!	Gross Pay
	!	.le
	!	Non-Comp Pay
	!	.le
	!	FICA
	!	.le
	!	Federal
	!	.le
	!	State
	!	.le
	!	Miscellaneous/Deductions
	!	.le
	!	Net Check
	!	.le
	!	Check Number
	!	.els
	!
	! Index:
	!	.x Report>Employee Check Audit
	!	.x Report>Employee Check Audit
	!	.x Employee Check Audit>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_PW4K/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_AUDT_PW4K, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_PW4K.OBJ;*
	!
	! Author:
	!
	!	11/20/2003 - Kevin Handy
	!
	! Modification history:
	!
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

	RECORD PR_TEMP_RECORD
		STRING	SORT = 26%
		STRING	EMPLOYEE = 10%
		STRING	PRDATE = 8%
		GFLOAT	AMOUNT(9%)
		STRING	CHECK = 6%
	END RECORD

	MAP (PR_TEMP) PR_TEMP_RECORD PR_TEMP

	DECLARE INTEGER CONSTANT FILE_MAX = 2000

	!
	! Dimension
	!
	DIM DATA_FILE$(FILE_MAX), &
		GRAND_TOTAL(FILE_MAX, 9%), &
		EMP_TOTAL(9%), &
		LINE_TOTAL(9%), &
		SD_TOTAL(9%), &
		SL_TOTAL(9%), &
		SS_TOTAL(9%), &
		TOTAL(9%)

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
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$) ! Reformat to (YYYYMMDD)

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
	!	The ^*From Item\* field specifies the beginning item.
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
	!	The ^*To Item\* field specifies the ending item.
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
	!	The ^*Sort\* field specifies the report is to
	!	be sorted in the indicated manner.
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

	WILD_401K$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(05) Show Information\*
	!	.p
	!	The ^*Show Information\* field specifies
	!	what additional employee information should be
	!	shown on the report.
	!	"*N" specifies no additional information.
	!	"*S" or "*Y" causes the Social Security Number to be displayed.
	!	"*4" causes information useful for auditing 401k plans, including
	!	Sex, Birthdate, Start Date, Termination Date, and Social Security Number.
	!
	! Index:
	!	.x Show Information>Employee Check Audit Report
	!	.x Employee Check Audit Report>Show Information
	!
	!--

	PCT1 = VAL(UTL_REPORTX::OPTDEF(6%)) / 100.0

	!++
	! Abstract:FLD07
	!	^*(07) Print Totals Only\*
	!	.p
	!	The ^*Print Totals Only\* field specifies that
	!	only employee totals should be printed (*Y), or detail for each pay
	!	date (*N), or only the final Grand Totals should be printed (*G).
	!
	! Index:
	!	.x Print Totals>Employee Check Audit Report
	!	.x Employee Check Audit Report>Print Totals
	!
	!--

	PCT2 = VAL(UTL_REPORTX::OPTDEF(7%)) / 100.0

	!++
	! Abstract:FLD08
	!	^*(08) Page After Employee\*
	!	.p
	!
	! Index:
	!	.x Page After>Employee Check Audit Report
	!	.x Employee Check Audit Report>Page After
	!
	! Datatype:YN
	! Size:1
	! Valid Input: Y,N
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

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Net Check Audit Report"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" To: " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = SB_TEXT$
	TITLE$(4%) = ""

	!
	! Define headers
	!
	TITLE$(5%) = "                                                 " + &
		"                                   Employee Employer"
	TITLE$(6%) = "Emp #      SSN          Name                     " + &
		"Date        Reg      Ovt     Gross      401    Match"
	TITLE$(7%) = ""

	!
	! Set up line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:033,DDate:042,VHours:051" + &
		"VUnits:060,VGrossPay:070,VNonComp:079,VFICA:088," + &
		"VFederal:097,VState:106,VMiscDed:115,VNetCheck:125,$ChkNum:132"

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
		LINE_TOTAL(I%) = 0.0 FOR I% = 1% TO 9%

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

17320		TEST_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

		GOTO 17310 IF PR_TRN_PAY::PTYPE = "A"

		IF UNIT_HOUR$ = "H"
		THEN
			LINE_TOTAL(1%) = FUNC_ROUND(LINE_TOTAL(1%) + &
				PR_TRN_PAY::REG_HR, 2%)
			LINE_TOTAL(2%) = FUNC_ROUND(LINE_TOTAL(2%) + &
				PR_TRN_PAY::OVT_HR, 2%)
		ELSE
			LINE_TOTAL(1%) = FUNC_ROUND(LINE_TOTAL(1%) + &
				PR_TRN_PAY::REG_HR + &
				PR_TRN_PAY::OVT_HR, 2%)
			LINE_TOTAL(2%) = FUNC_ROUND(LINE_TOTAL(2%) + &
				PR_TRN_PAY::PIECE, 2%)
		END IF

		LINE_TOTAL(3%) = FUNC_ROUND(LINE_TOTAL(3%) + &
			PR_TRN_PAY::GROSS, 2%) &

		GOTO 17310

17330		WHEN ERROR IN
			FIND #PR_TRN_DED.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM + &
				TEST_PR_END_DATE$, &
				REGARDLESS
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

		IF PR_TRN_DED::DTYPE = "D"
		THEN
			IF COMP_STRING(PR_TRN_DED::CODE, WILD_401K$)
			THEN
				LINE_TOTAL(4%) = FUNC_ROUND(LINE_TOTAL(4%) + &
					PR_TRN_DED::AMOUNT, 2%)
			END IF
		END IF

		GOTO 17340

17350		!
		! Get the check number now
		!
		PR_TRN_CHECK::PR_END_DATE	= ""
		PR_TRN_CHECK::CHECK		= ""
		PR_TRN_CHECK::CHECK_DATE	= ""
		PR_TRN_CHECK::PAYFREQ		= 0%

		LINE_TOTAL(9%) = LINE_TOTAL(9%) + 1.0

		WHEN ERROR IN
			GET #PR_TRN_CHECK.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM + &
				TEST_PR_END_DATE$, &
				REGARDLESS
		USE
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

		LINE_TOTAL(5%) = FUNC_ROUND(MIN(LINE_TOTAL(3%) * PCT1 * PCT2, &
			LINE_TOTAL(4%) * PCT1), 2%)

		PR_TEMP::EMPLOYEE = WORK_EMPNUM$
		PR_TEMP::PRDATE = BATCH_NO$
		PR_TEMP::AMOUNT(X%) = LINE_TOTAL(X%) FOR X% = 1% TO 9%
		PR_TEMP::CHECK = PR_TRN_CHECK::CHECK

		PUT #PR_TEMP.CH%

		GRAND_TOTAL(PR_LOOP%, LOOP%) = GRAND_TOTAL(PR_LOOP%, LOOP%) + &
			LINE_TOTAL(LOOP%) FOR LOOP% = 1% TO 9%

		GOTO ExitTotal IF UTL_REPORTX::STAT

		LINE_TOTAL(I%) = 0.0 FOR I% = 1% TO 9%

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

17410	GOTO ExitTotal IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #PR_TEMP.CH%
	USE
		CONTINUE ExitTotal
	END WHEN

17415	IF WORK_EMPNUM$ <> PR_TEMP::EMPLOYEE
	THEN
		GOSUB PrintEmployeeTotal

		IF PR_EMP_MASTER::EMPNUM <> PR_TEMP::EMPLOYEE
		THEN
			WHEN ERROR IN
				GET #PR_EMP_MASTER.CH%, &
					KEY #0% EQ PR_TEMP::EMPLOYEE, &
					REGARDLESS
			USE
				PR_EMP_MASTER::EMPNUM = PR_TEMP::EMPLOYEE
				PR_EMP_MASTER::EMPNAME = "** Undefined **"
				CONTINUE 17420
			END WHEN
		END IF
	END IF

17420	SELECT SORTBY$

	CASE "SD"
		IF THIS_SS$ <> PR_EMP_MASTER::TAX_PKG
		THEN
			GOSUB SDTotal
			GOSUB SLTotal
			GOSUB SSTotal
		END IF
		IF THIS_SS$ + THIS_SL$ <> &
			PR_EMP_MASTER::TAX_PKG + PR_EMP_MASTER::LOCATION
		THEN
			GOSUB SDTotal
			GOSUB SLTotal
		END IF
		GOSUB SDTotal IF THIS_SS$ + THIS_SL$ + THIS_SD$ <> &
			PR_EMP_MASTER::TAX_PKG + &
			PR_EMP_MASTER::LOCATION + &
			PR_EMP_MASTER::DEPT

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

	TEMP$ = "Grand Total "

	TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 9%

	FOR PR_LOOP% = 1% TO DATA_FILE%

		TEXT$ = LEFT(SPACE$(10%) + TEMP$ + &
			NUM1$(GRAND_TOTAL(PR_LOOP%, 9%)) + " Emps" + &
			SPACE$(34%), 34%) + &
			SPACE$(12%) + " " + &
			PRNT_DATE(DATA_FILE$(PR_LOOP%), 6%) + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 1%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 2%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 3%), "#######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 4%), "######.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 5%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOTAL(LOOP%) = TOTAL(LOOP%) + GRAND_TOTAL(PR_LOOP%, LOOP%) &
			FOR LOOP% = 1% TO 9%

	NEXT PR_LOOP%

	!
	! Print final total
	!
	TEMP$ = SPACE$(20%) + "All Payrolls " + NUM1$(TT_EMP_COUNT%) + " Emps"

	TEXT$ = LEFT(TEMP$ + SPACE$(56%), 54%) + &
		FORMAT$(TOTAL(1%), "#######.##") + &
		FORMAT$(TOTAL(2%), "######.##") + &
		FORMAT$(TOTAL(3%), "#######.##") + &
		FORMAT$(TOTAL(4%), "######.##") + &
		FORMAT$(TOTAL(5%), "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "Total of " + NUM1$(TT_EMP_COUNT%) + " employees paid"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

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


18210	EMP_TOTAL(LOOP%) = EMP_TOTAL(LOOP%) + PR_TEMP::AMOUNT(LOOP%) &
		FOR LOOP% = 1% TO 9%
	SD_TOTAL(LOOP%) = SD_TOTAL(LOOP%) + PR_TEMP::AMOUNT(LOOP%) &
		FOR LOOP% = 1% TO 9%
	SL_TOTAL(LOOP%) = SL_TOTAL(LOOP%) + PR_TEMP::AMOUNT(LOOP%) &
		FOR LOOP% = 1% TO 9%
	SS_TOTAL(LOOP%) = SS_TOTAL(LOOP%) + PR_TEMP::AMOUNT(LOOP%) &
		FOR LOOP% = 1% TO 9%

	TEXT$ = PR_TEMP::EMPLOYEE + " " + &
		PR_EMP_MASTER::SSN + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME + SPACE$(22%), 22%) + " " + &
		PRNT_DATE(PR_TEMP::PRDATE, 6%) + &
		FORMAT$(PR_TEMP::AMOUNT(1%), "######.##") + &
		FORMAT$(PR_TEMP::AMOUNT(2%), "######.##") + &
		FORMAT$(PR_TEMP::AMOUNT(3%), "#######.##") + &
		FORMAT$(PR_TEMP::AMOUNT(4%), "######.##") + &
		FORMAT$(PR_TEMP::AMOUNT(5%), "######.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NUMBER_OF_LINES% = NUMBER_OF_LINES% + 1%

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
	IF (NUMBER_OF_LINES% > 1%) OR (TOTALFLAG$ = "Y")
	THEN
		TEXT$ = SPACE$(16%) + "Employee Total" + SPACE$(25%) + &
			FORMAT$(EMP_TOTAL(1%), "######.##") + &
			FORMAT$(EMP_TOTAL(2%), "######.##") + &
			FORMAT$(EMP_TOTAL(3%), "#######.##") + &
			FORMAT$(EMP_TOTAL(4%), "######.##") + &
			FORMAT$(EMP_TOTAL(5%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	END IF

	IF (NUMBER_OF_LINES% >= 1%)
	THEN
		SD_EMP_COUNT% = SD_EMP_COUNT% + 1%
		SL_EMP_COUNT% = SL_EMP_COUNT% + 1%
		SS_EMP_COUNT% = SS_EMP_COUNT% + 1%
		TT_EMP_COUNT% = TT_EMP_COUNT% + 1%

	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

 PrintEmployeeTotal1:
	WORK_EMPNUM$ = PR_TEMP::EMPLOYEE
	NUMBER_OF_LINES% = 0%
	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 9%

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
			SPACE$(30%) + &
			FORMAT$(SD_TOTAL(1%), "######.##") + &
			FORMAT$(SD_TOTAL(2%), "######.##") + &
			FORMAT$(SD_TOTAL(3%), "#######.##") + &
			FORMAT$(SD_TOTAL(4%), "######.##") + &
			FORMAT$(SD_TOTAL(5%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

	THIS_SD$ = PR_EMP_MASTER::DEPT
	SD_EMP_COUNT% = 0%
	SD_TOTAL(I%) = 0.0 FOR I% = 1% TO 9%
	SD_EMP_COUNT% = 0%

	RETURN

	%Page

 SLTotal:
	!******************************************************************
	! Print Total for Location
	!******************************************************************

	IF SL_EMP_COUNT% <> 0%
	THEN
		TEXT$ = SPACE$(10%) + "Location " + THIS_SL$ + " Total" + &
			FORMAT$(SL_EMP_COUNT%, "#### Emps") + &
			SPACE$(19%) + &
			FORMAT$(SL_TOTAL(1%), "######.##") + &
			FORMAT$(SL_TOTAL(2%), "######.##") + &
			FORMAT$(SL_TOTAL(3%), "#######.##") + &
			FORMAT$(SL_TOTAL(4%), "######.##") + &
			FORMAT$(SL_TOTAL(5%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
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
	SL_TOTAL(I%) = 0.0 FOR I% = 1% TO 9%
	SL_EMP_COUNT% = 0%

	RETURN

	%Page

 SSTotal:
	!******************************************************************
	! Print Total for employee
	!******************************************************************

	IF SS_EMP_COUNT% <> 0%
	THEN
		TEXT$ = SPACE$(10%) + "State " + THIS_SS$ + " Total" + &
			FORMAT$(SS_EMP_COUNT%, "#### Emps") + &
			SPACE$(24%) + &
			FORMAT$(SS_TOTAL(1%), "######.##") + &
			FORMAT$(SS_TOTAL(2%), "######.##") + &
			FORMAT$(SS_TOTAL(3%), "#######.##") + &
			FORMAT$(SS_TOTAL(4%), "######.##") + &
			FORMAT$(SS_TOTAL(5%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

	THIS_SS$ = PR_EMP_MASTER::TAX_PKG
	SS_EMP_COUNT% = 0%
	SS_TOTAL(I%) = 0.0 FOR I% = 1% TO 9%

	RETURN

	%Page

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
