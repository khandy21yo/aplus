1	%TITLE "Payroll Audit Quarter Report"
	%SBTTL "PR_RPRT_KB401K"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:PR050
	!
	! Abstract:HELP
	!	.p
	!	This is a specialized report to display 401k information.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_KB401K/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_KB401K, -
	!	FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_KB401K.OBJ;*
	!
	! Author:
	!
	!	02/23/2000 - Kevin Handy
	!		Based upon PR_RPRT_PW401K
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
		GFLOAT	AMOUNT(8%)
		STRING	CHECK = 6%
	END RECORD

	MAP (PR_TEMP)	PR_TEMP_RECORD	PR_TEMP

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
	!	The ^*Start Payroll Date\* field is to contain the date of
	!	the payroll folder with which the report will begin printing.
	!	A blank field will cause the report to start
	!	with the first payroll folder date in the file.
	!
	! Index:
	!	.x Start Payroll Date>401k Audit Report
	!	.x 401k Audit Report>Start Payroll Date
	!
	! Datatype:DATE
	! Size:8
	! Required:Y
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	^*(02) End Payroll Date\*
	!	.p
	!	The ^*End Payroll Date\* field is to contain the date of
	!	the payroll folder with which the report is to end printing.
	!	A blank field will cause the report to end
	!	with the last payroll folder date in the file.
	!
	! Index:
	!	.x End Payroll Date>401k Audit Report
	!	.x 401k Audit Report>End Payroll Date
	!
	! Datatype:DATE
	! Size:8
	! Required:Y
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* entered in this field will cause the printing
	!	to begin with a particular item.
	!	The value must be in agreement with field
	!	(05).
	!	.p
	!	A blank field will cause the report to begin with the first item in
	!	the file.
	!
	! Index:
	!	.x From Item>401k Audit Report
	!	.x 401k Audit Report>From Item
	!
	! Datatype:TEXT
	! Size:20
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* entered in this field will cause the printing
	!	to end with a particular item.
	!	The value must be in agreement with field
	!	(05).
	!	.p
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!
	! Index:
	!	.x To Item>401k Audit Report
	!	.x 401k Audit Report>To Item
	!
	! Datatype:TEXT
	! Size:20
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort (NU, NA, LO, SO, SD)\*
	!	.p
	!	The ^*Sort \* field enters a code which
	!	causes the report to be sorted in the indicated manner.
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
	!	.x Sort>401k Audit Report
	!	.x 401k Audit Report>Sort
	!
	!--

	K4WILD$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(05) Wildcard 401(k) code(s)\*
	!	.p
	!	This field specifies what deduction code is used
	!	for 401(k) entries.
	!
	! Index:
	!	.x Sort>401k Audit Report
	!	.x 401k Audit Report>Sort
	!
	!--

	K4FILE$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(05) 401(k) Transmittal File\*
	!	.p
	!	Specifies the name of a text file to write out a 401(k)
	!	transmittal as specified by the company handling the plan.
	!
	! Index:
	!	.x Sort>401k Audit Report
	!	.x 401k Audit Report>Sort
	!
	!--

	EMPCONT = VAL(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)) / 100.0

	!++
	! Abstract:FLD08
	!	^*(08) Employers Rate\*
	!	.p
	!	The ^*Employers Rate\* field is to be entered with a
	!	rate of the employees retirement deduction
	!	that the employer will contribute to the employees
	!	retirement fund.
	!
	! Index:
	!	.x Employers>Rate
	!	.x Rate>Employers
	!
	!--

	EMPLIMIT = VAL(EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)) / 100.0

	!++
	! Abstract:FLD09
	!	^*(09) Employers Limit\*
	!	.p
	!	The ^*Employers Limit\* field is to be set to the
	!	rate of the employees gross pay
	!	that the employer will contribute up to.
	!
	! Index:
	!	.x Employers>Limit
	!	.x Limit>Employers
	!	.x Contribution>Limit
	!	.x Limit>Contribution
	!
	!--

	FLEXWILD$ = TRM$(UTL_REPORTX::OPTDEF(9%))

	!++
	! Abstract:FLD10
	!	^*(05) Wildcard 401(k) code(s)\*
	!	.p
	!	This field specifies what deduction code is used
	!	for 401(k) entries.
	!
	! Index:
	!	.x Sort>401k Audit Report
	!	.x 401k Audit Report>Sort
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

295	!
	! Create a temporary file
	!
	IF K4FILE$ <> ""
	THEN
		CALL ASSG_CHANNEL(K4FILE.CH%, STAT%)
		WHEN ERROR IN
			OPEN K4FILE$ FOR OUTPUT AS FILE K4FILE.CH%, &
				RECORDSIZE 200%, &
				ALLOW READ
		PRINT #K4FILE.CH%, &
			'SSN,' + &
			'NAME1,' + &
			'NAME2,' + &
			'BIRTH,' + &
			'HIRE,' + &
			'TERM,' + &
			'HOURS,' + &
			'401K,' + &
			'GROSS,' + &
			'GENDER,' + &
			'EMPRCON,' + &
			'W2WAGE,' + &
			'FLEX'

		USE
			FILENAME$ = "TRANSACTION.FILE"
			CONTINUE HelpError
		END WHEN
	END IF

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
	TITLE$(1%) = "401(K) Savings Plan"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" To: " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = SB_TEXT$
	TITLE$(4%) = ""
	!
	! Define headers
	!
	TITLE$(5%) = "EMP #     SSN       NAME                             " + &
		"BDATE      HDATE      TDATE     REG HRS OVT HRS       " + &
		"401K  GROSS PAY"
	TITLE$(6%) = ""

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
			CONTINUE 17030 IF ERR = 5%
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
		! Summarize by Pay, Deduction and Other
		!
		SELECT PR_TRN_PAY::PTYPE
		CASE "A"
			! Ignore
		CASE "P", "O"
			LINE_TOTAL(4%) = FUNC_ROUND(LINE_TOTAL(4%) + &
				PR_TRN_PAY::GROSS, 2%)
			LINE_TOTAL(1%) = FUNC_ROUND(LINE_TOTAL(1%) + &
				PR_TRN_PAY::REG_HR, 2%)
			LINE_TOTAL(2%) = FUNC_ROUND(LINE_TOTAL(2%) + &
				PR_TRN_PAY::OVT_HR, 2%)
		CASE "D", "F"
			IF COMP_STRING(PR_TRN_PAY::CODE, K4WILD$)
			THEN
				LINE_TOTAL(3%) = FUNC_ROUND(LINE_TOTAL(3%) + &
					PR_TRN_PAY::GROSS, 2%)
			END IF

			IF COMP_STRING(PR_TRN_PAY::CODE, FLEXWILD$)
			THEN
				LINE_TOTAL(7%) = FUNC_ROUND(LINE_TOTAL(7%) + &
					PR_TRN_PAY::GROSS, 2%)
			END IF

		END SELECT

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

		SELECT PR_TRN_DED::DTYPE
		CASE "C"
			IF PR_TRN_DED::CODE = "FW"
			THEN
				LINE_TOTAL(6%) = FUNC_ROUND(LINE_TOTAL(6%) + &
					PR_TRN_DED::TAXABLE, 2%)
			END IF

		CASE "D", "F"
			!
			! Pick off 401k
			!
			IF COMP_STRING(PR_TRN_DED::CODE, K4WILD$)
			THEN
				LINE_TOTAL(3%) = FUNC_ROUND(LINE_TOTAL(3%) + &
					PR_TRN_DED::AMOUNT, 2%)
			END IF

			IF COMP_STRING(PR_TRN_DED::CODE, FLEXWILD$)
			THEN
				LINE_TOTAL(7%) = FUNC_ROUND(LINE_TOTAL(7%) + &
					PR_TRN_DED::AMOUNT, 2%)
			END IF

		END SELECT

		GOTO 17340

17350		!
		! Get the check number now
		!
		PR_TRN_CHECK::PR_END_DATE	= ""
		PR_TRN_CHECK::CHECK		= ""
		PR_TRN_CHECK::CHECK_DATE	= ""
		PR_TRN_CHECK::PAYFREQ		= 0%

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
		! Calculate employer contributation
		!
		EMP_CONT = FUNC_ROUND(LINE_TOTAL(3%) * EMPCONT, 2%)
		EMP_MAX = FUNC_ROUND(LINE_TOTAL(4%) * EMPLIMIT, 2%)

		IF ABS(EMP_MAX) < ABS(EMP_CONT)
		THEN
			EMP_CONT = EMP_MAX
		END IF

		LINE_TOTAL(5%) = EMP_CONT

		!
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
		PR_HIS_CHECK.CH% = 0%

		CLOSE PR_TRN_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		PR_TRN_PAY.CH% = 0%
		PR_HIS_PAY.CH% = 0%

		CLOSE PR_TRN_DED.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_DED.CH%)
		PR_TRN_DED.CH% = 0%
		PR_HIS_DED.CH% = 0%

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

		IF PR_EMP_MASTER::EMPNUM <> PR_TEMP::EMPLOYEE
		THEN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_TEMP::EMPLOYEE, &
				REGARDLESS
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

		TEXT$ = FORMAT$(TEMP$, "'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL") + &
			SPACE$(42%) + &
			PRNT_DATE(DATA_FILE$(PR_LOOP%), 6%) + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 1%), "#####.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 2%), "#####.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 3%), "########.##") + &
			FORMAT$(GRAND_TOTAL(PR_LOOP%, 4%), "########.##")

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

	TEXT$ = LEFT(TEMP$ + SPACE$(43%), 42%) + &
		SPACE$(42%) + &
		FORMAT$(TOTAL(1%), "#####.##") + &
		FORMAT$(TOTAL(2%), "#####.##") + &
		FORMAT$(TOTAL(3%), "########.##") + &
		FORMAT$(TOTAL(4%), "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "Total of " + NUM1$(TT_EMP_COUNT%) + " employees paid"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	CLOSE #K4FILE.CH%

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
		TEXT$ = PR_TEMP::EMPLOYEE + &
			PRNT_SSN(PR_EMP_MASTER::SSN, 0%) + &
			PR_EMP_MASTER::EMPNAME + &
			PRNT_DATE(PR_EMP_MASTER::BIRTH, 8%) + " " + &
			PRNT_DATE(PR_EMP_MASTER::HIREDAY, 8%) + " " + &
			PRNT_DATE(PR_EMP_MASTER::TERMDAY, 8%) + &
			FORMAT$(PR_TEMP::AMOUNT(1%), "#####.##") + &
			FORMAT$(PR_TEMP::AMOUNT(2%), "#####.##") + &
			FORMAT$(PR_TEMP::AMOUNT(3%), "########.##") + &
			FORMAT$(PR_TEMP::AMOUNT(4%), "########.##")

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
			SPACE$(42%) + &
			FORMAT$(EMP_TOTAL(1%), "#####.##") + &
			FORMAT$(EMP_TOTAL(2%), "#####.##") + &
			FORMAT$(EMP_TOTAL(3%), "########.##") + &
			FORMAT$(EMP_TOTAL(4%), "########.##")

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

	IF K4FILE$ <> ""
	THEN
		NAME1$ = PR_EMP_MASTER::EMPNAME
		NAME2$ = ""
		I% = INSTR(1%, NAME1$, ",")
		IF I%
		THEN
			NAME2$ = EDIT$(RIGHT(NAME1$, I% + 1%), 8%)
			NAME1$ = LEFT(NAME1$, I% - 1%)
		END IF

		SSN$ = PR_EMP_MASTER::SSN
		WHILE INSTR(1%, SSN$, "-")
			I% = INSTR(1%, SSN$, "-")
			SSN$ = LEFT(SSN$, I% - 1%) + RIGHT(SSN$, I% + 1%)
		NEXT

		PRINT #K4FILE.CH%, &
			'"' + TRM$(SSN$) + '",' + &
			'"' + TRM$(NAME1$) + '",' + &
			'"' + TRM$(NAME2$) + '",' + &
			TRM$(PR_EMP_MASTER::BIRTH) + ',' + &
			TRM$(PR_EMP_MASTER::HIREDAY) + "," + &
			TRM$(PR_EMP_MASTER::TERMDAY) + "," + &
			EDIT$(FORMAT$((EMP_TOTAL(1%) + EMP_TOTAL(2%)), &
			"#####.##"), 8%) + ',' + &
			EDIT$(FORMAT$(EMP_TOTAL(3%), "########.##"), 8%) + ',' + &
			EDIT$(FORMAT$(EMP_TOTAL(4%), "########.##"), 8%) + ',' + &
			PR_EMP_MASTER::SEX + "," + &
			EDIT$(FORMAT$(EMP_TOTAL(5%), "########.##"), 8%) + "," + &
			EDIT$(FORMAT$(EMP_TOTAL(6%), "########.##"), 8%) + "," + &
			EDIT$(FORMAT$(EMP_TOTAL(7%), "########.##"), 8%)

	END IF

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
			SPACE$(42%) + &
			SPACE$(4%) + &
			FORMAT$(SD_TOTAL(1%), "#####.##") + &
			FORMAT$(SD_TOTAL(2%), "#####.##") + &
			FORMAT$(SD_TOTAL(3%), "########.##") + &
			FORMAT$(SD_TOTAL(4%), "########.##")

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
			SPACE$(42%) + &
			FORMAT$(SL_TOTAL(1%), "#####.##") + &
			FORMAT$(SL_TOTAL(2%), "#####.##") + &
			FORMAT$(SL_TOTAL(3%), "########.##") + &
			FORMAT$(SL_TOTAL(4%), "########.##")

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
			SPACE$(42%) + &
			SPACE$(11%) + &
			FORMAT$(SS_TOTAL(1%), "#####.##") + &
			FORMAT$(SS_TOTAL(2%), "#####.##") + &
			FORMAT$(SS_TOTAL(3%), "########.##") + &
			FORMAT$(SS_TOTAL(4%), "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

	THIS_SS$ = PR_EMP_MASTER::TAX_PKG
	SS_EMP_COUNT% = 0%
	SS_TOTAL(I%) = 0.0 FOR I% = 0% TO 8%

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

 !	IF ERR = 154%
 !	THEN
 !		SLEEP 1%
 !		RESUME
 !	END IF

	FILENAME$ = ""
	RESUME HelpError

32767	END
