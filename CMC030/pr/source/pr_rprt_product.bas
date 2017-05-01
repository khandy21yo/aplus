1	%TITLE "Payroll Productivity Report"
	%SBTTL "PR_RPRT_PRODUCT"
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
	! any other copies therof may not be provided or otherwise made
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
	! ID:PR039
	!
	! Abstract:HELP
	!	.p
	!	The ^*Productivity Report\* option prints a report displaying the Year to
	!	Date and Month to Date productivity of each employee.  The following fields
	!	are included in this report:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Name
	!	.le
	!	Year to Date Productive
	!	.le
	!	Year to Date Nonproductive
	!	.le
	!	Year to Date Pay
	!	.le
	!	Year to Date Percentage of Productive
	!	.le
	!	Month to Date Productive
	!	.le
	!	Month to Date Nonproductive
	!	.le
	!	Month to Date Pay
	!	.le
	!	Month to Date Percentage of Productive
	!	.els
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_PRODUCT/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_PRODUCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	01/15/90 - Kevin Handy
	!		Modified to fix bug where one folder could
	!		be ignored after first pass through all folders.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	05/16/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	05/16/91 - Kevin Handy
	!		Modified to also use Year in comparison of current
	!		period number being examined to TO_BATCH_NO$.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/26/2000 - Kevin Handy
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

	DECLARE INTEGER CONSTANT OPEN_MAX = 24
	DECLARE INTEGER CONSTANT FILE_MAX = 2000

	!
	! Dimension
	!
	DIM DATA_FILE$(FILE_MAX), &
		USE_HISTORY%(OPEN_MAX), PR_TMP_PAY.CH%(OPEN_MAX)

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
	!	^*(01) From Folder Date\*
	!	.p
	!	The ^*From Folder Date\* field enters the
	!	date of the payroll folder for which the report is to start
	!	printing.
	!	A blank in this field causes the
	!	report to start with the first folder date in the file.
	!	.note
	!	This date is the beginning of the year, not the beginning of the month.
	!	.end note
	!
	! Index:
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	^*(02) To Folder Date\*
	!	.p
	!	The ^*To Folder Date\* field enters the date
	!	of the payroll folder for which the report is to end printing.
	!	A blank in this field causes the report
	!	to end with the last folder date in the file.
	!	.note
	!	The report uses the month in this field to determine which
	!	month to print this report for.
	!	.end note
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* field causes the report
	!	to begin with a particular item. The
	!	value must be in agreement with field (05).
	!	.p
	!	A blank in this field causes the report to begin with the
	!	first item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* field causes the report
	!	to end with a particular item. The value
	!	must be in agreement with field (05).
	!	.p
	!	A blank cause the report to end with the last item in
	!	the file.
	!
	! Index:
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort by (NU,NA,LO)\*
	!	.p
	!	The ^*Sort by\* field
	!	indicates how the report will be sorted.
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
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field. Only the above codes
	!	are valid.
	!
	! Index:
	!
	!--

	NONPROD_CODE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Non-Productive Subaccount\*
	!	.p
	!	The ^*Non-Productive Subaccount\* field enters the
	!	account which will receive no immediate payment from a client, but the
	!	company must cover the cost themselves.
	!
	! Index:
	!	.x Non=Productive Subaccount
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

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Open up the pay file.  If there are more than OPEN_MAX files
	! then skip to slower logic - open close files
	!
	TMP_DATA_FILE% = OPEN_MAX
	TMP_DATA_FILE% = DATA_FILE% IF DATA_FILE% <= OPEN_MAX

	FOR PR_LOOP% = 1% TO TMP_DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

		USE_HISTORY%(PR_LOOP%) = 0%

310		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 320 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH%(PR_LOOP%) = PR_TRN_PAY.CH%

		GOTO 330

320		!
		! Open pay history folder if journal not there
		!
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		USE_HISTORY%(PR_LOOP%) = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH%(PR_LOOP%) = PR_HIS_PAY.CH%

330		PR_TRN_PAY.CH%, PR_HIS_PAY.CH% = 0%
	NEXT PR_LOOP%

	%PAGE

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Productivity Report"
	TITLE$(2%) = "For the Payroll Folders Dated From:  " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + "  To:  " + &
		PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(33%) + &
		"------------------ Year to Date ----------------- " + &
		"----------------- Month to Date -----------------"
	TITLE$(5%) = "Emp #      Name                  " + &
		"   Productive NonProductive           Pay %ofProd " + &
		"   Productive NonProductive           Pay %ofProd"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:033,VYTDProductive:046," + &
		"VTYDNonProductive:060,VYTDPay:074,VYTDPercentOfProd:082," + &
		"VMTDProductive:096,VMTDNonProductive:110,VMTDPay:124," + &
		"VMTDPercentOfProd:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	IF FROM_ITEM$ = ""
	THEN
		RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
	ELSE
		FIND #PR_EMP_MASTER.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
	END IF

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

	AMT(I%) = 0.0 FOR I% = 1% TO 10%
	ACT_PROD_YTD, STD_PROD_YTD, ACT_PROD_MTD, STD_PROD_MTD = 0.0

	!
	! Look up employee in all payroll files selected
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

		USE_HISTORY% = 0%

		!
		! To optimize performance open_max files are open all
		! of the time to test for an employee number
		!
		IF PR_LOOP% <= OPEN_MAX
		THEN
			USE_HISTORY% = USE_HISTORY%(PR_LOOP%)
			PR_TMP_PAY.CH% = PR_TMP_PAY.CH%(PR_LOOP%)
			GOTO 17100
		END IF

17030		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 17040 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 17100

17040		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

17100		!
		! Get pay detail information
		!
		WHEN ERROR IN
			FIND #PR_TMP_PAY.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 17200 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

17110		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 17200 IF ERR = 11%
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

		GOTO 17200 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM)

		!
		! Is this productive of non productive time
		!
		IF COMP_STRING(TRM$(PR_TRN_PAY::OPER), NONPROD_CODE$) AND &
			TRM$(NONPROD_CODE$) <> ""
		THEN
			AMT(2%) = FUNC_ROUND(AMT(2%) + PR_TRN_PAY::REG_HR + &
				PR_TRN_PAY::OVT_HR, 2%)
			!
			! Add to mtd if current month
			!

			IF LEFT(TO_BATCH_NO$, 6%) = LEFT(BATCH_NO$, 6%)
			THEN
				AMT(6%) = FUNC_ROUND(AMT(6%) + &
					PR_TRN_PAY::REG_HR + &
					PR_TRN_PAY::OVT_HR, 2%)
			END IF
		ELSE
			AMT(1%) = FUNC_ROUND(AMT(1%) + PR_TRN_PAY::REG_HR + &
				PR_TRN_PAY::OVT_HR, 2%)
			!
			! Add to mtd if current month
			!
			IF LEFT(TO_BATCH_NO$, 6%) = LEFT(BATCH_NO$, 6%)
			THEN
				AMT(5%) = FUNC_ROUND(AMT(5%) + &
					PR_TRN_PAY::REG_HR + &
					PR_TRN_PAY::OVT_HR, 2%)
			END IF
		END IF

		AMT(3%) = FUNC_ROUND(AMT(3%) + PR_TRN_PAY::GROSS, 2%)

		!
		! Add to mtd if current month
		!
		IF LEFT(TO_BATCH_NO$, 6%) = LEFT(BATCH_NO$, 6%)
		THEN
			AMT(7%) = FUNC_ROUND(AMT(7%) + PR_TRN_PAY::GROSS, 2%)
		END IF

		!
		! Set employee test value
		!
		CALL PR_READ_SUBJOPER(PR_TRN_PAY::OPER, &
			PR_TRN_PAY::PR_END_DATE, &
			PIECE_RATE, &
			HOUR_RATE)

		!
		! Calculate actual productivity
		!
		TEMP_TIME = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
		TEMP_VAR = 0.0
		TEMP_VAR = FUNC_ROUND(PR_TRN_PAY::PIECE / TEMP_TIME, 2%) &
			IF TEMP_TIME <> 0.0
		ACT_PROD_YTD = FUNC_ROUND(ACT_PROD_YTD + &
			(TEMP_VAR * TEMP_TIME), 2%)

		!
		! Add to mtd if current month
		!
		IF LEFT(TO_BATCH_NO$, 6%) = LEFT(BATCH_NO$, 6%)
		THEN
			ACT_PROD_MTD = FUNC_ROUND(ACT_PROD_MTD + &
				(TEMP_VAR * TEMP_TIME), 2%)
		END IF

		!
		! Calculate standard productivity
		!
		TEMP_VAR = 0.0
		TEMP_VAR = FUNC_ROUND(HOUR_RATE / PIECE_RATE, 2%) &
			IF PIECE_RATE <> 0.0
		STD_PROD_YTD = FUNC_ROUND(STD_PROD_YTD + &
			(TEMP_VAR * TEMP_TIME), 2%)

		!
		! Add to mtd if current month
		!
		IF MID(TO_BATCH_NO$, 5%, 2%) = MID(BATCH_NO$, 5%, 2%)
		THEN
			STD_PROD_MTD = FUNC_ROUND(STD_PROD_MTD + &
				(TEMP_VAR * TEMP_TIME), 2%)
		END IF

		GOTO 17110

17200	!
	! Get data from next payroll folder
	!
		IF PR_LOOP% > OPEN_MAX
		THEN
			CLOSE PR_TMP_PAY.CH%
			CALL ASSG_FREECHANNEL(PR_TMP_PAY.CH%)
		END IF

	NEXT PR_LOOP%

	!
	! Calculate productivity percentage
	!
	AMT(4%), AMT(8%) = 0.0
	AMT(4%) = ACT_PROD_YTD / STD_PROD_YTD * 100.0 &
		IF STD_PROD_YTD <> 0.0
	AMT(8%) = ACT_PROD_MTD / STD_PROD_MTD * 100.0 &
		IF STD_PROD_MTD <> 0.0

	TEMP = 0.0
	TEMP = TEMP + AMT(I%) FOR I% = 1% TO 8%

	!
	! Print totals for one employee
	!
	IF TEMP <> 0.0
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM  + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 22%) + &
			FORMAT$(AMT(1%), "##,###,###.## ") + &
			FORMAT$(AMT(2%), "##,###,###.## ") + &
			FORMAT$(AMT(3%), "##,###,###.## ") + &
			FORMAT$(AMT(4%), "####.## ") + &
			FORMAT$(AMT(5%), "##,###,###.## ") + &
			FORMAT$(AMT(6%), "##,###,###.## ") + &
			FORMAT$(AMT(7%), "##,###,###.## ") + &
			FORMAT$(AMT(8%), "####.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitTotal IF UTL_REPORTX::STAT

		TOTAL(LOOP%) = TOTAL(LOOP%) + AMT(LOOP%) FOR LOOP% = 1% TO 10%
	END IF

	!
	! Go to next record
	!
	GOTO 17020


 ExitTotal:
	!
	! Handle end of report
	!
	TEXT$ = SPACE$(20%) + "Grand Total  " + &
		FORMAT$(TOTAL(1%), "##,###,###.## ") + &
		FORMAT$(TOTAL(2%), "##,###,###.## ") + &
		FORMAT$(TOTAL(3%), "##,###,###.##         ") + &
		FORMAT$(TOTAL(5%), "##,###,###.## ") + &
		FORMAT$(TOTAL(6%), "##,###,###.## ") + &
		FORMAT$(TOTAL(7%), "##,###,###.## ")

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
