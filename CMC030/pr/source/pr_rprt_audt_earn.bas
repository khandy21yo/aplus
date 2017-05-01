1	%TITLE "Payroll Earnings Audit Report"
	%SBTTL "PR_RPRT_AUDT_EARN"
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
	! ID:PR035
	!
	! Abstract:HELP
	!	.p
	!	The ^*Employee Earning Audit Report\* shows regular and
	!	other earnings for employees during the specified time period.
	!
	! Index:
	!	.x Report>Employee Earnings Audit
	!	.x Employee Earnings Audit>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_EARN/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_AUDT_EARN, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_EARN.OBJ;*
	!
	! Author:
	!
	!	05/10/90 - Frank F. Starman
	!
	! Modification history:
	!
	!	06/14/90 - Aaron Redd
	!		Added line layout information so that the report can
	!		be sent to a spreadsheet.
	!
	!	02/11/91 - Kevin Handy
	!		Modified to have larger number of files open at
	!		one time, decrease rounding problems, and fix
	!		other so that it used gross instead of reg_pay.
	!
	!	12/03/91 - Kevin Handy
	!		Modified report settings to allow use of all of
	!		the SORTBY's, instead of just NU,NA,LO.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" type in PR_PAY.
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/93 - Kevin Handy
	!		Remove code that had been commented out.
	!
	!	04/15/93 - Kevin Handy
	!		Rewrite so that it creates a temporary file
	!		instead of trying to open/close each folder
	!		for each employee, which causes memory
	!		problems.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/27/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Added line numbers 365, 380 to try to fix KINGB
	!		problem. Report incorrectly gave an employee a
	!		larger amount of pay than he could possible have
	!		on one run.
	!
	!	02/21/96 - Kevin Handy
	!		Added wildcard department. (KingBJerkey)
	!
	!	02/22/96 - Kevin Handy
	!		Fixed SORTBY$ so that it would actually come out
	!		in other than employee number order. (KingBJerkey)
	!
	!	09/10/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/15/97 - Kevin Handy
	!		Fix so that "TO_ITEM" will work in "NU' mode.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/20/99 - Kevin Handy
	!		Add 'Windcard Account' and 'Totals Only' options.
	!
	!	11/16/2000 - Kevin Handy
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

	RECORD PR_TEMP_CDD
		STRING	SORTKEY = 32%
		STRING	EMPNUM = 10%
		STRING	EMPNAME = 30%
		STRING	SSN = 12%
		STRING	LOCATION = 4%
		STRING	DEPT = 6%
		GFLOAT	TOTAL(6%)
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	!
	! External functions
	!
	DECLARE INTEGER CONSTANT OPEN_MAX = 33
	DECLARE INTEGER CONSTANT FILE_MAX = 520

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
	!	^*(01) From Date\*
	!	.p
	!	The ^*From Date\* field specifies
	!	the date with which the report is to begin printing.
	!	A blank setting will cause the report to begin with the first
	!	date in the file.
	!
	! Index:
	!	.x Employee Earnings Audit>From Date
	!	.x From Date>Employee Earnings Audit
	!	.x Date>Employee Earnings Audit
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	^*(02) To Date\*
	!	.p
	!	The ^*To Date\* field specifies the date
	!	with which the report is to end printing. A blank setting
	!	causes the report to print to the end of the file.
	!
	! Index:
	!	.x To Date>Employee Earnings Audit
	!	.x Employe Earnings Audit>To Date
	!	.x Date>Employee Earnings Audit
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* field specifies the
	!	item which the report will start printing with.
	!	The value must be in agreement with
	!	field (05).
	!	.p
	!	A blank field causes the report to begin with the
	!	first item in the file.
	!
	! Index:
	!	.x Item>Employee Earnings Audit
	!	.x From Item>Employe Earnings Audit
	!	.x Employee Earnings Audit>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* field specifies the
	!	item at which the report will end printing.
	!	The value must be in agreement with
	!	field (05).
	!	.p
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!
	! Index:
	!	.x Employee Earnings Audit>To Item
	!	.x To Item>Employee Earnings Audit
	!	.x Item>Employee Earnings Audit
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort by (NU, NA, LO)\*
	!	.p
	!	The ^*Sort by\* field specifies
	!	how the report will be sorted.
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
	!	.lm -10
	!	.p
	!	.els
	!	An entry is required in this field. The only valid codes are
	!	shown above.
	!
	! Index:
	!	.x Employee Earnings Audit>Sory By
	!	.x Sort By>Employee Earnings Audit
	!
	!--

	P_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Regular Pay Code\*
	!	.p
	!	The ^*Regular Pay Code\* specifies the code for
	!	the type of pay which is regular pay for the purpose
	!	of printing on this report.
	!	.b
	!	If you use more than one code for regular pay,
	!	enter all of the regular pay codes seperated by commas.
	!
	! Index:
	!	.x Regular Pay Code>Employee Earnings Audit
	!	.x Employee Earnings Audit>Regular Pay Code
	!	.x Pay Code>Employee Earnings Audit
	!	.x Code>Employee Earnings Audit
	!
	!--

	WILD_DEPART$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*^*(07) Wildcard Department\*
	!	.p
	!	This field specifies which departments should be
	!	included.
	!
	! Index:
	!	.x Department>Employee Earnings Audit
	!	.x Employee Earnings Audit>Department
	!
	!--

	WILD_ACCOUNT$ = TRM$(UTL_REPORTX::OPTDEF(7%))

	!++
	! Abstract:FLD08
	!	^*^*(08) Wildcard Department\*
	!	.p
	!	This field specifies which departments should be
	!	included.
	!
	! Index:
	!	.x Department>Employee Earnings Audit
	!	.x Employee Earnings Audit>Department
	!
	!--

	TOTALS_ONLY$ = LEFT$(UTL_REPORTX::OPTDEF(8%), 1%)

	!++
	! Abstract:FLD09
	!	^*^*(09) Wildcard Department\*
	!	.p
	!	This field specifies which departments should be
	!	included.
	!
	! Index:
	!	.x Department>Employee Earnings Audit
	!	.x Employee Earnings Audit>Department
	!
	!--


	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

	GOTO ExitProgram IF DATA_FILE% = 0%

290	!
	! Open up a temporary file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)
	OPEN "PR_TEMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		BUFFER 32%, &
		MAP PR_TEMP, &
		PRIMARY KEY &
			PR_TEMP::EMPNUM &
			DUPLICATES, &
		ALTERNATE KEY &
			PR_TEMP::SORTKEY &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, &
		ALLOW NONE

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
	FOR PR_LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)
		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

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

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 330

320		!
		! Open pay history folder if journal not there
		!
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			FILENAME$ = "PR_HIS_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

330		!
		! Get pay detail information
		!
		WHEN ERROR IN
			IF (SORTBY$ = "NU") AND (FROM_ITEM$ <> "")
			THEN
				FIND #PR_TMP_PAY.CH%, &
					KEY #0% GE FROM_ITEM$, &
					REGARDLESS
			ELSE
				RESET #PR_TMP_PAY.CH%
			END IF
		USE
			CONTINUE 390
		END WHEN

 ReadHist:
335		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 390
		END WHEN

		!
		! If history then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		!
		! Skip Accrual Records
		!
		GOTO ReadHist IF PR_TRN_PAY::PTYPE = "A"

		!
		! Quick check for employee number
		!
		IF (SORTBY$ = "NU") AND (TO_ITEM$ <> "")
		THEN
			GOTO 390 IF PR_TRN_PAY::EMPNUM > TO_ITEM$
		END IF

340		!
		! Get Employee master record
		!
		IF PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM
		THEN
			WHEN ERROR IN
				GET #PR_EMP_MASTER.CH%, &
					KEY #0% EQ PR_TRN_PAY::EMPNUM, &
					REGARDLESS
			USE
				PR_EMP_MASTER::EMPNUM = PR_TRN_PAY::EMPNUM
				PR_EMP_MASTER::EMPNAME = ""
				PR_EMP_MASTER::SSN = ""
				PR_EMP_MASTER::LOCATION = ""
				PR_EMP_MASTER::SORT = ""
				PR_EMP_MASTER::DEPT = ""
				PR_EMP_MASTER::ACCT = ""

				CONTINUE 350 IF ERR = 155%
				FILENAME$ = "PR_EMP_MASTER"
				CONTINUE HelpError
			END WHEN
		END IF

350		!
		! Check current record
		!
		SELECT SORTBY$
		CASE "NA"
			GOTO ReadHist &
				IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			GOTO ReadHist IF (PR_EMP_MASTER::EMPNAME < FROM_ITEM$)

		CASE "SN"
			GOTO ReadHist IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			GOTO ReadHist IF (PR_EMP_MASTER::SSN < FROM_ITEM$)

		CASE "LO"
			GOTO ReadHist &
				IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			GOTO ReadHist IF (PR_EMP_MASTER::LOCATION < FROM_ITEM$)

		CASE "AL"
			GOTO ReadHist IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
				TO_ITEM$ <> ""
			GOTO ReadHist IF (PR_EMP_MASTER::SORT < FROM_ITEM$)
		END SELECT

		IF WILD_DEPART$ <> ""
		THEN
			GOTO ReadHist &
				IF COMP_STRING(PR_EMP_MASTER::DEPT, &
				WILD_DEPART$) = 0%
		END IF

		IF WILD_ACCOUNT$ <> ""
		THEN
			GOTO ReadHist &
				IF COMP_STRING(PR_EMP_MASTER::ACCT, &
				WILD_ACCOUNT$) = 0%
		END IF

360		!
		! Search for record already in temp file
		!
		WHEN ERROR IN
			GET #PR_TEMP.CH%, KEY #0% EQ PR_TRN_PAY::EMPNUM
		USE
			CONTINUE 370
		END WHEN

		PR_TEMP::TOTAL(1%) = PR_TEMP::TOTAL(1%) + PR_TRN_PAY::GROSS

		IF COMP_STRING(PR_TRN_PAY::CODE, P_TYPE$)
		THEN
			REG_PAY = FUNC_ROUND(PR_TRN_PAY::REG_HR * &
				PR_TRN_PAY::HOUR_RATE, 2%)

			OVT_PAY = FUNC_ROUND(PR_TRN_PAY::GROSS - REG_PAY, 2%)
			PR_TEMP::TOTAL(2%) = PR_TEMP::TOTAL(2%) + &
				PR_TRN_PAY::REG_HR
			PR_TEMP::TOTAL(3%) = PR_TEMP::TOTAL(3%) + REG_PAY
			PR_TEMP::TOTAL(4%) = PR_TEMP::TOTAL(4%) + &
				PR_TRN_PAY::OVT_HR
			PR_TEMP::TOTAL(5%) = PR_TEMP::TOTAL(5%) + OVT_PAY
		ELSE
			PR_TEMP::TOTAL(6%) = PR_TEMP::TOTAL(6%) + &
				PR_TRN_PAY::GROSS
		END IF

365		UPDATE #PR_TEMP.CH%

		GOTO ReadHist

370		!
		! Add new record to temp file
		!
		SELECT SORTBY$
		CASE "NU"
			PR_TEMP::SORTKEY = PR_EMP_MASTER::EMPNUM

		CASE "NA"
			PR_TEMP::SORTKEY = PR_EMP_MASTER::EMPNAME

		CASE "SN"
			PR_TEMP::SORTKEY = PR_EMP_MASTER::SSN

		CASE "LO"
			PR_TEMP::SORTKEY = PR_EMP_MASTER::LOCATION + &
				PR_EMP_MASTER::DEPT + &
				PR_EMP_MASTER::EMPNUM

		CASE ELSE
			PR_TEMP::SORTKEY = PR_EMP_MASTER::SORT
		END SELECT

		PR_TEMP::EMPNUM		= PR_TRN_PAY::EMPNUM
		PR_TEMP::EMPNAME	= PR_EMP_MASTER::EMPNAME
		PR_TEMP::SSN		= PR_EMP_MASTER::SSN
		PR_TEMP::LOCATION	= PR_EMP_MASTER::LOCATION
		PR_TEMP::DEPT		= PR_EMP_MASTER::DEPT

		PR_TEMP::TOTAL(1%) = PR_TRN_PAY::GROSS

		IF COMP_STRING(PR_TRN_PAY::CODE, P_TYPE$)
		THEN
			REG_PAY = FUNC_ROUND(PR_TRN_PAY::REG_HR * &
				PR_TRN_PAY::HOUR_RATE, 2%)

			OVT_PAY = FUNC_ROUND(PR_TRN_PAY::GROSS - REG_PAY, 2%)
			PR_TEMP::TOTAL(2%) = PR_TRN_PAY::REG_HR
			PR_TEMP::TOTAL(3%) = REG_PAY
			PR_TEMP::TOTAL(4%) = PR_TRN_PAY::OVT_HR
			PR_TEMP::TOTAL(5%) = OVT_PAY
			PR_TEMP::TOTAL(6%) = 0.0
		ELSE
			PR_TEMP::TOTAL(2%) = 0.0
			PR_TEMP::TOTAL(3%) = 0.0
			PR_TEMP::TOTAL(4%) = 0.0
			PR_TEMP::TOTAL(5%) = 0.0
			PR_TEMP::TOTAL(6%) = PR_TRN_PAY::GROSS
		END IF

380		PUT #PR_TEMP.CH%

		GOTO ReadHist

390		!
		! Close pay file so we can re-use channel
		!
		CLOSE #PR_TMP_PAY.CH%

	NEXT PR_LOOP%

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Earnings Audit Report"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" To: " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Define headers
	!
	TITLE$(4%) = "Emp#       Name                           " + &
		"SSN          Loc  Depart       Gross       Hrs    " + &
		"Regular       Hrs   Overtime   OtherPay"
	TITLE$(5%) = "."

	!
	! Define line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:041,$SSN:054,$Location059," + &
		"$Dept:066,VGross:078,VRegHours:088,VRegPay:099," + &
		"VOvtHrs:109,VOvtPay:120,VOtherPay:131"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #PR_TEMP.CH%, KEY #1%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "LO"
		PRINT_LINE% = 0%
		IF PR_TEMP::LOCATION + PR_TEMP::DEPT <> &
			LAST_LOCDEPT$ AND PASS%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

			TEXT$ = SPACE$(11%) + "Department Total" + &
				SPACE$(40%) + &
				FORMAT$(DEPT_TOTAL(1%), " #######.##") + &
				FORMAT$(DEPT_TOTAL(2%), " ######.##") + &
				FORMAT$(DEPT_TOTAL(3%), " #######.##") + &
				FORMAT$(DEPT_TOTAL(4%), " ######.##") + &
				FORMAT$(DEPT_TOTAL(5%), " #######.##") + &
				FORMAT$(DEPT_TOTAL(6%), " #######.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

			DEPT_TOTAL(I%) = 0% FOR I% = 1% TO 6%
			PRINT_LINE% = -1%
		END IF

		IF PR_TEMP::LOCATION <> LAST_LOC$ AND PASS%
		THEN
			TEXT$ = SPACE$(11%) + "Location Total" + &
				SPACE$(42%) + &
				FORMAT$(LOC_TOTAL(1%), " #######.##") + &
				FORMAT$(LOC_TOTAL(2%), " ######.##") + &
				FORMAT$(LOC_TOTAL(3%), " #######.##") + &
				FORMAT$(LOC_TOTAL(4%), " ######.##") + &
				FORMAT$(LOC_TOTAL(5%), " #######.##") + &
				FORMAT$(LOC_TOTAL(6%), " #######.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			LOC_TOTAL(I%) = 0% FOR I% = 1% TO 6%
			PRINT_LINE% = -1%
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) &
			IF PRINT_LINE%

		LAST_LOCDEPT$ = PR_TEMP::LOCATION + PR_TEMP::DEPT
		LAST_LOC$     = PR_TEMP::LOCATION
		PASS% = -1%

	END SELECT

	IF TOTALS_ONLY$ <> "Y"
	THEN
		!
		! Print employee total
		!
		TEXT$ = PR_TEMP::EMPNUM + " " + &
			PR_TEMP::EMPNAME + " " + &
			PR_TEMP::SSN + " " + &
			PR_TEMP::LOCATION + " " + &
			PR_TEMP::DEPT + " " + &
			FORMAT$(PR_TEMP::TOTAL(1%), " #######.##") + &
			FORMAT$(PR_TEMP::TOTAL(2%), " ######.##") + &
			FORMAT$(PR_TEMP::TOTAL(3%), " #######.##") + &
			FORMAT$(PR_TEMP::TOTAL(4%), " ######.##") + &
			FORMAT$(PR_TEMP::TOTAL(5%), " #######.##") + &
			FORMAT$(PR_TEMP::TOTAL(6%), " #######.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	IF SORTBY$ = "LO"
	THEN
		FOR I% = 1% TO 6%
			DEPT_TOTAL(I%) = FUNC_ROUND(DEPT_TOTAL(I%) + &
				PR_TEMP::TOTAL(I%), 2%)
			LOC_TOTAL(I%) = FUNC_ROUND(LOC_TOTAL(I%) + &
				PR_TEMP::TOTAL(I%), 2%)
		NEXT I%
	END IF

	GRAND_TOTAL(I%) = FUNC_ROUND(GRAND_TOTAL(I%) + PR_TEMP::TOTAL(I%), 2%) &
		FOR I% = 1% TO 6%

	!
	! Go to next record
	!
	GOTO GetNextRec

 ExitTotal:

	IF SORTBY$ = "LO"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEXT$ = SPACE$(11%) + "Department Total" + &
			SPACE$(40%) + &
			FORMAT$(DEPT_TOTAL(1%), " #######.##") + &
			FORMAT$(DEPT_TOTAL(2%), " ######.##") + &
			FORMAT$(DEPT_TOTAL(3%), " #######.##") + &
			FORMAT$(DEPT_TOTAL(4%), " ######.##") + &
			FORMAT$(DEPT_TOTAL(5%), " #######.##") + &
			FORMAT$(DEPT_TOTAL(6%), " #######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEXT$ = SPACE$(11%) + "Location Total" + &
			SPACE$(42%) + &
			FORMAT$(LOC_TOTAL(1%), " #######.##") + &
			FORMAT$(LOC_TOTAL(2%), " ######.##") + &
			FORMAT$(LOC_TOTAL(3%), " #######.##") + &
			FORMAT$(LOC_TOTAL(4%), " ######.##") + &
			FORMAT$(LOC_TOTAL(5%), " #######.##") + &
			FORMAT$(LOC_TOTAL(6%), " #######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	END IF

	!
	! Handle grand totals
	!
	TEXT$ = SPACE$(11%) + "Grand Total" + &
		SPACE$(45%) + &
		FORMAT$(GRAND_TOTAL(1%), " #######.##") + &
		FORMAT$(GRAND_TOTAL(2%), " ######.##") + &
		FORMAT$(GRAND_TOTAL(3%), " #######.##") + &
		FORMAT$(GRAND_TOTAL(4%), " ######.##") + &
		FORMAT$(GRAND_TOTAL(5%), " #######.##") + &
		FORMAT$(GRAND_TOTAL(6%), " #######.##")

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
