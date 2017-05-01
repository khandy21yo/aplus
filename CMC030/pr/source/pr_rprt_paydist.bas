1	%TITLE "Payroll Pay Distribution Report"
	%SBTTL "PR_RPRT_PAYDIST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:PR032
	!
	! Abstract:HELP
	!	.p
	!	The ^*Payroll Pay Distribution Report\* prints a list which displays how
	!	the payroll was distributed for each department, location, and the grand total.
	!	The following fields are included:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Location
	!	.le
	!	Department
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Hire Date
	!	.le
	!	Pay Type
	!	.le
	!	Current Hours
	!	.le
	!	Current Dollars
	!	.le
	!	Year to Date Hours
	!	.le
	!	Year to Date Dollars
	!	.els
	!
	! Index:
	!	.x Pay Distribution>Report
	!	.x Report>Pay Distribution
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_PAYDIST/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_PAYDIST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_PAYDIST.OBJ;*
	!
	! Author:
	!
	!	03/23/87 - Robert Peterson
	!
	! Modification history:
	!
	!	01/15/90 - Kevin Handy
	!		Fixed bug where one folder could be missed
	!		after the first pass through all folders.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be either sent to a spreadsheet or a DIF file.
	!
	!	01/15/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	01/15/91 - Craig Tanner
	!		Untangled error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY file.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Remove unsolicited_input stuff.
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
	!	05/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Increase file dim from 200 to 1000 (DWI)
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	DECLARE INTEGER CONSTANT OPEN_MAX = 24

	!
	! Dimension
	!
	DIM DATA_FILE$(1000%), &
		USE_HISTORY%(OPEN_MAX), PR_TMP_PAY.CH%(OPEN_MAX), &
		EMP_TOTAL(200%, 10%), PAY_CODE$(200%), &
		DEPT_TOTAL(200%, 10%), DEPT_CODE$(200%), &
		LOC_TOTAL(200%, 10%), LOC_CODE$(200%), &
		GRAND_TOTAL(200%, 10%), TOTAL_CODE$(200%)

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Folder Date\*
	!	.p
	!	The ^*From Folder Date\* field enters the
	!	date of the payroll folder for which the report is to start
	!	printing. A blank in this field causes the
	!	report to start with the first folder date in the file.
	!
	! Index:
	!
	!--

	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)
	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Folder Date\*
	!	.p
	!	The ^*To Folder Date\* field enters the date
	!	of the payroll folder for which the report is to end printing.
	!	A blank in this field causes the report
	!	to end with the last folder date in the file.
	!
	! Index:
	!
	!--

	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)
	YYYY$ = LEFT(TO_BATCH_NO$, 4%)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* field causes the report
	!	to print beginning with a particular item. The
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
	!	The ^*To Item\* field causess the report
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
	!	^*(05) Sort by\*
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

340	!
	! Open pay deduction file for year
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Pay Distribution"
	TITLE$(2%) = "For the Payroll Folders Dated From  " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + "  To  " + &
		PRNT_DATE(TO_BATCH_NO$, 6%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(59%) + "Pay    " + &
		"----------Current----------  --------Year to Date-------"
	TITLE$(5%) = "Loc   Dept    EmpNum     " + &
		"EmpName               HireDate    Type   " + &
                "       Hours        Dollars         Hours        Dollars"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$Location:004,$Dept:012,$EmpNum:024,$EmpName:046," + &
		"DHireDate:058,$Code:062,VCurHours:078,VCurDols:093," + &
		"VYTDHours:107,VYTDDols:122"

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

	WORK_EMPNUM$ = PR_EMP_MASTER::EMPNUM
	WORK_EMPNAME$ = PR_EMP_MASTER::EMPNAME
	WORK_HIREDAY$ = PRNT_DATE(PR_EMP_MASTER::HIREDAY, 8%)

	PAY_LOOP% = 0%
	TEST% = 0%

	EMP_TOTAL(I%, J%) = 0.0 FOR I% = 1% TO 200% FOR J% = 1% TO 10%

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
			CONTINUE 17150 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

17110		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			IF ERR = 11%
			THEN
				PR_TRN_PAY::EMPNUM = ""
				CONTINUE 17150
			END IF
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

		GOTO 17150 IF PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM

17120		GOTO 17130 IF PAY_CODE$(LOOP%) = PR_TRN_PAY::CODE &
			FOR LOOP% = 1% TO PAY_LOOP%

		LOOP%, PAY_LOOP% = PAY_LOOP% + 1%
		PAY_CODE$(LOOP%) = PR_TRN_PAY::CODE
		TEST% = -1%

17130		EMP_TOTAL(LOOP%, 1%) = FUNC_ROUND(EMP_TOTAL(LOOP%, 1%) + &
			PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR, 2%)

		EMP_TOTAL(LOOP%, 2%) = FUNC_ROUND(EMP_TOTAL(LOOP%, 2%) + &
				PR_TRN_PAY::GROSS, 2%)

		IF (PR_TRN_PAY::UPDATE_FLAG AND 1%) = 0%
		THEN
			EMP_TOTAL(LOOP%, 3%) = &
				FUNC_ROUND(EMP_TOTAL(LOOP%, 1%) + &
				PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR, 2%)

			EMP_TOTAL(LOOP%, 4%) = &
				FUNC_ROUND(EMP_TOTAL(LOOP%, 2%) + &
				PR_TRN_PAY::GROSS, 2%)
		END IF

		GOTO 17110

17150		IF PR_LOOP% > OPEN_MAX
		THEN
			CLOSE PR_TMP_PAY.CH%
			CALL ASSG_FREECHANNEL(PR_TMP_PAY.CH%)
		END IF

	NEXT PR_LOOP%

17200	!
	! Get pay deduction information
	!
	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

17210	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 17350 IF PR_EMP_MASTER::EMPNUM <> PR_REG_ERNDED::EMPNUM

	GOTO 17210 IF PR_REG_ERNDED::ETYPE <> "P"

17220	GOTO 17230 IF PAY_CODE$(LOOP%) = PR_REG_ERNDED::CODE &
		FOR LOOP% = 1% TO PAY_LOOP%

	LOOP%, PAY_LOOP% = PAY_LOOP% + 1%
	PAY_CODE$(LOOP%) = PR_REG_ERNDED::CODE
	TEST% = -1%

17230	EMP_TOTAL(LOOP%, 3%) = FUNC_ROUND(EMP_TOTAL(LOOP%, 3%) + &
		PR_REG_ERNDED::REG_HRS(I%) + PR_REG_ERNDED::PRE_HRS(I%), 2%) &
		FOR I% = 0% TO 3%

	EMP_TOTAL(LOOP%, 4%) = FUNC_ROUND(EMP_TOTAL(LOOP%, 4%) + &
		PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
		FOR I% = 0% TO 3%

	GOTO 17210

17350	!
	! Print employee total
	!
	IF TEST%
	THEN
		IF PASS_1% = 0%
		THEN
			WORK_LOCATION$ = PR_EMP_MASTER::LOCATION
			WORK_DEPT$ = PR_EMP_MASTER::DEPT
		END IF

		IF TEST_DEPT$ <> PR_EMP_MASTER::DEPT AND PASS_1%
		THEN
			GOSUB DeptTotal
			GOTO ExitTotal IF UTL_REPORTX::STAT

			WORK_DEPT$ = PR_EMP_MASTER::DEPT
		END IF

		IF TEST_LOCATION$ <> PR_EMP_MASTER::LOCATION AND PASS_1%
		THEN
			GOSUB LocTotal
			GOTO ExitTotal IF UTL_REPORTX::STAT

			WORK_LOCATION$ = PR_EMP_MASTER::LOCATION
		END IF

		TEST_LOCATION$ = PR_EMP_MASTER::LOCATION
		TEST_DEPT$ = PR_EMP_MASTER::DEPT
		PASS_1% = -1%

		GOSUB PrintEmployee
		GOTO ExitTotal IF UTL_REPORTX::STAT
	END IF

	!
	! Go to next employee
	!
	GOTO 17020


 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB DeptTotal

	GOSUB LocTotal

	TOTAL(I%) = 0.0 FOR I% = 1% TO 4%

	TEMP$ = "    Grand Total "

	FOR LOOP% = 1% TO TOTAL_LOOP%

		TEXT$ = SPACE$(44%) + &
			LEFT(TEMP$ + SPACE$(16%), 16%) + &
			TOTAL_CODE$(LOOP%) + "   " + &
			FORMAT$(GRAND_TOTAL(LOOP%, 1%), "##,###,###.##  ") + &
			FORMAT$(GRAND_TOTAL(LOOP%, 2%), "##,###,###.## ") + &
			FORMAT$(GRAND_TOTAL(LOOP%, 3%), "##,###,###.## ") + &
			FORMAT$(GRAND_TOTAL(LOOP%, 4%), "###,###,###.##")

		TEMP$ = ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOTAL(I%) = FUNC_ROUND(TOTAL(I%) + GRAND_TOTAL(LOOP%, I%), 2%) &
				FOR I% = 1% TO 4%

	NEXT LOOP%

	IF TOTAL_LOOP% > 1%
	THEN
		TEXT$ = SPACE$(60%) + "Total" + &
			FORMAT$(TOTAL(1%), "##,###,###.##  ") + &
			FORMAT$(TOTAL(2%), "##,###,###.## ") + &
			FORMAT$(TOTAL(3%), "##,###,###.## ") + &
			FORMAT$(TOTAL(4%), "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT
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

 PrintEmployee:
	!******************************************************************
	! Print one line of employee history
	!******************************************************************

	TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 4%

	FOR LOOP% = 1% TO PAY_LOOP%

		TEXT$ = LEFT(WORK_LOCATION$ + SPACE$(4), 4%) + "  " + &
			LEFT(WORK_DEPT$ + SPACE$(6%), 6%) + "  " + &
			LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + "  " + &
			LEFT(WORK_EMPNAME$ + SPACE$(20%), 20%) + "  " + &
			LEFT(WORK_HIREDAY$ + SPACE$(10%), 10%) + "  " + &
			PAY_CODE$(LOOP%) + "   " + &
			FORMAT$(EMP_TOTAL(LOOP%, 1%), "##,###,###.##  ") + &
			FORMAT$(EMP_TOTAL(LOOP%, 2%), "##,###,###.## ") + &
			FORMAT$(EMP_TOTAL(LOOP%, 3%), "##,###,###.## ") + &
			FORMAT$(EMP_TOTAL(LOOP%, 4%), "###,###,###.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO PrintEmployee2 IF UTL_REPORTX::STAT

		WORK_LOCATION$, WORK_DEPT$ = ""
		WORK_EMPNUM$, WORK_EMPNAME$, WORK_HIREDAY$ = ""

		TOTAL(I%) = FUNC_ROUND(TOTAL(I%) + EMP_TOTAL(LOOP%, I%), 2%) &
			FOR I% = 1% TO 4%

		GOTO PrintEmployee1 IF PAY_CODE$(LOOP%) = DEPT_CODE$(I%) &
			FOR I% = 1% TO DEPT_LOOP%

		DEPT_LOOP%, I% = DEPT_LOOP% + 1%
		DEPT_CODE$(I%) = PAY_CODE$(LOOP%)

 PrintEmployee1:
		DEPT_TOTAL(I%, J%) = FUNC_ROUND(DEPT_TOTAL(I%, J%) + &
			EMP_TOTAL(LOOP%, J%), 2%) FOR J% = 1% TO 4%

	NEXT LOOP%

	IF PAY_LOOP% > 1%
	THEN
		TEXT$ = SPACE$(60%) + "Total" + &
			FORMAT$(TOTAL(1%), "##,###,###.##  ") + &
			FORMAT$(TOTAL(2%), "##,###,###.## ") + &
			FORMAT$(TOTAL(3%), "##,###,###.## ") + &
			FORMAT$(TOTAL(4%), "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO PrintEmployee2 IF UTL_REPORTX::STAT
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

 PrintEmployee2:
	RETURN

	%Page

 DeptTotal:
	!******************************************************************
	! Print Total for Dept
	!******************************************************************
	TOTAL(I%) = 0.0 FOR I% = 1% TO 4%

	TEMP$ = "Dept Total      "

	FOR LOOP% = 1% TO DEPT_LOOP%

		TEXT$ = SPACE$(44%) + &
			LEFT(TEMP$ + SPACE$(16%), 16%) + &
			DEPT_CODE$(LOOP%) + "   " + &
			FORMAT$(DEPT_TOTAL(LOOP%, 1%), "##,###,###.##  ") + &
			FORMAT$(DEPT_TOTAL(LOOP%, 2%), "##,###,###.## ") + &
			FORMAT$(DEPT_TOTAL(LOOP%, 3%), "##,###,###.## ") + &
			FORMAT$(DEPT_TOTAL(LOOP%, 4%), "###,###,###.##")

		TEMP$ = ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO DeptTotal2 IF UTL_REPORTX::STAT

		TOTAL(I%) = FUNC_ROUND(TOTAL(I%) + DEPT_TOTAL(LOOP%, I%), 2%) &
				FOR I% = 1% TO 4%

		GOTO DeptTotal1 IF DEPT_CODE$(LOOP%) = LOC_CODE$(I%) &
			FOR I% = 1% TO LOC_LOOP%

		LOC_LOOP%, I% = LOC_LOOP% + 1%
		LOC_CODE$(I%) = DEPT_CODE$(LOOP%)

 DeptTotal1:
		LOC_TOTAL(I%, J%) = FUNC_ROUND(LOC_TOTAL(I%, J%) + &
			DEPT_TOTAL(LOOP%, J%), 2%) FOR J% = 1% TO 4%

	NEXT LOOP%

	IF DEPT_LOOP% > 1%
	THEN
		TEXT$ = SPACE$(60%) + "Total" + &
			FORMAT$(TOTAL(1%), "##,###,###.##  ") + &
			FORMAT$(TOTAL(2%), "##,###,###.## ") + &
			FORMAT$(TOTAL(3%), "##,###,###.## ") + &
			FORMAT$(TOTAL(4%), "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO DeptTotal2 IF UTL_REPORTX::STAT
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	DEPT_TOTAL(I%, J%) = 0.0 FOR I% = 1% TO 200% FOR J% = 1% TO 10%

	DEPT_LOOP% = 0%

 DeptTotal2:
	RETURN

	%Page

 LocTotal:
	!******************************************************************
	! Print Total for Location
	!******************************************************************
	TOTAL(I%) = 0.0 FOR I% = 1% TO 4%

	TEMP$ = "  Loc Total     "

	FOR LOOP% = 1% TO LOC_LOOP%

		TEXT$ = SPACE$(44%) + &
			LEFT(TEMP$ + SPACE$(16%), 16%) + &
			LOC_CODE$(LOOP%) + "   " + &
			FORMAT$(LOC_TOTAL(LOOP%, 1%), "##,###,###.##  ") + &
			FORMAT$(LOC_TOTAL(LOOP%, 2%), "##,###,###.## ") + &
			FORMAT$(LOC_TOTAL(LOOP%, 3%), "##,###,###.## ") + &
			FORMAT$(LOC_TOTAL(LOOP%, 4%), "###,###,###.##")

		TEMP$ = ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO LocTotal2 IF UTL_REPORTX::STAT

		TOTAL(I%) = FUNC_ROUND(TOTAL(I%) + LOC_TOTAL(LOOP%, I%), 2%) &
			FOR I% = 1% TO 4%

		GOTO LocTotal1 IF LOC_CODE$(LOOP%) = TOTAL_CODE$(I%) &
			FOR I% = 1% TO TOTAL_LOOP%

		TOTAL_LOOP%, I% = TOTAL_LOOP% + 1%
		TOTAL_CODE$(I%) = LOC_CODE$(LOOP%)

 LocTotal1:
		GRAND_TOTAL(I%, J%) = FUNC_ROUND(GRAND_TOTAL(I%, J%) + &
			LOC_TOTAL(LOOP%, J%), 2%) FOR J% = 1% TO 4%

	NEXT LOOP%

	IF LOC_LOOP% > 1%
	THEN
		TEXT$ = SPACE$(60%) + "Total" + &
			FORMAT$(TOTAL(1%), "##,###,###.##  ") + &
			FORMAT$(TOTAL(2%), "##,###,###.## ") + &
			FORMAT$(TOTAL(3%), "##,###,###.## ") + &
			FORMAT$(TOTAL(4%), "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO LocTotal2 IF UTL_REPORTX::STAT
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	LOC_TOTAL(I%, J%) = 0.0 FOR I% = 1% TO 200% FOR J% = 1% TO 10%

	LOC_LOOP% = 0%

 LocTotal2:
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

