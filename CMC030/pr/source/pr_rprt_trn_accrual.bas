1	%TITLE "Payroll Accrual Report"
	%SBTTL "PR_RPRT_TRN_ACCRUAL"
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
	! ID:PR005
	!
	! Abstract:HELP
	!	.p
	!	This report prints the activity occuring in the
	!	accrual codes in a specified payroll period.
	!	It reports the accrued and used amounts for each employee.
	!	.note
	!	This report should only be run after the folder has been calculated,
	!	since the calculation process
	!	creates the accrual information in the folder.
	!	Running it before the calculation will only show the used
	!	hours, and will not have the accrued hours.
	!	.end note
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_ACCRUAL/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_ACCRUAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_ACCRUAL.OBJ;*
	!
	! Author:
	!
	!	01/15/92 - Kevin Handy
	!
	! Modification history:
	!
	!	02/13/92 - Kevin Handy
	!		Modification to optionally show dollars.
	!
	!	02/24/92 - Kevin Handy
	!		Modified to FUNC_ROUND the totals.
	!
	!	01/18/93 - Kevin Handy
	!		Modified to add location and department to the
	!		report (in temp file).
	!
	!	01/18/93 - Kevin Handy
	!		Moved employee information necessary for report
	!		into temp file to lose extra searches.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/08/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP	(PR_EMP_ACCRUAL) PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	RECORD TEMP_CDD
		STRING	CODE = 2%
		STRING	LOCATION = 4%
		STRING	DEPT = 6%
		STRING	EMPNUM = 10%
		STRING	EMPNAME = 20%
		STRING	PRDATE = 8%
		REAL	HOURS
		REAL	DOLLARS
		REAL	USE_HOURS
		REAL	USE_DOLLARS
	END RECORD

	MAP (TEMP) TEMP_CDD TEMP

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Date\* field enters a
	!	particular date for which this report is to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Payroll Date>Net Check Report
	!	.x Net CHeck Report>Payroll Date
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Accrual Type\*
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Accrual Type\*
	!
	! Index:
	!
	!--

	SHOW_DOLL$ = LEFT(UTL_REPORTX::OPTDEF(3%), 1%)
	!++
	! Abstract:FLD04
	!	^*(04) Show Dollars\*
	!
	! Index:
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO 320

315	!
	! Open pay history folder if journal not there
	!
	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY.CH% = PR_HIS_PAY.CH%

320	!
	! Accrual definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
	USE
		FILENAME$ = "PR_EMP_ACCRUAL"
		CONTINUE HelpError
	END WHEN

	%PAGE

600	!
	! Create temporary file to sort the accrual items in proper
	! order.
	!
	CALL ASSG_CHANNEL(TEMP.CH%, STAT%)

	OPEN "PR_W2_TEMP.TMP" FOR OUTPUT AS FILE TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TEMP, &
		BUFFER 32%, &
		PRIMARY KEY (TEMP::CODE, TEMP::EMPNUM, TEMP::PRDATE) DUPLICATES, &
		TEMPORARY, &
		ACCESS MODIFY, &
		ALLOW NONE

605	RESET #PR_TRN_PAY.CH%

610	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 900 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If journal net there the set history map to journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

620	!
	! Handle "A" type records
	!
	GOTO 630 IF (PR_TRN_PAY::PTYPE <> "A")

	GOTO 610 UNLESS (PR_TRN_PAY::CODE >= FROM_ITEM$) AND &
		((TO_ITEM$ = "") OR (PR_TRN_PAY::CODE <= TO_ITEM$))

	GOSUB GetMaster

	TEMP::CODE	= PR_TRN_PAY::CODE
	TEMP::EMPNUM	= PR_TRN_PAY::EMPNUM
	TEMP::EMPNAME	= PR_EMP_MASTER::EMPNAME
	TEMP::LOCATION	= PR_EMP_MASTER::LOCATION
	TEMP::DEPT	= PR_EMP_MASTER::DEPT
	TEMP::PRDATE	= PR_TRN_PAY::PR_END_DATE
	TEMP::HOURS	= PR_TRN_PAY::REG_HR
	TEMP::DOLLARS	= PR_TRN_PAY::GROSS
	TEMP::USE_HOURS	= 0.0
	TEMP::USE_DOLLARS = 0.0

	PUT #TEMP.CH%

630	!
	! Handle "P" type records
	!
	GOTO 690 UNLESS PR_TRN_PAY::PTYPE = "P"

	WHEN ERROR IN
		FIND #PR_EMP_ACCRUAL.CH%, &
			KEY #0% EQ PR_TRN_PAY::EMPNUM + PR_TRN_PAY::CODE, &
			REGARDLESS
	USE
		CONTINUE 690
	END WHEN

640	WHEN ERROR IN
		GET #TEMP.CH%, KEY #0% EQ PR_TRN_PAY::CODE + PR_TRN_PAY::EMPNUM
	USE
		CONTINUE 650
	END WHEN

	TEMP::USE_HOURS = FUNC_ROUND(TEMP::USE_HOURS + PR_TRN_PAY::REG_HR + &
		PR_TRN_PAY::OVT_HR, 2%)
	TEMP::USE_DOLLARS = FUNC_ROUND(TEMP::DOLLARS + PR_TRN_PAY::GROSS, 2%)

	UPDATE #TEMP.CH%
	GOTO 690

650	GOSUB GetMaster

	TEMP::CODE	= PR_TRN_PAY::CODE
	TEMP::EMPNUM	= PR_TRN_PAY::EMPNUM
	TEMP::EMPNAME	= PR_EMP_MASTER::EMPNAME
	TEMP::LOCATION	= PR_EMP_MASTER::LOCATION
	TEMP::DEPT	= PR_EMP_MASTER::DEPT
	TEMP::PRDATE	= PR_TRN_PAY::PR_END_DATE
	TEMP::HOURS	= 0.0
	TEMP::DOLLARS	= 0.0
	TEMP::USE_HOURS	= PR_TRN_PAY::REG_HR
	TEMP::USE_DOLLARS	= PR_TRN_PAY::GROSS

	PUT #TEMP.CH%

690	GOTO 610

	%Page

900	!
 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Accrual Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	TITLE$(3%) = ""

	IF SHOW_DOLL$ = "Y"
	THEN
		TITLE$(4%) = "                                                         " + &
		"-----Accrued----    -----Used------"
		TITLE$(5%) = "Code  Locat  Dept Employee Name                  Date    " + &
		"  Hours   Dollars   Hours   Dollars"
	ELSE
		TITLE$(4%) = "                                                         " + &
		"Accrued     Used"
		TITLE$(5%) = "Code  Locat  Dept Employee Name                  Date    " + &
		"  Hours    Hours"
	END IF

	TITLE$(6%) = ""

	LYT_LINE$ = "$Code:3,$Empnum:14,$EmpName:31,$PDate:42,NHours:52"
	LYT_LINE$ = LYT_LINE$ + ",NDOLLWR:65" IF SHOW_DOLL$ = "Y"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	RESET #TEMP.CH%
	THIS_CODE$ = "!!!!!!"
	SUB_HOURS = 0.0
	SUB_DOLLARS = 0.0
	TOT_HOURS = 0.0
	TOT_DOLLARS = 0.0

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

17080	GOSUB CodeTotal IF THIS_CODE$ <> TEMP::CODE

	IF SHOW_DOLL$ = "Y"
	THEN
		TEXT$ = TEMP::CODE + " " + &
			TEMP::LOCATION + " " + &
			TEMP::DEPT + " " + &
			TEMP::EMPNUM + " " + &
			LEFT(TEMP::EMPNAME, 19%) + " " + &
			PRNT_DATE(TEMP::PRDATE, 8%) + " " + &
			FORMAT$(TEMP::HOURS, "####.##") + " " + &
			FORMAT$(TEMP::DOLLARS, "######.##") + " " + &
			FORMAT$(TEMP::USE_HOURS, "####.##") + " " + &
			FORMAT$(TEMP::USE_DOLLARS, "######.##")
	ELSE
		TEXT$ = TEMP::CODE + " " + &
			TEMP::LOCATION + " " + &
			TEMP::DEPT + " " + &
			TEMP::EMPNUM + " " + &
			LEFT(TEMP::EMPNAME, 19%) + " " + &
			PRNT_DATE(TEMP::PRDATE, 8%) + " " + &
			FORMAT$(TEMP::HOURS, "####.##") + " " + &
			FORMAT$(TEMP::USE_HOURS, "######.##")
	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	SUB_HOURS = FUNC_ROUND(SUB_HOURS + TEMP::HOURS, 2%)
	SUB_DOLLARS = FUNC_ROUND(SUB_DOLLARS + TEMP::DOLLARS, 2%)
	SUB_USE_HOURS = FUNC_ROUND(SUB_USE_HOURS + TEMP::USE_HOURS, 2%)
	SUB_USE_DOLLARS = FUNC_ROUND(SUB_USE_DOLLARS + TEMP::USE_DOLLARS, 2%)
	TOT_HOURS = FUNC_ROUND(TOT_HOURS + TEMP::HOURS, 2%)
	TOT_DOLLARS = FUNC_ROUND(TOT_DOLLARS + TEMP::DOLLARS, 2%)
	TOT_USE_HOURS = FUNC_ROUND(TOT_USE_HOURS + TEMP::USE_HOURS, 2%)
	TOT_USE_DOLLARS = FUNC_ROUND(TOT_USE_DOLLARS + TEMP::USE_DOLLARS, 2%)

	!
	! Try for next record
	!
	GOTO 17020

 CodeTotal:

	IF THIS_CODE$ <> "!!!!!!"
	THEN
		IF SHOW_DOLL$ = "Y"
		THEN
			TEXT$ = THIS_CODE$ + " " + &
				"     " + &
				"       " + &
				"           " + &
				"SubTotal             " + &
				"          " + &
				FORMAT$(SUB_HOURS, "####.##") + " " + &
				FORMAT$(SUB_DOLLARS, "######.##") + " " + &
				FORMAT$(SUB_USE_HOURS, "####.##") + " " + &
				FORMAT$(SUB_USE_DOLLARS, "######.##")
		ELSE
			TEXT$ = THIS_CODE$ + " " + &
				"     " + &
				"       " + &
				"           " + &
				"SubTotal             " + &
				"        " + &
				FORMAT$(SUB_HOURS, "######.##") + " " + &
				FORMAT$(SUB_USE_HOURS, "######.##")
		END IF

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	THIS_CODE$ = TEMP::CODE
	SUB_HOURS = 0.0
	SUB_DOLLARS = 0.0

	RETURN

	%PAGE

 GetMaster:
18400	!*******************************************************************
	! Search the employee master file for an employee name
	!*******************************************************************

	IF PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM
	THEN
		WHEN ERROR IN
			PR_EMP_MASTER::EMPNUM = PR_TRN_PAY::EMPNUM
			PR_EMP_MASTER::EMPNAME = ""
			PR_EMP_MASTER::LOCATION = ""
			PR_EMP_MASTER::DEPT = ""

			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 18490
		END WHEN
	END IF

18490	RETURN

	%PAGE

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB CodeTotal

	IF SHOW_DOLL$ = "Y"
	THEN
		TEXT$ = "   " + &
			"     " + &
			"       " + &
			"           " + &
			"Grand Total          " + &
			"          " + &
			FORMAT$(TOT_HOURS, "####.##") + " " + &
			FORMAT$(TOT_DOLLARS, "######.##") + " " + &
			FORMAT$(TOT_USE_HOURS, "####.##") + " " + &
			FORMAT$(TOT_USE_DOLLARS, "######.##")
	ELSE
		TEXT$ = "   " + &
			"     " + &
			"       " + &
			"           " + &
			"Grand Total          " + &
			"        " + &
			FORMAT$(TOT_HOURS, "######.##") + " " + &
			FORMAT$(TOT_USE_HOURS, "######.##")
	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)


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
