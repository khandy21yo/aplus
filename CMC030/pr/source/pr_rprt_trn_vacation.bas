1	%TITLE "Payroll Vacation Report"
	%SBTTL "PR_RPRT_TRN_VACATION"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	!	This report is used to display the amount of vacation that each employee
	!	has available after a payroll period.
	!	.note
	!	This report should be run after the calculation of a folder,
	!	and before the next payroll folder has been updated.
	!	.end note
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_VACATION/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_VACATION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_VACATION.OBJ;*
	!
	! Author:
	!
	!	05/18/92 - Kevin Handy
	!
	! Modification history:
	!
	!	01/14/93 - Kevin Handy
	!		Modified to handle the update flags in the
	!		folders.
	!
	!	01/14/93 - Kevin Handy
	!		Added location to fields in temp file.  Make
	!		alternate sort in temp file by department.
	!		Subtotal by department.
	!
	!	01/22/93 - Kevin Handy
	!		Added option to page after each department.
	!
	!	03/29/93 - Kevin Handy
	!		Fixed bug where could die at 620 if record didn't
	!		exist in temp file.  Modified to create one if
	!		it doesn't exist.
	!
	!	06/14/93 - Kevin Handy
	!		Added several REGARDLESS statements.
	!
	!	01/28/94 - Kevin Handy
	!		Added error trap for 624.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to version 3.6 coding standard.
	!		Removed unsolicited_input stuff.
	!
	!	05/21/96 - Kevin Handy
	!		Reformat source code.
	!		Display employee number as "Undefined" instead
	!		of displaying the previous employee name when
	!		can't find one.
	!
	!	08/14/96 - Kevin Handy
	!		Change 'PRIMARY' to 'PRIMARY KEY'.
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	03/10/97 - Kevin Handy
	!		Modifications to handle accrual type 3.
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/28/2000 - Kevin Handy
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
		STRING	EMPNUM = 10%
		STRING	LOCATION = 4%
		STRING	DEPT = 6%
		STRING	HIREDATE = 8%
		STRING	EMPNAME = 20%
		REAL	HOURS			! Hours accrued this folder
		REAL	USE_HOURS		! Hours used this folder
		REAL	THIS_ACCRUAL		!
		REAL	ACCRUAL_HOURS		! Hours available from ACCRUAL
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
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)

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

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	SELECT SORTBY$
	CASE "DP"
		SORTBY% = 1%
		SORTBY_NAME$ = "Department"
	CASE ELSE
		SORTBY% = 0%
		SORTBY_NAME$ = "Employee Name"
	END SELECT


	!++
	! Abstract:FLD04
	!	^*(04) Sort By\*
	!	.table
	!	.te
	!	NU	Employee Number
	!	.te
	!	DP	Department
	!	.end table
	!
	! Index:
	!
	!--

	PAGEYN$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Page After Department\*
	!	.p
	!	A yes in this field will cause the report to page after
	!	each department, when printing in department order.
	!	It does nothing when printing in employee order.
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
		PRIMARY KEY (TEMP::CODE, TEMP::EMPNUM, TEMP::LOCATION, TEMP::DEPT) &
			DUPLICATES, &
		ALTERNATE KEY (TEMP::CODE, TEMP::LOCATION, TEMP::DEPT, TEMP::EMPNAME) &
			DUPLICATES, &
		TEMPORARY, &
		ACCESS MODIFY, &
		ALLOW NONE

601	!
	! Loop through Accrual definition file
	!
	RESET #PR_EMP_ACCRUAL.CH%

602	WHEN ERROR IN
		GET #PR_EMP_ACCRUAL.CH%, REGARDLESS
	USE
		CONTINUE 605
	END WHEN

	GOTO 602 UNLESS (PR_EMP_ACCRUAL::ATYPE >= FROM_ITEM$) AND &
		((TO_ITEM$ = "") OR (PR_EMP_ACCRUAL::ATYPE <= TO_ITEM$))

603	TEMP::CODE	= PR_EMP_ACCRUAL::ATYPE
	TEMP::EMPNUM	= PR_EMP_ACCRUAL::EMPNUM

	IF PR_EMP_MASTER::EMPNUM <> TEMP::EMPNUM
	THEN
		PR_EMP_MASTER::LOCATION	= ""
		PR_EMP_MASTER::DEPT	= ""
		PR_EMP_MASTER::HIREDAY	= ""
		PR_EMP_MASTER::EMPNAME	= "** Undefined **"

		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ TEMP::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 604
		END WHEN

	END IF

604	TEMP::LOCATION	= PR_EMP_MASTER::LOCATION
	TEMP::DEPT	= PR_EMP_MASTER::DEPT
	TEMP::HIREDATE	= PR_EMP_MASTER::HIREDAY
	TEMP::EMPNAME	= PR_EMP_MASTER::EMPNAME
	TEMP::HOURS	= 0.0
	TEMP::USE_HOURS	= 0.0

	TEMP::THIS_ACCRUAL = 0.0
	TEMP::ACCRUAL_HOURS = PR_EMP_ACCRUAL::HOURSAVA

	PUT #TEMP.CH%

	GOTO 602

605	!
	! Loop through TRN/HIS file
	!
	RESET #PR_TRN_PAY.CH%

610	!
	! Get one pay record
	!
	WHEN ERROR IN
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

	!
	! Ignore record if out of users specified range
	!
	GOTO 610 UNLESS (PR_TRN_PAY::CODE >= FROM_ITEM$) AND &
		((TO_ITEM$ = "") OR (PR_TRN_PAY::CODE <= TO_ITEM$))

620	!
	! Accrual information?
	!
	GOTO 630 UNLESS (PR_TRN_PAY::PTYPE = "A")

	WHEN ERROR IN
		GET #TEMP.CH%, &
			KEY #0% EQ PR_TRN_PAY::CODE + PR_TRN_PAY::EMPNUM
	USE
		CONTINUE 622 IF ERR = 155%
		FILENAME$ = "TEMP"
		CONTINUE HelpError
	END WHEN

	TEMP::HOURS = TEMP::HOURS + PR_TRN_PAY::REG_HR

	IF (PR_TRN_PAY::UPDATE_FLAG AND 1%) <> 0%
	THEN
		WHEN ERROR IN
			GET #PR_EMP_ACCRUAL.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM + &
				PR_TRN_PAY::CODE, &
				REGARDLESS
		USE
			CONTINUE 622 IF ERR = 155%
			FILENAME$ = "PR_EMP_ACCRUAL"
			CONTINUE HelpError
		END WHEN

		TEMP::ACCRUAL_HOURS = TEMP::ACCRUAL_HOURS - &
			PR_TRN_PAY::REG_HR &
			IF PR_EMP_ACCRUAL::AVAILFLAG = "1"

		TEMP::ACCRUAL_HOURS = TEMP::ACCRUAL_HOURS - &
			PR_TRN_PAY::REG_HR &
			IF PR_EMP_ACCRUAL::AVAILFLAG = "3" AND &
			PR_TRN_PAY::PR_END_DATE >= PR_EMP_ACCRUAL::AVAILDATE

	END IF

	UPDATE #TEMP.CH%

	GOTO 630

622	TEMP::CODE	= PR_TRN_PAY::CODE
	TEMP::EMPNUM	= PR_TRN_PAY::EMPNUM

	IF PR_EMP_MASTER::EMPNUM <> TEMP::EMPNUM
	THEN
		GET #PR_EMP_MASTER.CH%, KEY #0% EQ TEMP::EMPNUM, REGARDLESS
	END IF

624	TEMP::LOCATION	= PR_EMP_MASTER::LOCATION
	TEMP::DEPT	= PR_EMP_MASTER::DEPT
	TEMP::HIREDATE	= PR_EMP_MASTER::HIREDAY
	TEMP::EMPNAME	= PR_EMP_MASTER::EMPNAME

	TEMP::HOURS	= PR_TRN_PAY::REG_HR
	TEMP::USE_HOURS	= 0.0
	TEMP::THIS_ACCRUAL = 0.0
	TEMP::ACCRUAL_HOURS = 0.0

	IF (PR_TRN_PAY::UPDATE_FLAG AND 1%) <> 0%
	THEN
		WHEN ERROR IN
			GET #PR_EMP_ACCRUAL.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM + &
				PR_TRN_PAY::CODE, &
				REGARDLESS
		USE
			CONTINUE 626 IF ERR = 155%
			FILENAME$ = "PR_EMP_ACCRUAL"
			CONTINUE HelpError
		END WHEN

		TEMP::ACCRUAL_HOURS = -PR_TRN_PAY::REG_HR &
			IF PR_EMP_ACCRUAL::AVAILFLAG = "1"

		TEMP::ACCRUAL_HOURS = -PR_TRN_PAY::REG_HR &
			IF PR_EMP_ACCRUAL::AVAILFLAG = "3" AND &
			PR_TRN_PAY::PR_END_DATE >= PR_EMP_ACCRUAL::AVAILDATE

	END IF

626	PUT #TEMP.CH%

630	!
	! Used Up information
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
		GET #TEMP.CH%, &
			KEY #0% EQ PR_TRN_PAY::CODE + PR_TRN_PAY::EMPNUM
	USE
		CONTINUE 650
	END WHEN

	TEMP::USE_HOURS = FUNC_ROUND(TEMP::USE_HOURS + PR_TRN_PAY::REG_HR + &
		PR_TRN_PAY::OVT_HR, 2%)

	TEMP::ACCRUAL_HOURS = TEMP::ACCRUAL_HOURS + &
		PR_TRN_PAY::REG_HR &
		IF (PR_TRN_PAY::UPDATE_FLAG AND 1%) <> 0%

	UPDATE #TEMP.CH%
	GOTO 690

650	!

690	GOTO 610

	%Page

900	!
	! Now start the actual output
	!
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

	TITLE$(4%) = "                                                        " + &
		"----Accrued----  -----Used-----  ---Remaining---"
	TITLE$(5%) = "Cd  Loc  Dept  Employee Name                    Date    " + &
		"  Hours   Days     Hours   Days     Hours   Days"

	TITLE$(6%) = ""

	LYT_LINE$ = "$Code:3,$Loc:8,$Dept:15,$Empnum:24,$EmpName:41,$PDate:52," + &
		"NHours:62"
	LYT_LINE$ = LYT_LINE$ + ",NDOLLWR:65" IF SHOW.DOLL$ = "Y"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	RESET #TEMP.CH%, KEY #SORTBY%

	THIS_CODE$ = "!!!!!!"
	THIS_DEPT$ = "!!!!!!"

	SUB_BEFORE = 0.0
	SUB_CHANGE = 0.0
	SUB_BEFORE_DAY = 0.0
	SUB_CHANGE_DAY = 0.0

	DEPT_BEFORE = 0.0
	DEPT_CHANGE = 0.0
	DEPT_BEFORE_DAY = 0.0
	DEPT_CHANGE_DAY = 0.0

	TOT_BEFORE = 0.0
	TOT_CHANGE = 0.0
	TOT_BEFORE_DAY = 0.0
	TOT_CHANGE_DAY = 0.0

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

17080	IF THIS_CODE$ <> TEMP::CODE
	THEN
		GOSUB DeptTotal IF SORTBY% = 1%
		GOSUB CodeTotal
	END IF

	IF (THIS_DEPT$ <> TEMP::LOCATION + TEMP::DEPT) AND (SORTBY% = 1%)
	THEN
		GOSUB DeptTotal
	END IF

17200	BEFORE_DAY = FIX((TEMP::ACCRUAL_HOURS + TEMP::THIS_ACCRUAL) / 4.0) / 2.0
	USE_DAY = FIX(TEMP::USE_HOURS / 4.0) / 2.0

	TEXT$ = TEMP::CODE + " " + &
		TEMP::LOCATION + " " + &
		TEMP::DEPT + " " + &
		TEMP::EMPNUM + " " + &
		LEFT(TEMP::EMPNAME, 19%) + " " + &
		PRNT_DATE(TEMP::HIREDATE, 8%) + " " + &
		FORMAT$(TEMP::ACCRUAL_HOURS + TEMP::THIS_ACCRUAL, "#####.##") + " " + &
		FORMAT$(BEFORE_DAY, "####.#") + " " + &
		FORMAT$(TEMP::USE_HOURS, "#####.##") + " " + &
		FORMAT$(USE_DAY, "####.#") + " " + &
		FORMAT$(TEMP::ACCRUAL_HOURS + TEMP::THIS_ACCRUAL - TEMP::USE_HOURS, "#####.##") + " " + &
		FORMAT$(BEFORE_DAY - USE_DAY, "####.#")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	DEPT_BEFORE = FUNC_ROUND(DEPT_BEFORE + &
		TEMP::ACCRUAL_HOURS + TEMP::THIS_ACCRUAL, 2%)
	DEPT_CHANGE = FUNC_ROUND(DEPT_CHANGE + TEMP::USE_HOURS, 2%)
	DEPT_BEFORE_DAY = DEPT_BEFORE_DAY + BEFORE_DAY
	DEPT_CHANGE_DAY = DEPT_CHANGE_DAY + CHANGE_DAY

	SUB_BEFORE = FUNC_ROUND(SUB_BEFORE + &
		TEMP::ACCRUAL_HOURS + TEMP::THIS_ACCRUAL, 2%)
	SUB_CHANGE = FUNC_ROUND(SUB_CHANGE + TEMP::USE_HOURS, 2%)
	SUB_BEFORE_DAY = SUB_BEFORE_DAY + BEFORE_DAY
	SUB_CHANGE_DAY = SUB_CHANGE_DAY + CHANGE_DAY

	TOT_BEFORE = FUNC_ROUND(TOT_BEFORE + &
		TEMP::ACCRUAL_HOURS + TEMP::THIS_ACCRUAL, 2%)
	TOT_CHANGE = FUNC_ROUND(TOT_CHANGE + TEMP::USE_HOURS, 2%)
	TOT_BEFORE_DAY = TOT_BEFORE_DAY + BEFORE_DAY
	TOT_CHANGE_DAY = TOT_CHANGE_DAY + CHANGE_DAY

	!
	! Try for next record
	!
	GOTO 17020

	%PAGE

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB DeptTotal IF SORTBY% = 1%
	GOSUB CodeTotal

	TEXT$ = "   " + &
		"     " + &
		"           " + &
		"Grand Total          " + &
		"                 " + &
		FORMAT$(TOT_BEFORE, "#####.##") + " " + &
		FORMAT$(TOT_BEFORE_DAY, "####.#") + " " + &
		FORMAT$(TOT_CHANGE, "#####.##") + " " + &
		FORMAT$(TOT_CHANGE_DAY, "####.#") + " " + &
		FORMAT$(TOT_BEFORE - TOT_CHANGE, "#####.##") + " " + &
		FORMAT$(TOT_BEFORE_DAY - TOT_CHANGE_DAY, "####.#")

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

	!*******************************************************************
	! Print Accrual Code Subtotal
	!*******************************************************************

 CodeTotal:

	IF THIS_CODE$ <> "!!!!!!"
	THEN
		TEXT$ = THIS_CODE$ + " " + &
			"     " + &
			"           " + &
			"SubTotal             " + &
			"                 " + &
			FORMAT$(SUB_BEFORE, "#####.##") + " " + &
			FORMAT$(SUB_BEFORE_DAY, "####.#") + " " + &
			FORMAT$(SUB_CHANGE, "#####.##") + " " + &
			FORMAT$(SUB_CHANGE_DAY, "####.#") + " " + &
			FORMAT$(SUB_BEFORE - SUB_CHANGE, "#####.##") + " " + &
			FORMAT$(SUB_BEFORE_DAY - SUB_CHANGE_DAY, "####.#")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	THIS_CODE$ = TEMP::CODE
	SUB_BEFORE = 0.0
	SUB_BEFORE_DAY = 0.0
	SUB_CHANGE = 0.0
	SUB_CHANGE_DAY = 0.0

	RETURN

	%PAGE

	!*******************************************************************
	! Print Department Subtotal
	!*******************************************************************

 DeptTotal:

	IF THIS_DEPT$ <> "!!!!!!"
	THEN
		TEXT$ = THIS_CODE$ + " " + &
			LEFT(THIS_DEPT$, 4%) + " " + &
			RIGHT(THIS_DEPT$, 5%) + " " + &
			"    " + &
			"Department SubTotal  " + &
			"                 " + &
			FORMAT$(DEPT_BEFORE, "#####.##") + " " + &
			FORMAT$(DEPT_BEFORE_DAY, "####.#") + " " + &
			FORMAT$(DEPT_CHANGE, "#####.##") + " " + &
			FORMAT$(DEPT_CHANGE_DAY, "####.#") + " " + &
			FORMAT$(DEPT_BEFORE - DEPT_CHANGE, "#####.##") + " " + &
			FORMAT$(DEPT_BEFORE_DAY - DEPT_CHANGE_DAY, "####.#")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		IF PAGEYN$ = "Y"
		THEN
			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 3000%)
		ELSE
			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)
		END IF
	END IF

	THIS_DEPT$ = TEMP::LOCATION + TEMP::DEPT
	DEPT_BEFORE = 0.0
	DEPT_BEFORE_DAY = 0.0
	DEPT_CHANGE = 0.0
	DEPT_CHANGE_DAY = 0.0

	RETURN

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
