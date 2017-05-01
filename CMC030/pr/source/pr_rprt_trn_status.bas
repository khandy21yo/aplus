1	%TITLE "Payroll Tax Register Report"
	%SBTTL "PR_RPRT_TRN_STATUS"
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
	! ID:PR007
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee Payroll Taxes\* report option produces a report which
	!	lists the following information for a specified payroll folder:
	!	.b
	!	^*Note:\* The Employee Payroll Taxes report should be run after the calculate.
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Total Pay (Earnings)
	!	.te
	!	Total Non-Compensation Pay
	!	.te
	!	FICA Taxable Earnings
	!	.te
	!	FICA Tax Withheld
	!	.te
	!	Federal Taxable Earnings
	!	.te
	!	Federal Taxes Withheld
	!	.te
	!	State(s) Taxable Earnings
	!	.te
	!	Applicable State(s)
	!	.te
	!	Applicable State(s) Taxes Withheld
	!	.te
	!	Other State(s) Taxable Earnings
	!	.te
	!	Applicable State(s)
	!	.te
	!	Other State(s) Taxes
	!	.te
	!	City(ies) Taxable Earnings
	!	.te
	!	Applicable City(ies)
	!	.te
	!	Applicable City(ies) Taxes Withheld
	!	.te
	!	County(ies) Taxable Earnings
	!	.te
	!	Applicable County(ies)
	!	.te
	!	Applicable County(ies) Taxes Withheld
	!	.te
	!	School District Taxable Earnings
	!	.te
	!	Applicable School District
	!	.te
	!	Applicable School District Taxes Withheld
	!	.te
	!	Total Pay (Earnings)
	!	.te
	!	Total Non-Compensation Pay
	!	.te
	!	Total Taxable Earnings in each category
	!	.te
	!	Employee Count
	!	.te
	!	Total Federal Deposit Information
	!	.break
	!	i.e. Employee and Employer FICA and
	!	Federal Taxes Withheld
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Payroll Taxes>Employee>Report
	!	.x Report>Payroll Taxes>Employee
	!	.x Employee>Payroll Taxes>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_STATUS/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_STATUS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_STATUS.OBJ;*
	!
	! Author:
	!
	!	09/28/92 - Kevin Handy
	!		taken from pr_rprt_trn_tax.
	!
	! Modification history:
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Lose definition for TAX_TYPE_TABLE$ and
	!		SUBJECT_TYPE_TABLE, which were never used.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/29/2000 - Kevin Handy
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
	DECLARE				UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP	(PR_TRN_DED)		PR_TRN_DED_CDD		PR_TRN_DED
	MAP	(PR_HIS_DED)		PR_TRN_DED_CDD		PR_HIS_DED

	!
	! Dimension
	!
	DIM	EMPLOYEE_CODES%(10%), &
		TOTAL_WH_CODE%(10%), &
		LOCAL_WH_CODE%(10%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Other Variables
	!
 !	SUBJECT_TYPE_TABLE$ = "FIE*FWH*SWH*OST*SUI*CWH*DWH*EWH*SWC"
 !	TAX_TYPE_TABLE$ = "FI*FW*SW*SX*SU*CW*DW*EW*SI*"

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Date\* field enters a particular
	!	payroll date which is to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Payroll Date>Tax Register
	!	.x Tax Register>Payroll Date
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)
	YYYY$ = LEFT(BATCH_NO$, 4%)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enter a particular
	!	item with which the report will begin printing.
	!	The value must be in agreement with field
	!	(04).
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Tax Register
	!	.x Tax Register>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters a particular
	!	item with which the report will end printing.
	!	This field must correspond with field (04).
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Tax Register
	!	.x Tax Register>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.ts 55
	!	^*(04) Sort	NU,NA,SO,LO\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* code field causes the report
	!	to print in a particular order.
	!	.b
	!	Valid sort codes are:
	!	.table 3,25
	!	.te
	!	^*NU\*	Number
	!	.te
	!	^*NA\*	Name
	!	.te
	!	^*SO\*	Alphabetical (last name first)
	!	.te
	!	^*LO\*	Location
	!	.end table
	!	This field requires an entry.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Tax Register
	!	.x Tax Register>Sort
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

	WHCODE$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Tax Code	FW,SW\*
	!
	! Index:
	!	.x Tax Code>Tax Register
	!	.x Tax Register>Tax Code
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
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	GOTO 320

315	!
	! Open history Pay folder if journal not found
	!
	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	PR_TRN_DED.CH% = PR_HIS_DED.CH%

320	!

	%PAGE

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Employee Payroll Folder Status Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	SELECT SORTBY$
	CASE "NU"
		TITLE$(3%) = "By Employee Number"

	CASE "NA"
		TITLE$(3%) = "By Employee Name"

	CASE "SN"
		TITLE$(3%) = "By Soc. Sec. Number"

	CASE "LO"
		TITLE$(3%) = "By Location"

	CASE ELSE
		TITLE$(3%) = "By Alpha Sort"

	END SELECT

	TITLE$(4%) = ""

	TITLE$(5%) = ""

	LYT_LINE$ = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

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

	LOCAL_FLAG% = 0%
	EMPTOTAL% = 0%
	LOCTOTAL% = 0%

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
	CASE "LO"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOSUB LocalTotal IF (PR_EMP_MASTER::LOCATION <> THIS_LOCATION$)

	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

17100	!
	! Get pay detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17450 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

17110	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 17450 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	!
	! If history then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_DED = PR_HIS_DED
	END IF

	GOTO 17450 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM)

	GOTO 17110 IF (PR_TRN_DED::DTYPE <> "C") OR &
		(PR_TRN_DED::CODE <> WHCODE$)

	TEXT$ = PR_TRN_DED::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + "   " + &
		PR_TRN_DED::SSTATUS + "   " + &
		FORMAT$(PR_TRN_DED::EXEMPT, "###   ") + &
		FORMAT$(PR_TRN_DED::ADDEXEMPT, "###   ")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	EMPTOTAL% = EMPTOTAL% + 1%
	LOCTOTAL% = LOCTOTAL% + 1%

17450	!
	! Next employee
	!
	GOTO 17020

	%Page

 ExitTotal:
	GOSUB LocalTotal IF (SORTBY$ = "LO")

	TEXT$ = "      Total Employees " + NUM1$(EMPTOTAL%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

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
	! Print Local Total
	!*******************************************************************
 LocalTotal:
18500	GOTO 18590 IF LOCTOTAL% = 0%

	TEXT$ = "Total Local Employees " + NUM1$(LOCTOTAL%)

	CALL OUTP_LINE(OUTP_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -1%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

18590	LOCTOTAL% = 0%

	TITLE$(3%) = "For Location " + PR_EMP_MASTER::LOCATION
	THIS_LOCATION$ = PR_EMP_MASTER::LOCATION

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
