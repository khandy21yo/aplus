1	%TITLE "Payroll Retirement Report"
	%SBTTL "PR_RPRT_RETIRE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:PR089
	!
	! Abstract:HELP
	!	.p
	!	The ^*Payroll Retirement\* report option
	!	prints, for a specified payroll folder date, a payroll retirement
	!	report.
	!	.p
	!	The fields include:
	!	.b
	!	.lm +15
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
	!	Check Date
	!	.le
	!	Gross
	!	.le
	!	Retirement Witholding
	!	.le
	!	Employer Contributation
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Report>Retirement
	!	.x Retirement>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_RETIRE/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_RETIRE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_RETIRE.OBJ;*
	!
	! Author:
	!
	!	04/11/90 - Kevin Handy
	!
	! Modification history:
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY file.
	!
	!	06/10/93 - Kevin Handy
	!		Modified code around 17100 to fix bug when negitive
	!		numbers come through.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	08/14/96 - Kevin Handy
	!		Lose extra '&' before 'end if'.
	!		Reformat source code.
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!
	!	05/29/98 - Kevin Handy
	!		Modified to handle new 'f' final deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/07/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED
	MAP (PR_HIS_DED)	PR_TRN_DED_CDD	PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK
	MAP (PR_HIS_CHECK)	PR_TRN_CHECK_CDD	PR_HIS_CHECK

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD01
	!	^*(01) Payroll Date\*
	!	.p
	!	The ^*Payroll Date\* field enters the
	!	particular date for which this report is to be printed.
	!	.p
	!	This field requires an entry.  The format for entry
	!	is MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Payroll Date>Report
	!	.x Report>Payroll Date
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Location\*
	!	.p
	!	The ^*From Location\* enters the location
	!	from which the report will begin printing.
	!	A blank field causes the report to start printing
	!	with the first Location in the file.
	!
	! Index:
	!	.x From Location>Report
	!	.x Report>From Location
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Location\*
	!	.p
	!	The ^*To Location\* field enters the location
	!	with which this report will end. A blank field will
	!	cause the report to end with the last location in the file.
	!
	! Index:
	!	.x To Location>Report
	!	.x Report>To Location
	!
	!--

	DCODES$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(05) Retirement Codes\*
	!	.p
	!	The ^*Retirement Codes\* field sets the
	!	code(s) that are used in the payroll folder for the
	!	retirement deductions.
	!	.p
	!	An example would be "^*RE\*".
	!
	! Index:
	!	.x Retirement Code>Report
	!	.x Report>Retirement Code
	!
	!--

	EMPCONT = VAL(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)) / 100.0

	!++
	! Abstract:FLD07
	!	^*(05) Employers Rate\*
	!	.p
	!	The ^*Employers Rate\* field entesr a
	!	rate of the employees retirement deduction
	!	that the employer will contribute to the employees
	!	retirement fund.
	!
	! Index:
	!	.x Employers>Rate
	!	.x Rate>Employers
	!
	!--

	EMPLIMIT = VAL(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)) / 100.0

	!++
	! Abstract:FLD08
	!	^*(05) Employers Limit\*
	!	.p
	!	The ^*Employers Limit\* field is set to the
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
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	%Page

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Retirement Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	TITLE$(3%) = ""
	TITLE$(4%) = "Loca Dept   Emp Number Name                           Gross " + &
		"     W/H  Emp Cont "

	TITLE$(5%) = ""

	LYT_LINE$ = "$Loc:5,$Dept:12,$Empnum:23,$Name:49," + &
		"VGross:60,VWH:69,VEmpCont:79"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, KEY #4%
		ELSE
			FIND #PR_EMP_MASTER.CH%, &
				KEY #4% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	THIS_LOCA$ = "^&#*##^$#@"

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
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	EMP_GROSS = 0.0
	EMP_RETIRE = 0.0

17040	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

17050	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 11%
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

	GOTO 17050 IF PR_TRN_PAY::PTYPE = "A"

	GOTO 17060 IF PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM

	EMP_GROSS = FUNC_ROUND(EMP_GROSS + PR_TRN_PAY::GROSS, 2%)

	GOTO 17050

17060	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17080 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

17070	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 17080 IF ERR = 11%
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

	GOTO 17080 IF PR_TRN_DED::EMPNUM <> PR_EMP_MASTER::EMPNUM

	IF (PR_TRN_DED::DTYPE = "D" OR PR_TRN_DED::DTYPE = "F") AND &
		COMP_STRING(PR_TRN_DED::CODE, DCODES$)
	THEN
		EMP_RETIRE = FUNC_ROUND(EMP_RETIRE + &
			PR_TRN_DED::AMOUNT, 2%)
	END IF

	GOTO 17070

17080	!

17100	!
	! Print total for employee
	!
	EMP_CONT = FUNC_ROUND(EMP_RETIRE * EMPCONT, 2%)
	EMP_MAX = FUNC_ROUND(EMP_GROSS * EMPLIMIT, 2%)

	IF ABS(EMP_MAX) < ABS(EMP_CONT)
	THEN
		EMP_CONT = EMP_MAX
	END IF

	IF (PR_EMP_MASTER::LOCATION <> THIS_LOCA$)
	THEN
		GOSUB LocaTotal
	END IF

	IF (EMP_RETIRE <> 0.0) OR (EMP_CONT <> 0.0)
	THEN
		TEXT$ = PR_EMP_MASTER::LOCATION + " " + &
			PR_EMP_MASTER::DEPT + " " + &
			PR_EMP_MASTER::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 25%) + " " + &
			FORMAT$(EMP_GROSS, "#######.## ") + &
			FORMAT$(EMP_RETIRE, "#####.## ") + &
			FORMAT$(EMP_CONT, "######.## ")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		LOCA_GROSS = LOCA_GROSS + EMP_GROSS
		LOCA_RETIRE = LOCA_RETIRE + EMP_RETIRE
		LOCA_CONT = LOCA_CONT + EMP_CONT

		GRAND_GROSS = GRAND_GROSS + EMP_GROSS
		GRAND_RETIRE = GRAND_RETIRE + EMP_RETIRE
		GRAND_CONT = GRAND_CONT + EMP_CONT

	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB LocaTotal

	TEXT$ = "     " + &
		"       " + &
		"       *** " + &
		"Grand Total               " + &
		FORMAT$(GRAND_GROSS, "#######.## ") + &
		FORMAT$(GRAND_RETIRE, "#####.## ") + &
		FORMAT$(GRAND_CONT, "######.## ")

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

 LocaTotal:
	!*******************************************************************
	! Print total for location
	!*******************************************************************

	IF (LOCA_GROSS <> 0.0) OR (LOCA_RETIRE <> 0.0) OR (LOCA_CONT <> 0.0)
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "     " + &
			"       " + &
			"       *** " + &
			"Total for Location " + THIS_LOCA$ + "   " + &
			FORMAT$(LOCA_GROSS, "#######.## ") + &
			FORMAT$(LOCA_RETIRE, "#####.## ") + &
			FORMAT$(LOCA_CONT, "######.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	THIS_LOCA$ = PR_EMP_MASTER::LOCATION
	LOCA_GROSS = 0.0
	LOCA_RETIRE = 0.0
	LOCA_CONT = 0.0

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

