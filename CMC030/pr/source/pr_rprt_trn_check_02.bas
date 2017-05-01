1	%TITLE "Payroll Net Check Report"
	%SBTTL "PR_RPRT_TRN_CHECK_02"
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
	! ID:PR019
	!
	! Abstract:HELP
	!	.p
	!	The ^*Payroll Net Check Register\* report option
	!	prints, for a specified payroll folder date, a payroll register in check
	!	number sequence.  The report prints current period information only.
	!	For current and year-to-date payroll register information, see the
	!	Payroll Register report.
	!	.p
	!	It is recommended that the Payroll Net Check Register report be
	!	filed permanently.
	!	.p
	!	The fields include:
	!	.b
	!	.lm +15
	!	.list 0,"o"
	!	.le
	!	Check Number
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Pay Date
	!	.le
	!	Hours
	!	.le
	!	Units
	!	.le
	!	Gross Pay
	!	.le
	!	Non-Compensation Pay
	!	.le
	!	Miscellaneous Deductions (includes local taxes)
	!	.le
	!	State Taxes Withheld
	!	.le
	!	Federal Withholding Taxes
	!	.le
	!	FICA Taxes Withheld
	!	.le
	!	Net Check Amount
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Report>Payroll Net Check Register
	!	.x Check Register
	!	.x Payroll Net Check Register>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_CHECK_02/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_CHECK_02, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_CHECK_02.OBJ;*
	!
	! Author:
	!
	!	05/22/89 - Kevin Handy
	!		Taken from PR_RPRT_TRN_CHECK
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	07/06/90 - Kevin Handy
	!		Modified so that it will not modify a closed folder
	!		by adding check information.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Handle FH codes
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/22/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED
	MAP (PR_HIS_DED)	PR_TRN_DED_CDD	PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK
	MAP (PR_HIS_CHECK)	PR_TRN_CHECK_CDD	PR_HIS_CHECK

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	!
	! Dimension Statements
	!
	DIM EMP_TOTAL(11%)
	DIM GRAND_TOTAL(11%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Payroll Date\*
	!	.p
	!	The ^*Payroll Date\* field enters the
	!	particular date for which this report is to be printed.
	!	.p
	!	This field requires an entry. The format for entry
	!	is MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Payroll Date
	!	.x Date>Payroll
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Check _#\*
	!	.p
	!	The ^*From Check _#\* enters the check _#
	!	from which the report will begin printing.
	!	A blank field causes the report to start printing
	!	with the first Check _# in the file.
	!
	! Index:
	!	.x From Check
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(02) To Check _#\*
	!	.p
	!	The ^*To Check _#\* field enters the check
	!	_# with which this report will end. A blank field will
	!	cause the report to end with the last check _# in the file.
	!
	! Index:
	!	.x To Check Number>Check Register
	!	.x Check Register>To Check Number
	!	.x Check Number>To
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
	IF USE_HISTORY% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
		USE
			CONTINUE 330 IF ERR = 5%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN
	ELSE
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
		USE
			CONTINUE 330 IF ERR = 5%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		PR_TRN_DED.CH% = PR_HIS_DED.CH%
	END IF

330	!
	! Open Check folder
	!
	IF USE_HISTORY% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.CRE"
		USE
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN
	ELSE
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.OPN"
		USE
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

		PR_TRN_CHECK.CH% = PR_HIS_CHECK.CH%
	END IF

	%PAGE

400	GOTO 600 IF USE_HISTORY%

	!
	! Read pay folder and deduction folder
	! Check the check folder to make certain all
	! employees are in that folder.
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Do all pay/ded records have a check record?  Testing. . .", 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		OUTP_XUNSOL, SCOPE::SMG_KBID)

	RRR_FLAG% = 0%

	WHEN ERROR IN
		RESET #PR_TRN_PAY.CH%, KEY #0%
	USE
		CONTINUE 500
	END WHEN

410	!
	! Get pay record
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 500
	END WHEN

	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			OUTP_XUNSOL, SCOPE::SMG_KBID)

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	GOTO 410 IF PR_TRN_PAY::EMPNUM + PR_TRN_PAY::PR_END_DATE = TEST$

420	!
	! Check check folder to see if there
	!
	IF EMP_COUNT_TEST$ <> PR_TRN_PAY::EMPNUM
	THEN
		EMP_COUNT% = EMP_COUNT% + 1%
	END IF

	EMP_COUNT_TEST$ = PR_TRN_PAY::EMPNUM

	TEST$ = PR_TRN_PAY::EMPNUM + PR_TRN_PAY::PR_END_DATE

	WHEN ERROR IN
		FIND #PR_TRN_CHECK.CH%, &
			KEY #0% EQ PR_TRN_PAY::EMPNUM + &
			PR_TRN_PAY::PR_END_DATE, &
			REGARDLESS
	USE
		CONTINUE 430 IF ERR = 155%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	GOTO 410

430	!
	! Add check record
	!
	PR_TRN_CHECK::EMPNUM		= PR_TRN_PAY::EMPNUM
	PR_TRN_CHECK::PR_END_DATE	= PR_TRN_PAY::PR_END_DATE
	PR_TRN_CHECK::CHECK		= ""
	PR_TRN_CHECK::CHECK_DATE	= ""
	PR_TRN_CHECK::PAYFREQ		= 0%

	PR_TRN_CHECK::UPDATE_FLAG	= 0%

	WHEN ERROR IN
		PUT #PR_TRN_CHECK.CH%
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	GOTO 410

500	!
	! Now do the deduction folder
	!
	TEST$ = ""

	WHEN ERROR IN
		RESET #PR_TRN_DED.CH%, KEY #0%
	USE
		CONTINUE ReportTitle
	END WHEN

510	!
	! Get ded record
	!
	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle
	END WHEN

	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			OUTP_XUNSOL, SCOPE::SMG_KBID)

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		UTL_REPORTX::STAT = -1
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	GOTO 510 IF PR_TRN_DED::EMPNUM + PR_TRN_DED::PR_END_DATE = TEST$

520	!
	! Check check folder to see if there
	!
	TEST$ = PR_TRN_DED::EMPNUM + PR_TRN_DED::PR_END_DATE

	WHEN ERROR IN
		FIND #PR_TRN_CHECK.CH%, &
			KEY #0% EQ PR_TRN_DED::EMPNUM + &
			PR_TRN_DED::PR_END_DATE, &
			REGARDLESS
	USE
		CONTINUE 530 IF ERR = 155%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	GOTO 510

530	!
	! Add check record
	!
	PR_TRN_CHECK::EMPNUM		= PR_TRN_DED::EMPNUM
	PR_TRN_CHECK::PR_END_DATE	= PR_TRN_DED::PR_END_DATE
	PR_TRN_CHECK::CHECK		= ""
	PR_TRN_CHECK::CHECK_DATE	= ""
	PR_TRN_CHECK::PAYFREQ		= 0%

	PR_TRN_CHECK::UPDATE_FLAG	= 0%

	WHEN ERROR IN
		PUT #PR_TRN_CHECK.CH%
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	GOTO 510

600	!

	%Page

 ReportTitle:
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Net Check Register"
	TITLE$(2%) = "For the Payroll Folder Dated:  " + &
		PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "CkNum  EmpNum     EmpName                 " + &
		"Date        Hrs  Gross Pay     FICA  Federal    State     " + &
		"City SUI/OST  Misc/Ded Net Check"
	TITLE$(5%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$CheckNum:006,$EmpNum:017,$EmpName:039,DPRDate:048," + &
		"VHours:057,VGross:067,VFICA:077,VFederal:086,VState:095," + &
		"VCity:104,VSUI/OST:113,VMiscDed:122,VNetCheck:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TRN_CHECK.CH%, KEY #1%
		ELSE
			FIND #PR_TRN_CHECK.CH%, &
				KEY #1% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	!
	! If journal net there the set history map to journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_CHECK = PR_HIS_CHECK
	END IF

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_TRN_CHECK::CHECK > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 11%

17040	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_TRN_CHECK::EMPNUM, &
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

	GOTO 17060 IF PR_TRN_CHECK::EMPNUM <> PR_TRN_PAY::EMPNUM

	IF PR_TRN_PAY::PR_END_DATE = PR_TRN_CHECK::PR_END_DATE
	THEN
		EMP_TOTAL(1%) = FUNC_ROUND(EMP_TOTAL(1%) + &
			PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR, 2%)
		EMP_TOTAL(2%) = FUNC_ROUND(EMP_TOTAL(2%) + &
			PR_TRN_PAY::GROSS, 2%)
	END IF

	GOTO 17050

17060	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ PR_TRN_CHECK::EMPNUM, &
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

	GOTO 17080 IF PR_TRN_DED::EMPNUM <> PR_TRN_CHECK::EMPNUM

	IF PR_TRN_DED::PR_END_DATE = PR_TRN_CHECK::PR_END_DATE
	THEN
		SELECT PR_TRN_DED::CODE

		CASE "FI", "FH"
			EMP_TOTAL(5%) = FUNC_ROUND(EMP_TOTAL(5%) + &
				PR_TRN_DED::AMOUNT, 2%)

		CASE "FW"
			EMP_TOTAL(6%) = FUNC_ROUND(EMP_TOTAL(6%) + &
				PR_TRN_DED::AMOUNT, 2%)

		CASE "SW"
			EMP_TOTAL(7%) = FUNC_ROUND(EMP_TOTAL(7%) + &
				PR_TRN_DED::AMOUNT, 2%)

		CASE "CW"
			EMP_TOTAL(8%) = FUNC_ROUND(EMP_TOTAL(8%) + &
				PR_TRN_DED::AMOUNT, 2%)

		CASE "SU", "SX"
			EMP_TOTAL(10%) = FUNC_ROUND(EMP_TOTAL(10%) + &
				PR_TRN_DED::AMOUNT, 2%)

		CASE ELSE
			EMP_TOTAL(11%) = FUNC_ROUND(EMP_TOTAL(11%) + &
				PR_TRN_DED::AMOUNT, 2%)

		END SELECT
	END IF

	GOTO 17070

17080	!
	! Master file look up
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TRN_CHECK::EMPNUM, &
			REGARDLESS
	USE
		PR_EMP_MASTER::EMPNAME = "??????????????????????????????????"

		CONTINUE 17100 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17100	!
	! Print total for employee
	!
	EMP_TOTAL(9%) = FUNC_ROUND(EMP_TOTAL(2%) - EMP_TOTAL(5%) - &
		EMP_TOTAL(6%) - EMP_TOTAL(7%) - &
		EMP_TOTAL(8%) - &
		EMP_TOTAL(10%) - EMP_TOTAL(11%), 2%)

	GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 11%

	TEXT$ = PR_TRN_CHECK::CHECK + " " + &
		PR_TRN_CHECK::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 21%) + " " + &
		PRNT_DATE(PR_TRN_CHECK::PR_END_DATE, 6%) + " " + &
		FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(2%), "######.## ") + &
		FORMAT$(EMP_TOTAL(5%), "######.## ") + &
		FORMAT$(EMP_TOTAL(6%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(7%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(8%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(10%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(11%), "#####.##") + &
		FORMAT$(EMP_TOTAL(9%), "#######.## ")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!

	TEXT$ = "                  Grand Total                    " + &
		FORMAT$(GRAND_TOTAL(1%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(5%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(7%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(8%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(10%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(11%), "#####.##") + &
		FORMAT$(GRAND_TOTAL(9%), "#######.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Employee count = " + &
		FORMAT$(EMP_COUNT%, "#####"), 0%)

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
