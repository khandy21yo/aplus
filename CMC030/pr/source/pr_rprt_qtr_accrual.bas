1	%TITLE "QTR - Quarterly Accrual Report"
	%SBTTL "PR_RPRT_QTR_ACCRUAL"
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
	! ID:PR047
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This report lists accrual amounts/used amounts by quarter.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_QTR_ACCRUAL/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_QTR_ACCRUAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_QTR_ACCRUAL.OBJ;*
	!
	! Author:
	!
	!	04/09/92 - Kevin Handy
	!
	! Modification history:
	!
	!	10/29/92 - Kevin Handy
	!		Added ability to print out by alpha.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/14/93 - Kevin Handy
	!		Added a REGARDLESS to get on PR_REG_ERNDED.
	!
	!	09/30/94 - Kevin Handy
	!		Added ability to print annual totals by entering
	!		a zero for the quarter.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	12/14/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED) PR_REG_ERNDED_CDD PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP (PR_EMP_ACCRUAL) PR_EMP_ACCRUAL_CDD PR_EMP_ACCRUAL

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)		PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	RECORD PR_TEMP_RECORD
		STRING	CODE = 2
		STRING	SORTBY = 15
		STRING	EMPNUM = 10
		GFLOAT	ACC_HR
		GFLOAT	ACC_DOL
		GFLOAT	USE_HR
		GFLOAT	USE_DOL
	END RECORD

	MAP (PR_TEMP) PR_TEMP_RECORD PR_TEMP

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	!
	! Abstract:FLD01
	!	^*(01) From Employee\*
	!	.p
	!	The ^*From Employee\* field causes the printing
	!	to begin with a particular employee.
	!	.p
	!	A blank field will cause the report to start with the first employee in
	!	the file.
	!
	! Index:
	!	.x From Employee>Deduction Report
	!	.x Deduction Report>From Employee
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) To Employee\*
	!	.p
	!	The ^*To Employee\* setting causes the printing
	!	to end with a particular employee.
	!	.p
	!	A blank field will cause the report to end with the last employee in the
	!	file.
	!
	! Index:
	!	.x To Employee>Deduction Report
	!	.x Deduction Report>To Employee
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!	^*(03) Year\*
	!	.p
	!	The ^*Year\* field enters the year for which this report is
	!	to print.
	!	.p
	!	The format for entry is YYYY and the field must be entered.
	!
	! Index:
	!	.x Year>Deduction Report
	!	.x Deduction Report>Year
	!
	!--

	QTR$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	QTR% = VAL%(QTR$) - 1%
	!++
	!
	! Abstract:FLD04
	!	^*(04) Quarter\*
	!	.p
	!	The ^*Quarter\* field enters with the year for which this report is
	!	to print.
	!	.p
	!	Entering a zero (0) will display annual figures instead of a single quarter.
	!	.p
	!	The format for entry is YYYY and the field must be entered.
	!
	! Index:
	!	.x Year>Deduction Report
	!	.x Deduction Report>Year
	!
	!--

	SORTBY$ = LEFT(UTL_REPORTX::OPTDEF(4%), 2%)
	!++
	!
	! Abstract:FLD05
	!	^*(05) Sort By\*
	!
	! Index:
	!	.x Sort By>Deduction Report
	!	.x Deduction Report>Sort By
	!
	! Datatype:TEXT
	! Size:2
	! Required:Y
	!--


	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Earnings and deduction definition
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Earnings and Deduction register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

330	!
	! Accrual Definition File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
	USE
		FILENAME$ = "PR_EMP_ACCRUAL"
		CONTINUE HelpError
	END WHEN

350	!
	! Open Tax Package file
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"

360	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			PRIMARY KEY &
			( &
				PR_TEMP::CODE, &
				PR_TEMP::SORTBY, &
				PR_TEMP::EMPNUM &
			)	DUPLICATES, &
			ACCESS MODIFY, &
			ALLOW NONE, &
			TEMPORARY
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

400	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_REG_ERNDED.CH%
		ELSE
			FIND #PR_REG_ERNDED.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file.  Reading register file", 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

410	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 490 IF ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 490 IF (PR_REG_ERNDED::EMPNUM > TO_ITEM$) AND (TO_ITEM$ <> "")

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
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		UTL_REPORTX::STAT = -1
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	!
	! Looking for gaining accrual amounts
	!
	IF PR_REG_ERNDED::ETYPE = "A"
	THEN
		!
		! With a "A" record, we know that it must be the first
		! one, and that it must be an accrual, so add it in
		! quickly.  Don't look for duplicates, don't look in
		! accrual definition file, just add it.
		!
		PR_TEMP::CODE	= PR_REG_ERNDED::CODE
		PR_TEMP::EMPNUM	= PR_REG_ERNDED::EMPNUM
		GOSUB SetSortby

		IF QTR% = -1%
		THEN
			PR_TEMP::ACC_HR = 0.0
			PR_TEMP::ACC_HR = PR_TEMP::ACC_HR + &
				PR_REG_ERNDED::REG_HRS(I%) + &
				PR_REG_ERNDED::PRE_HRS(I%) &
				FOR I% = 0% TO 3%
			PR_TEMP::ACC_DOL = 0.0
			PR_TEMP::ACC_DOL = PR_TEMP::ACC_DOL + &
				PR_REG_ERNDED::QTR_DOLL(I%) &
				FOR I% = 0% TO 3%
		ELSE
			PR_TEMP::ACC_HR = PR_REG_ERNDED::REG_HRS(QTR%) + &
				PR_REG_ERNDED::PRE_HRS(QTR%)
			PR_TEMP::ACC_DOL = PR_REG_ERNDED::QTR_DOLL(QTR%)
		END IF
		PR_TEMP::USE_HR = 0.0
		PR_TEMP::USE_DOL = 0.0

		PUT #PR_TEMP.CH%
		GOTO 480
	END IF

420	!
	! Looking for using accrual amounts
	!
	GOTO 480 IF PR_REG_ERNDED::ETYPE <> "P"

	!
	! Will get an error if not an accrual type
	!
	WHEN ERROR IN
		FIND #PR_EMP_ACCRUAL.CH%, &
			KEY #0% EQ PR_REG_ERNDED::EMPNUM + PR_REG_ERNDED::CODE, &
			REGARDLESS
	USE
		CONTINUE 480
	END WHEN

460	WHEN ERROR IN
		GET #PR_TEMP.CH%, &
			KEY #0% EQ PR_REG_ERNDED::CODE + PR_REG_ERNDED::EMPNUM
	USE
		CONTINUE 470
	END WHEN

	IF QTR% = -1%
	THEN
		PR_TEMP::USE_HR = PR_TEMP::USE_HR + &
			PR_REG_ERNDED::REG_HRS(I%) + &
			PR_REG_ERNDED::PRE_HRS(I%) &
			FOR I% = 0% TO 3%
		PR_TEMP::USE_DOL = PR_TEMP::USE_DOL + &
			PR_REG_ERNDED::QTR_DOLL(I%) &
			FOR I% = 0% TO 3%
	ELSE
		PR_TEMP::USE_HR = PR_TEMP::USE_HR + &
			PR_REG_ERNDED::REG_HRS(QTR%) + &
			PR_REG_ERNDED::PRE_HRS(QTR%)
		PR_TEMP::USE_DOL = PR_TEMP::USE_DOL + &
			PR_REG_ERNDED::QTR_DOLL(QTR%)
	END IF

	UPDATE #PR_TEMP.CH%

	GOTO 480

470	PR_TEMP::CODE	= PR_REG_ERNDED::CODE
	PR_TEMP::EMPNUM	= PR_REG_ERNDED::EMPNUM
	GOSUB SetSortby

	IF PR_REG_ERNDED::ETYPE = "A"
	THEN
		IF QTR% = -1%
		THEN
			PR_TEMP::ACC_HR = 0.0
			PR_TEMP::ACC_HR = PR_TEMP::ACC_HR + &
				PR_REG_ERNDED::REG_HRS(I%) + &
				PR_REG_ERNDED::PRE_HRS(I%) &
				FOR I% = 0% TO 3%
			PR_TEMP::ACC_DOL = 0.0
			PR_TEMP::ACC_DOL = PR_TEMP::ACC_DOL + &
				PR_REG_ERNDED::QTR_DOLL(QTR%) &
				FOR I% = 0% TO 3%
		ELSE
			PR_TEMP::ACC_HR = PR_REG_ERNDED::REG_HRS(QTR%) + &
				PR_REG_ERNDED::PRE_HRS(QTR%)
			PR_TEMP::ACC_DOL = PR_REG_ERNDED::QTR_DOLL(QTR%)
		END IF
		PR_TEMP::USE_HR = 0.0
		PR_TEMP::USE_DOL = 0.0
	ELSE
		PR_TEMP::ACC_HR = 0.0
		PR_TEMP::ACC_DOL = 0.0
		IF QTR% = -1%
		THEN
			PR_TEMP::USE_HR = 0.0
			PR_TEMP::USE_HR = PR_TEMP::USE_HR + &
				PR_REG_ERNDED::REG_HRS(I%) + &
				PR_REG_ERNDED::PRE_HRS(I%) &
				FOR I% = 0% TO 3%
			PR_TEMP::USE_DOL = 0.0
			PR_TEMP::USE_DOL = PR_TEMP::USE_DOL + &
				PR_REG_ERNDED::QTR_DOLL(I%) &
				FOR I% = 0% TO 3%
		ELSE
			PR_TEMP::USE_HR = PR_REG_ERNDED::REG_HRS(QTR%) + &
				PR_REG_ERNDED::PRE_HRS(QTR%)
			PR_TEMP::USE_DOL = PR_REG_ERNDED::QTR_DOLL(QTR%)
		END IF
	END IF

	PUT #PR_TEMP.CH%

480	GOTO 410

490	!

	%Page


 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Quarterly Accrual Report"
	IF QTR% = -1%
	THEN
		TITLE$(2%) = "For the Year " + YYYY$
	ELSE
		TITLE$(2%) = "For the " + QTR$ + &
			PRNT_NUMBERITH(QTR% + 1%) + &
			" Quarter of " + YYYY$
	END IF
	TITLE$(3%) = "Payroll System"
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "ED                                          " + &
		"           ------ AccruaL -------  -------- Use " + &
		"------------  ------ Change -------"
	TITLE$(6%) = "CD  Description      EmpNum      EmpName    " + &
		"              Hours      Dollars      Hours     " + &
		" Dollars         Hours      Dollars"
	TITLE$(7%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EType:001,$ERNDEDCode:006,$CodeDescr:023," + &
		"$EmpNum:035,$EmpName:057,VFirstQtrDol:072,VSecondQtrDol:,086" + &
		"VThirdQtrDol:100,VFourthQtrDol:114,VTotalDollars:128"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	COUNT% = 0%

	WHEN ERROR IN
		RESET #PR_TEMP.CH%
	USE
		FILENAME$ = "PR_TEMP"
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
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	IF TEST_CODE$ <> PR_TEMP::CODE
	THEN
		GOSUB Summary
		TEST_CODE$ = PR_TEMP::CODE
	END IF

17060	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, KEY #0% EQ PR_TEMP::EMPNUM, REGARDLESS
	USE
		PR_EMP_MASTER::EMPNUM	= PR_TEMP::EMPNUM
		PR_EMP_MASTER::EMPNAME	= "???????????????????????????????"
		PR_EMP_MASTER::SSN	= "???-??-????"
		PR_EMP_MASTER::HIREDAY	= "00000000"

		CONTINUE 17070 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17070	IF (PR_ERNDED_DEF::CODE <> PR_TEMP::CODE)
	THEN
		WHEN ERROR IN
			GET #PR_ERNDED_DEF.CH%, &
				KEY #0% EQ "P" + PR_TEMP::CODE, &
				REGARDLESS
		USE
			PR_ERNDED_DEF::ETYPE = "P"
			PR_ERNDED_DEF::CODE  = PR_TEMP::CODE
			PR_ERNDED_DEF::DESCR = "????????????????????"

			CONTINUE 17100 IF ERR = 155%
			FILENAME$ = "PR_ERNDED_DEF"
			CONTINUE HelpError
		END WHEN
	END IF

17100	!
	! Print total for employee
	!
	TEXT$ = PR_TEMP::CODE + "  " + &
		LEFT(PR_ERNDED_DEF::DESCR, 15%) + "  " + &
		PR_TEMP::EMPNUM + "  " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 20%) + " " + &
		FORMAT$(PR_TEMP::ACC_HR, "#,###,###.##") + &
		FORMAT$(PR_TEMP::ACC_DOL, "#,###,###.##") + &
		FORMAT$(PR_TEMP::USE_HR, "#,###,###.##") + &
		FORMAT$(PR_TEMP::USE_DOL, "#,###,###.##") + &
		FORMAT$(PR_TEMP::ACC_HR - PR_TEMP::USE_HR, "#,###,###.##") + &
		FORMAT$(PR_TEMP::ACC_DOL - PR_TEMP::USE_DOL, "#,###,###.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	SUMM_QTR_ACC_DOL = SUMM_QTR_ACC_DOL + PR_TEMP::ACC_DOL
	SUMM_QTR_ACC_HR = SUMM_QTR_ACC_HR + PR_TEMP::ACC_HR
	SUMM_QTR_USE_DOL = SUMM_QTR_USE_DOL + PR_TEMP::USE_DOL
	SUMM_QTR_USE_HR = SUMM_QTR_USE_HR + PR_TEMP::USE_HR

	COUNT% = COUNT% + 1%

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB Summary

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close channel
	!
	CLOSE PR_TEMP.CH%

17510	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 Summary:
	!******************************************************************
	! Look up state profile record
	!******************************************************************

	IF COUNT% <> 0%
	THEN
		TEXT$ = "                    " + &
			"       Total                      " + &
			FORMAT$(SUMM_QTR_ACC_HR, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_ACC_DOL, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_USE_HR, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_USE_DOL, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_ACC_HR - SUMM_QTR_USE_HR, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_ACC_DOL - SUMM_QTR_USE_DOL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	SUMM_QTR_ACC_DOL = 0.0
	SUMM_QTR_ACC_HR = 0.0
	SUMM_QTR_USE_DOL = 0.0
	SUMM_QTR_USE_HR = 0.0

	COUNT% = 0%

	RETURN

	%Page

 SetSortby:
18000	!*******************************************************************
	! Set up the sortby field based on the SORTBY$ and PR_TEMP::EMPNUM
	!*******************************************************************

	SELECT SORTBY$

	CASE "SO"
		PR_TEMP::SORTBY = ""

		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_TEMP::EMPNUM, &
				REGARDLESS &
				UNLESS PR_TEMP::EMPNUM = PR_EMP_MASTER::EMPNUM
		USE
			CONTINUE 18090
		END WHEN

		PR_TEMP::SORTBY = PR_EMP_MASTER::SORT

	CASE ELSE

		PR_TEMP::SORTBY = PR_TEMP::EMPNUM
	END SELECT

18090	RETURN

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
