1	%TITLE "Payroll Deduction Report"
	%SBTTL "PR_RPRT_QTRDED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	!	.p
	!	The ^*Deduction report\* consists of a report of all non-tax deductions
	!	taken from each employee over a specified time period. This report contains
	!	the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Earned Deduction Type
	!	.le
	!	Earned Deduction Code
	!	.le
	!	Description
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	First Quarter Dollars
	!	.le
	!	Second Quarter Dollars
	!	.le
	!	Third Quarter Dollars
	!	.le
	!	Fourth Quarter Dollars
	!	.le
	!	Totals
	!	.els
	!
	! Index:
	!	.x Deduction>Report
	!	.x Report>Deduction
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_QTRDED/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_QTRDED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_QTRDED.OBJ;*
	!
	! Author:
	!
	!	06/22/89 - Kevin Handy
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent either to a spreadsheet or to a DIF file.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	08/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

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
		STRING	ETYPE = 1
		STRING	CODE = 2
		STRING	EMPNUM = 10
		GFLOAT	QTR_DOLL(3)
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
				PR_TEMP::ETYPE, &
				PR_TEMP::CODE, &
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
		"Creating work file by state.  Reading register file", 1%)

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
	! Routine to write record
	!
	IF (INSTR(1%, "PD", PR_REG_ERNDED::ETYPE) <> 0%)
	THEN
		PR_TEMP::ETYPE	= PR_REG_ERNDED::ETYPE
		PR_TEMP::CODE	= PR_REG_ERNDED::CODE
		PR_TEMP::EMPNUM	= PR_REG_ERNDED::EMPNUM
		PR_TEMP::QTR_DOLL(I%) = PR_REG_ERNDED::QTR_DOLL(I%) &
			FOR I% = 0% TO 3%

		PUT #PR_TEMP.CH%
	END IF

	GOTO 410

490	!

	%Page


 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Pay/Deduction Report"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = "Payroll System"
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "ED  ED                                          " + &
		"           -------------------------------- Dollars ----" + &
		"-------------------------"
	TITLE$(6%) = "TP  CD  Description      EmpNum      EmpName    " + &
		"              1stQuarter    2ndQuarter    3rdQuarter    " + &
		"4thQuarter         Totals"
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

	IF TEST_CODE$ <> PR_TEMP::ETYPE + PR_TEMP::CODE
	THEN
		GOSUB Summary
		TEST_CODE$ = PR_TEMP::ETYPE + PR_TEMP::CODE
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

17070	IF (PR_ERNDED_DEF::ETYPE <> PR_TEMP::ETYPE) OR &
		(PR_ERNDED_DEF::CODE <> PR_TEMP::CODE)
	THEN
		WHEN ERROR IN
			GET #PR_ERNDED_DEF.CH%, &
				KEY #0% EQ PR_TEMP::ETYPE + PR_TEMP::CODE, &
				REGARDLESS
		USE
			PR_ERNDED_DEF::ETYPE = PR_TEMP::ETYPE
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
	TEXT$ = PR_TEMP::ETYPE + "   " + &
		PR_TEMP::CODE + "  " + &
		LEFT(PR_ERNDED_DEF::DESCR, 15%) + "  " + &
		PR_TEMP::EMPNUM + "  " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 20%) + " " + &
		FORMAT$(PR_TEMP::QTR_DOLL(0%), "###,###,###.##") + &
		FORMAT$(PR_TEMP::QTR_DOLL(1%), "###,###,###.##") + &
		FORMAT$(PR_TEMP::QTR_DOLL(2%), "###,###,###.##") + &
		FORMAT$(PR_TEMP::QTR_DOLL(3%), "###,###,###.##") + &
		FORMAT$(PR_TEMP::QTR_DOLL(0%) + &
			PR_TEMP::QTR_DOLL(1%) + &
			PR_TEMP::QTR_DOLL(2%) + &
			PR_TEMP::QTR_DOLL(3%), "###,###,###.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	SUMM_QTR_DOLL(I%) = SUMM_QTR_DOLL(I%) + PR_TEMP::QTR_DOLL(I%) &
		FOR I% = 0% TO 3%
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
		TEXT$ = "                        " + &
			"       Total                      " + &
			FORMAT$(SUMM_QTR_DOLL(0%), "###,###,###.##") + &
			FORMAT$(SUMM_QTR_DOLL(1%), "###,###,###.##") + &
			FORMAT$(SUMM_QTR_DOLL(2%), "###,###,###.##") + &
			FORMAT$(SUMM_QTR_DOLL(3%), "###,###,###.##") + &
			FORMAT$(SUMM_QTR_DOLL(0%) + &
				SUMM_QTR_DOLL(1%) + &
				SUMM_QTR_DOLL(2%) + &
				SUMM_QTR_DOLL(3%), "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	SUMM_QTR_DOLL(I%) = 0.0 FOR I% = 0% TO 3%
	COUNT% = 0%

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
