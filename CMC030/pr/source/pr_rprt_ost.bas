1	%TITLE "Payroll OST/SDI Report"
	%SBTTL "PR_RPRT_OST"
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
	! ID:PR043
	!
	! Abstract:HELP
	!	.p
	!	The ^*SDI Report\* option
	!	prints the
	!	required information for State Disability Insurance reporting.
	!	This information is listed in the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	State
	!	.le
	!	Employee Number
	!	.le
	!	Employer Tax Number
	!	.le
	!	Employee Name
	!	.le
	!	Other State Tax Percentage
	!	.le
	!	Other State Tax Limit
	!	.le
	!	State Earning
	!	.le
	!	Other State Tax Earning
	!	.le
	!	State Tax
	!	.le
	!	Other State Tax
	!	.els
	!
	! Index:
	!	.x SDI>Report
	!	.x Report>SDI
	!	.x State Disability Insurance>Report
	!	.x Report>State Disability Insurance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_OST/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_OST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_OST.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI tax type.
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP file TEMPORARY so that we don't
	!		forget to delete it.
	!
	!	01/27/90 - Kevin Handy
	!		Completely rewrote section that calculates taxes
	!		and other such.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or to a DIF file.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some filenames in the error trapping.
	!
	!	05/14/91 - J. Shad Rydalch
	!		Seperate list into workmen comp codes and print a total.
	!
	!	05/15/91 - Kevin Handy
	!		Modifications to get Totals correct, and skip second
	!		lookup in the master file.
	!		Fixed totals so they are correcter.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_REG_ERNDED.CH and PR_ERNDED_DEF.CH
	!		from PR_FUNC_READTAXES.
	!
	!	07/13/91 - Kevin Handy
	!		Removed file PR_REG_ERNDED, which is no longer
	!		used in this program.
	!
	!	07/13/91 - Kevin Handy
	!		Removed file PR_ERNDED_DEF, which is no longer
	!		used in this program.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/27/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	06/27/95 - Kevin Handy
	!		Lose check for 1099 flag, so we don't have any
	!		magic happening.
	!
	!	09/11/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code
	!		Use integer for #key
	!
	!	05/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	11/16/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:PR_STRUCTURE.INC"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES) PR_REG_TAXES_CDD PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP (PR_TAX_TABLE) PR_TAX_TABLE_CDD PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	RECORD PR_TEMP_CDD
		STRING	STCODE = 2%
		STRING	WCCODE = 6%
		STRING	SORTKEY = 20%
		STRING	EMPNUM = 10%
		STRING	EMPNAME = 30%
		REAL	STWAGES(3%)
		REAL	OSTWAGES(3%)
		REAL	TAX(3%)
		REAL	OST(3%)
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	DIM PR_TAXES_STRUCT PR_TAXES(50%)

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)
	EXTERNAL LONG   FUNCTION PR_FUNC_READTAXES

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
	! Abstract:FLD01
	!	^*(01) From State\*
	!	.p
	!	The ^*From State\* field enters the state with
	!	which the report will begin printing. A blank
	!	field will cause the report to begin with the first state in the
	!	file.
	!	.p
	!	The field will accommodate a two digit state postal code.
	!
	! Index:
	!	.x From State>State Disability Insurance Report
	!	.x State Disability Insurance Report>From State
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To State\*
	!	.p
	!	The ^*To State\* field enters a code for the
	!	state with which the report will end. A blank will cause the
	!	report to end with the last state in the file.
	!	.p
	!	The field will contain a two digit state postal code.
	!
	! Index:
	!	.x To State>State Disability Insurance Report
	!	.x State Disability Insurance Report>To State
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU, SN, NA, SO)\*
	!	.p
	!	The ^*Sort\* field enters a code which
	!	causes the report to be sorted in the indicated manner.
	!	.p
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	SN = Social Security Number
	!	.le
	!	NA = Name
	!	.le
	!	SO = Alphabetical (last name first)
	!	.lm -10
	!	.p
	!	.els
	!	An entry is required in this field and only the above codes
	!	are valid.
	!
	! Index:
	!	.x Sort>State Disability Insurance Report
	!	.x State Disability Insurance Report>Sort
	!
	!--


	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Year (YYYY)\*
	!	.p
	!	The ^*Year\* field enters the year for which
	!	this report will print.
	!	.p
	!	This field requires an entry. The format for entry is YYYY.
	!
	! Index:
	!	.x Year>State Disability Insurance Report
	!	.x State Disability Insurance Report>Year
	!
	!--


	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%))

	!++
	! Abstract:FLD05
	!	^*(05) Quarter\*
	!	.p
	!	The ^*Quarter\* field enters the payroll
	!	quarter for which this report will print.
	!	.p
	!	This field requires an entry. The field will accommodate
	!	a one digit number.
	!
	! Index:
	!	.x Quarter>State Disability Insurance Report
	!	.x State Disability Insurance Report>Quarter
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

330	!
	! Open Tax register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

340	!
	! Open Tax Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

350	!
	! Open Tax Table
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY &
			( &
				PR_TEMP::STCODE, &
				PR_TEMP::WCCODE, &
				PR_TEMP::SORTKEY, &
				PR_TEMP::EMPNUM &
			) NODUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

400	WHEN ERROR IN
		RESET #PR_EMP_MASTER.CH%
	USE
		FILENAME$ = "PR_EMP_MASTER"
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
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE 600 IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
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
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	!
	! Total earnings by quarter for an employee
	!
500	!
	! Set sort key
	!
	SELECT SORTBY$

	CASE "NU"
		SORTKEY$ = PR_EMP_MASTER::EMPNUM

	CASE "NA"
		SORTKEY$ = PR_EMP_MASTER::EMPNAME

	CASE "SN"
		SORTKEY$ = PR_EMP_MASTER::SSN

	CASE "DP"
		SORTKEY$ = PR_EMP_MASTER::DEPT

	CASE ELSE
		SORTKEY$ = PR_EMP_MASTER::SORT

	END SELECT

550	!
	! Get reportable amounts/taxes
	!
	X% = PR_FUNC_READTAXES(PR_EMP_MASTER::EMPNUM, &
		PR_REG_TAXES.CH%, &
		PR_TAXES%, &
		PR_TAXES())
	!
	! Figure out what states are involved
	!
	STATE_LOOP% = 0%

	FOR LOOP% = 1% TO PR_TAXES%

		GOTO 557 IF INSTR(1%, "SW!SI!SX!", PR_TAXES(LOOP%)::TTYPE) = 0%

		GOTO 557 IF STATE$(I%) = PR_TAXES(LOOP%)::CODE &
			FOR I% = 1% TO STATE_LOOP%

555		STATE_LOOP% = STATE_LOOP% + 1%
		STATE$(STATE_LOOP%) = PR_TAXES(LOOP%)::CODE

557	NEXT LOOP%

560	FOR LOOP% = 1% TO STATE_LOOP%

		IF ((STATE$(LOOP%) >= FROM_ITEM$) OR (FROM_ITEM$ = "")) AND &
			((STATE$(LOOP%) <= TO_ITEM$) OR (TO_ITEM$ = ""))
		THEN
			PR_TEMP::STCODE		= STATE$(LOOP%)
			PR_TEMP::WCCODE		= PR_EMP_MASTER::WC
			PR_TEMP::SORTKEY	= SORTKEY$
			PR_TEMP::EMPNUM		= PR_EMP_MASTER::EMPNUM
			PR_TEMP::EMPNAME	= PR_EMP_MASTER::EMPNAME

			FOR I% = 0% TO 3%
				PR_TEMP::STWAGES(I%) = 0.0
				PR_TEMP::OSTWAGES(I%) = 0.0
				PR_TEMP::TAX(I%) = 0.0
				PR_TEMP::OST(I%) = 0.0
			NEXT I%

			FOR J% = 1% TO PR_TAXES%

				IF PR_TAXES(J%)::CODE = STATE$(LOOP%)
				THEN
					SELECT PR_TAXES(J%)::TTYPE

					CASE "SW"
						PR_TEMP::STWAGES(I%) = &
							PR_TEMP::STWAGES(I%) + &
							PR_TAXES(J%)::REPORTABLE(I%) &
							FOR I% = 0% TO 3%
						PR_TEMP::TAX(I%) = &
							PR_TEMP::TAX(I%) + &
							PR_TAXES(J%)::TAX(I%) &
							FOR I% = 0% TO 3%

					CASE "SI", "SX"
						PR_TEMP::OSTWAGES(I%) = &
							PR_TEMP::OSTWAGES(I%) + &
							PR_TAXES(J%)::REPORTABLE(I%) &
							FOR I% = 0% TO 3%
						PR_TEMP::OST(I%) = &
							PR_TEMP::OST(I%) + &
							PR_TAXES(J%)::TAX(I%) &
							FOR I% = 0% TO 3%
					END SELECT
				END IF

			NEXT J%

			PUT #PR_TEMP.CH%
		END IF

	NEXT LOOP%

590	GOTO 410

600	!

	%PAGE


 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "State/SDI Tax Report - " + NUM1$(QTR%) + &
		MID("stndrdth", QTR% * 2% - 1%, 2%) + " Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(35%) + &
		"------------------Year to Date----------------- !" + &
		" ----------------Quarter to Date----------------"
	TITLE$(5%) = "EmpNum     EmpName                 " + &
		" State Earn.    SDI Earn.  State Tax    SDI Tax !" + &
		" State Earn.    SDI Earn.  State Tax    SDI Tax"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:034,VYTDStErn:047," + &
		"VYTDSDIErn:060,VYTDStTax:071,VYTDSDITax:082," + &
		"$YTDCorr:083,VQTDStErn:096,VQTDSDIErn:109," + &
		"VQTDStTax:120,VQTDSDITax:131,$QTDCorr:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
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

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 8%

	FOR I% = 0% TO QTR% - 1%
		EMP_TOTAL(1%) = FUNC_ROUND(EMP_TOTAL(1%) + &
			PR_TEMP::STWAGES(I%), 2%)
		EMP_TOTAL(2%) = FUNC_ROUND(EMP_TOTAL(2%) + &
			PR_TEMP::OSTWAGES(I%), 2%)
		EMP_TOTAL(3%) = FUNC_ROUND(EMP_TOTAL(3%) + &
			PR_TEMP::TAX(I%), 2%)
		EMP_TOTAL(4%) = FUNC_ROUND(EMP_TOTAL(4%) + &
			PR_TEMP::OST(I%), 2%)
	NEXT I%

	EMP_TOTAL(5%) = FUNC_ROUND(PR_TEMP::STWAGES(QTR% - 1%), 2%)
	EMP_TOTAL(6%) = FUNC_ROUND(PR_TEMP::OSTWAGES(QTR% - 1%), 2%)
	EMP_TOTAL(7%) = FUNC_ROUND(PR_TEMP::TAX(QTR% - 1%), 2%)
	EMP_TOTAL(8%) = FUNC_ROUND(PR_TEMP::OST(QTR% - 1%), 2%)

17100	!
	! Print total for employee
	!
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + EMP_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 8%

	IF SUM <> 0.0
	THEN
		IF WC_CODE$ <> PR_TEMP::WCCODE
		THEN
			GOSUB WCtotal
		END IF

		IF TEST_STCODE$ <> PR_TEMP::STCODE
		THEN
			GOSUB STProfile
		END IF

		TEST = EMP_TOTAL(2%)
		EMP_TOTAL(2%) = OT_ANL_MAX IF EMP_TOTAL(2%) >= OT_ANL_MAX
		EMP_TOTAL(6%) = OT_ANL_MAX - (TEST - EMP_TOTAL(6%)) &
			IF EMP_TOTAL(2%) >= OT_ANL_MAX
		EMP_TOTAL(6%) = 0.0 IF EMP_TOTAL(6%) < 0.0

		YTD_CORR$, QTD_CORR$ = " "

		TEST = EMP_TOTAL(2%) * OT_ANL_PCT
		YTD_CORR$ = "*" IF ABS(TEST-EMP_TOTAL(4%)) > 0.05

		TEST = EMP_TOTAL(6%) * OT_ANL_PCT
		QTD_CORR$ = "*" IF ABS(TEST-EMP_TOTAL(8%)) > 0.05

		STATE_TOTAL(LOOP%) = STATE_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 8%

		WCCODE_TOTAL(LOOP%) = WCCODE_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 8%

		GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 8%

		TEXT$ = PR_TEMP::EMPNUM + " " + &
			LEFT(PR_TEMP::EMPNAME, 23%) + " " + &
			FORMAT$(EMP_TOTAL(1%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(2%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(3%), "###,###.## ") + &
			FORMAT$(EMP_TOTAL(4%), "###,###.##") + &
			YTD_CORR$ + " " + &
			FORMAT$(EMP_TOTAL(5%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(6%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(7%), "###,###.## ") + &
			FORMAT$(EMP_TOTAL(8%), "###,###.##") + &
			QTD_CORR$

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB WCtotal

	GOSUB STtotal

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "         Grand Total               " + &
		FORMAT$(GRAND_TOTAL(1%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "###,###.## ") + &
		FORMAT$(GRAND_TOTAL(4%), "###,###.##  ") + &
		FORMAT$(GRAND_TOTAL(5%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(7%), "###,###.## ") + &
		FORMAT$(GRAND_TOTAL(8%), "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

 STProfile:
	!******************************************************************
	! Look up state profile record
	!******************************************************************

	CODE$ = PR_TEMP::STCODE
	REPNO$ = "??????????????"
	OT_ANL_PCT = 0%
	OT_ANL_MAX = 0.0

18100	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "S" + CODE$, REGARDLESS
	USE
		CONTINUE 18110 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	REPNO$ = PR_TAX_PROFILE_S::REPNO

18110	WHEN ERROR IN
		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "S" + CODE$, REGARDLESS
	USE
		CONTINUE 18190 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	OT_ANL_PCT = PR_TAX_TABLE::OT_ANL_PCT / 100.0
	OT_ANL_MAX = PR_TAX_TABLE::OT_ANL_MAX

18190	TITLE$(4%) = "State: " + CODE$ + &
		" Employer Tax # " + REPNO$ + "  SDI Percent: " + &
		NUM1$(OT_ANL_PCT * 100.0) + "%   SDI Limit: " + &
		NUM1$(OT_ANL_MAX)

	IF TEST_STCODE$ <> ""
	THEN
		GOSUB STtotal
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
	END IF

	TEST_STCODE$ = PR_TEMP::STCODE

	RETURN

	!*******************************************************************
	! Print state total
	!*******************************************************************
 STtotal:
	IF TEST_STCODE$ <> ""
	THEN
		TEXT$ = "         State Total               " + &
			FORMAT$(STATE_TOTAL(1%), "#,###,###.## ") + &
			FORMAT$(STATE_TOTAL(2%), "#,###,###.## ") + &
			FORMAT$(STATE_TOTAL(3%), "###,###.## ") + &
			FORMAT$(STATE_TOTAL(4%), "###,###.##  ") + &
			FORMAT$(STATE_TOTAL(5%), "#,###,###.## ") + &
			FORMAT$(STATE_TOTAL(6%), "#,###,###.## ") + &
			FORMAT$(STATE_TOTAL(7%), "###,###.## ") + &
			FORMAT$(STATE_TOTAL(8%), "###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	STATE_TOTAL(I%) = 0.0 FOR I% = 1% TO 8%

	RETURN

	!*******************************************************************
	! WC Total
	!*******************************************************************

 WCtotal:
	!
	! Print out a total for workmens comp
	!
	IF WC_CODE$ <> ""
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEXT$ = "        Workmen comp " + WC_CODE$ + " Total  " + &
			FORMAT$(WCCODE_TOTAL(1%), "#,###,###.## ") + &
			FORMAT$(WCCODE_TOTAL(2%), "#,###,###.## ") + &
			FORMAT$(WCCODE_TOTAL(3%), "###,###.## ") + &
			FORMAT$(WCCODE_TOTAL(4%), "###,###.##  ") + &
			FORMAT$(WCCODE_TOTAL(5%), "#,###,###.## ") + &
			FORMAT$(WCCODE_TOTAL(6%), "#,###,###.## ") + &
			FORMAT$(WCCODE_TOTAL(7%), "###,###.## ") + &
			FORMAT$(WCCODE_TOTAL(8%), "###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	WC_CODE$ = PR_TEMP::WCCODE
	WCCODE_TOTAL(I%) = 0.0 FOR I% = 1% TO 8%

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
