1	%TITLE "Payroll Net Check Report"
	%SBTTL "PR_RPRT_TRN_CHECKBYLOC"
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
	! Abstract:HELP
	!	.p
	!	This report prints out the net check register by location.
	!
	! Index:
	!
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_CHECKBYLOC/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_CHECKBYLOC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_CHECKBYLOC.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	04/05/89 - Kevin Handy
	!		Modified to RSET the check number into the record
	!		so 9 would sort before 1000.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	07/05/90 - Kevin Handy
	!		Modified so that it will not modify a closed
	!		payroll folder by adding check records.
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
	!	08/14/96 - Kevin Handy
	!		Lose extra '&' before 'end if'.
	!		Reformat source code.
	!
	!	08/15/96 - Kevin Handy
	!		Lose another extra '&' before 'end if'.
	!
	!	03/13/97 - Kevin Handy
	!		Handle FH code.
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	12/04/2000 - Kevin Handy
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

	RECORD PR_TEMP_CHECK_CDD
		STRING	LOCATION = 4%
		STRING	CHECK = 6%
		STRING	EMPNUM = 10%
		STRING	PR_END_DATE = 8%
	END RECORD

	MAP (PR_TEMP_CHECK)	PR_TEMP_CHECK_CDD	PR_TEMP_CHECK

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up compiling options
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

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
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Check folder
	!
	WHEN ERROR IN
		IF USE_HISTORY% = 0%
		THEN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.CRE"
		ELSE
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.OPN"
			PR_TRN_CHECK.CH% = PR_HIS_CHECK.CH%
		END IF
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

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
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

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
		CONTINUE 700
	END WHEN

510	!
	! Get ded record
	!
	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 700
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
		FIND #PR_TRN_CHECK.CH%, KEY #0% EQ PR_TRN_DED::EMPNUM + &
			PR_TRN_DED::PR_END_DATE, REGARDLESS
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

	%PAGE

700	!
	! Create temporary check file index
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STATUS%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP_CHECK, &
			PRIMARY KEY (PR_TEMP_CHECK::LOCATION,PR_TEMP_CHECK::CHECK) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, &
			ALLOW NONE

		RESET #PR_TRN_CHECK.CH%
	USE
		FILENAME$ = "PR_TEMP_CHECK"
		CONTINUE HelpError
	END WHEN

710	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%, REGARDLESS
	USE
		CONTINUE 790 IF ERR = 11%
		FILENAME$ = "PR_TEMP_CHECK"
		CONTINUE HelpError
	END WHEN

	IF USE_HISTORY%
	THEN
		PR_TRN_CHECK = PR_HIS_CHECK
	END IF

720	PR_EMP_MASTER::LOCATION = ""

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TRN_CHECK::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 730
	END WHEN

730	GOTO 710 IF (PR_TRN_CHECK::CHECK > TO_ITEM$) AND &
		TO_ITEM$ <> ""
	GOTO 710 IF (PR_TRN_CHECK::CHECK < FROM_ITEM$) AND &
		FROM_ITEM$ <> ""

	PR_TEMP_CHECK::LOCATION = PR_EMP_MASTER::LOCATION
	RSET PR_TEMP_CHECK::CHECK = TRM$(PR_TRN_CHECK::CHECK)
	PR_TEMP_CHECK::EMPNUM = PR_TRN_CHECK::EMPNUM
	PR_TEMP_CHECK::PR_END_DATE = PR_TRN_CHECK::PR_END_DATE

	WHEN ERROR IN
		PUT #PR_TEMP.CH%
	USE
		FILENAME$ = "PR_TEMP_CHECK"
		CONTINUE HelpError
	END WHEN

	GOTO 710

790	!

 ReportTitle:
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Net Check Register by Location"
	TITLE$(2%) = "For the Payroll Folder Dated:  " + &
		PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = "."
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "CkNum  EmpNum     EmpName                  " + &
		"Date        Hrs    Units  GrossPay Non-Comp     FICA  " + &
		"Federal    State Misc/Ded  NetCheck"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$CheckNum:006,$EmpNum:017,$EmpName:040,DPRDate:049," + &
		"VHours:058,VUnits:067,VGrossPay:077,VNonComp:086," + &
		"VFICA:095,VFederal:104,VState:113,VMiscDed:122,VNetCheck:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	THIS_LOC$ = "00000000"

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

	!
	! Check current record
	!
	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%

17040	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_TEMP_CHECK::EMPNUM, &
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

	GOTO 17060 IF PR_TEMP_CHECK::EMPNUM <> PR_TRN_PAY::EMPNUM

	IF PR_TRN_PAY::PR_END_DATE = PR_TEMP_CHECK::PR_END_DATE
	THEN
		EMP_TOTAL(1%) = FUNC_ROUND(EMP_TOTAL(1%) + &
			PR_TRN_PAY::REG_HR + &
			PR_TRN_PAY::OVT_HR, 2%)
		EMP_TOTAL(2%) = FUNC_ROUND(EMP_TOTAL(2%) + &
			PR_TRN_PAY::PIECE, 2%)
		EMP_TOTAL(3%) = FUNC_ROUND(EMP_TOTAL(3%) + &
			PR_TRN_PAY::GROSS, 2%)
	END IF

	GOTO 17050

17060	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ PR_TEMP_CHECK::EMPNUM, &
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

	GOTO 17080 IF PR_TRN_DED::EMPNUM <> PR_TEMP_CHECK::EMPNUM

	IF PR_TRN_DED::PR_END_DATE = PR_TEMP_CHECK::PR_END_DATE
	THEN
		IF PR_TRN_DED::DTYPE = "M" OR PR_TRN_DED::DTYPE = "T"
		THEN
			EMP_TOTAL(4%) = FUNC_ROUND(EMP_TOTAL(4%) + &
				PR_TRN_DED::AMOUNT, 2%)
		ELSE
			SELECT PR_TRN_DED::CODE

			CASE "FI", "FW"
				EMP_TOTAL(5%) = FUNC_ROUND(EMP_TOTAL(5%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE "FW"
				EMP_TOTAL(6%) = FUNC_ROUND(EMP_TOTAL(6%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE "SW"
				EMP_TOTAL(7%) = FUNC_ROUND(EMP_TOTAL(7%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE ELSE
				EMP_TOTAL(8%) = FUNC_ROUND(EMP_TOTAL(8%) + &
					PR_TRN_DED::AMOUNT, 2%)

			END SELECT
		END IF
	END IF

	GOTO 17070

17080	!
	! Master file look up
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TEMP_CHECK::EMPNUM, &
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
	GOSUB LocTotal IF THIS_LOC$ <> PR_TEMP_CHECK::LOCATION

	EMP_TOTAL(9%) = FUNC_ROUND(EMP_TOTAL(3%) - EMP_TOTAL(5%) - &
		EMP_TOTAL(6%) - EMP_TOTAL(7%) - &
		EMP_TOTAL(8%), 2%)

	GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 9%
	LOC_TOTAL(LOOP%) = LOC_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 9%

	TEXT$ = PR_TEMP_CHECK::CHECK + " " + &
		PR_TEMP_CHECK::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 22%) + " " + &
		PRNT_DATE(PR_TEMP_CHECK::PR_END_DATE, 6%) + " " + &
		FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(2%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(3%), "######.## ") + &
		FORMAT$(EMP_TOTAL(4%), "#####.##") + &
		FORMAT$(EMP_TOTAL(5%), "######.## ") + &
		FORMAT$(EMP_TOTAL(6%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(7%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(8%), "#####.##") + &
		FORMAT$(EMP_TOTAL(9%), "#######.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO 17020

 LocTotal:
18000	!
	! Handle end of report
	!
	GOTO 18090 IF THIS_LOC$ = "00000000"

	TEXT$ = "                  Location Total                  " + &
		FORMAT$(LOC_TOTAL(1%), "#####.## ") + &
		FORMAT$(LOC_TOTAL(2%), "#####.## ") + &
		FORMAT$(LOC_TOTAL(3%), "######.## ") + &
		FORMAT$(LOC_TOTAL(4%), "#####.##") + &
		FORMAT$(LOC_TOTAL(5%), "######.## ") + &
		FORMAT$(LOC_TOTAL(6%), "#####.## ") + &
		FORMAT$(LOC_TOTAL(7%), "#####.## ") + &
		FORMAT$(LOC_TOTAL(8%), "#####.##") + &
		FORMAT$(LOC_TOTAL(9%), "#######.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, -3%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", -2%)

18090	LOC_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%
	TITLE$(3%) = "Location: " + PR_TEMP_CHECK::LOCATION

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 3000%) &
		IF THIS_LOC$ <> "00000000"

	THIS_LOC$ = PR_TEMP_CHECK::LOCATION

	RETURN

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB LocTotal

	TEXT$ = "                  Grand Total                     " + &
		FORMAT$(GRAND_TOTAL(1%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(4%), "#####.##") + &
		FORMAT$(GRAND_TOTAL(5%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(7%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(8%), "#####.##") + &
		FORMAT$(GRAND_TOTAL(9%), "#######.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "Employee count = " + &
		FORMAT$(EMP_COUNT%, "#####"), 0%)

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	CLOSE #PR_TEMP.CH%

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
