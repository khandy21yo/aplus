1	%TITLE "Payroll Premium Pay Report"
	%SBTTL "PR_RPRT_SUMMARY_PREMIUM"
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
	! ID:PR058
	!
	! Abstract:HELP
	!	.p
	!	This report will print a summary of employees regular
	!	and his premium pay. This report contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	For the State of
	!	.le
	!	Regular Time
	!	.le
	!	Over Time
	!	.le
	!	Regular Pay
	!	.le
	!	Premium
	!	.els
	!
	! Index:
	!	.x Premium Pay>Report
	!	.x Report>Premium Pay
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_SUMMARY_PREMIUM/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_SUMMARY_PREMIUM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_SUMMARY_PREMIUM.OBJ;*
	!
	! Author:
	!
	!	03/02/90 - Kevin Handy
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	10/25/90 - Kevin Handy
	!		Added pay type "X" (eXcess).
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	MAP (PR_TEMP) &
		PR_TEMP.STATE$ = 4%, &
		PR_TEMP.EMPNUM$ = 10%, &
		PR_TEMP.PR_END_DATE$ = 08%, &
		PR_TEMP.REG_HR, &
		PR_TEMP.OVT_HR, &
		PR_TEMP.REG_PAY, &
		PR_TEMP.OVT_PAY

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	!
	! Dimension
	!
	DIM DATA_FILE$(200%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Find a channel for the TEMP file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Folder Date\*
	!	.p
	!	The ^*From Folder Date\* enters the folder date with which
	!	the report will begin printing.  A blank field causes the report to begin
	!	with the first folder date in the file.
	!
	! Index:
	!	.x From Folder Date
	!	.x Folder Date>From
	!
	!--

	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)
	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Folder Date\*
	!	.p
	!	The ^*To Folder Date\* enters the folder date with which
	!	the report will conclude printing. A blank field causes the report to conclude
	!	the printing with the last folder date in the file.
	!
	! Index:
	!	.x To Folder Date
	!	.x Folder Date>To
	!
	!--

	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

410	!
	! Create wc audit file
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file.  Reading Pay folder.", 1%)

	WHEN ERROR IN
		OPEN PR_TRN_PAY.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY (PR_TEMP.STATE$, &
				PR_TEMP.EMPNUM$, &
				PR_TEMP.PR_END_DATE$) &
			DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Open all payroll folder files
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

420		!
		! Open Pay folder
		!
		USE_HISTORY% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 425 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 430

425		!
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

430		!
		! Add to audit file
		!
		WHEN ERROR IN
			RESET #PR_TMP_PAY.CH%, KEY #0%
		USE
			CONTINUE 530
		END WHEN

440		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 530 IF ERR = 11%
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

		GOTO 440 IF PR_TRN_PAY::PTYPE = "A"

460		!
		! Look up state (See if we already have tax package record)
		!
		IF PR_TRN_PAY::TAX_PKG <> PR_TAX_PKG::TAX_PKG
		THEN
			STATE$ = "??"

			WHEN ERROR IN
				GET #PR_TAX_PKG.CH%, &
					KEY #0% EQ PR_TRN_PAY::TAX_PKG + "SW", &
					REGARDLESS
			USE
				CONTINUE 490 IF ERR = 155%
				FILENAME$ = "PR_TAX_PKG"
				CONTINUE HelpError
			END WHEN

			STATE$ = PR_TAX_PKG::CODE
		END IF

490		!
		! Calculate subject pay
		!
		SUBJECT_PREM = 0.0

		IF PR_TRN_PAY::RTYPE = "H" OR PR_TRN_PAY::RTYPE = "S" OR &
			PR_TRN_PAY::RTYPE = "X"
		THEN
			SUBJECT_PREM = FUNC_ROUND(PR_TRN_PAY::OVT_HR * &
				(PR_TRN_PAY::FACTOR/100. - 1.0) * &
				PR_TRN_PAY::HOUR_RATE, 2%)
			SUBJECT_PREM = 0.0 IF SUBJECT_PREM < 0.0
		END IF

		!
		! Set initial values in temp file
		!
		PR_TEMP.STATE$ = STATE$
		PR_TEMP.EMPNUM$ = PR_TRN_PAY::EMPNUM
		PR_TEMP.PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE
		PR_TEMP.REG_HR = PR_TRN_PAY::REG_HR
		PR_TEMP.OVT_HR = PR_TRN_PAY::OVT_HR
		PR_TEMP.REG_PAY = PR_TRN_PAY::GROSS - SUBJECT_PREM
		PR_TEMP.OVT_PAY = SUBJECT_PREM

		PUT #PR_TEMP.CH%

525		!
		! Get next payroll record
		!
		GOTO 440

530		CLOSE PR_TMP_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)
	!
	! Get next payroll
	!
	NEXT PR_LOOP%

	%PAGE

 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Workmen Comp Audit Report"
	TITLE$(2%) = "For the Payroll Folders Dated From:  " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		"  To:  " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "EmpNum     EmpName              RegTime   " + &
		"OverTime      RegPay     Premium"
	TITLE$(5%) = "."
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:029,VRegHours:040," + &
		"VOvertimeHrs:051,VRegPay:062,VOtertimePay:073"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	RESET #PR_TEMP.CH%, KEY #0%

	THIS_EMPNUM$ = ""
	THIS_STATE$ = "  "

	EMP_TOTAL_REG_HR = 0.0
	EMP_TOTAL_OVT_HR = 0.0
	EMP_TOTAL_REG_PAY = 0.0
	EMP_TOTAL_OVT_PAY = 0.0

	STATE_TOTAL_REG_HR = 0.0
	STATE_TOTAL_OVT_HR = 0.0
	STATE_TOTAL_REG_PAY = 0.0
	STATE_TOTAL_OVT_PAY = 0.0

	GRAND_TOTAL_REG_HR = 0.0
	GRAND_TOTAL_OVT_HR = 0.0
	GRAND_TOTAL_REG_PAY = 0.0
	GRAND_TOTAL_OVT_PAY = 0.0

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

	IF PR_TEMP.STATE$ <> THIS_STATE$
	THEN
		GOSUB EmployeeTotal
		GOSUB StateTotal
	END IF

	IF PR_TEMP.EMPNUM$ <> THIS_EMPNUM$
	THEN
		GOSUB EmployeeTotal
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	EMP_TOTAL_REG_HR = EMP_TOTAL_REG_HR + PR_TEMP.REG_HR
	EMP_TOTAL_OVT_HR = EMP_TOTAL_OVT_HR + PR_TEMP.OVT_HR
	EMP_TOTAL_REG_PAY = EMP_TOTAL_REG_PAY + PR_TEMP.REG_PAY
	EMP_TOTAL_OVT_PAY = EMP_TOTAL_OVT_PAY + PR_TEMP.OVT_PAY

	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB EmployeeTotal
	GOSUB StateTotal
	GOSUB GrandTotal

 ExitProgram:
17500	!
	! Kill temp file
	!
	CLOSE PR_TEMP.CH%

17510	CALL OUTP_FINISH(UTL_REPORTX)

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

 EmployeeTotal:
17700	!******************************************************************
	! Print employee total
	!******************************************************************

	GOTO 17790 IF THIS_EMPNUM$ = ""

	!
	! Look up employee name
	!
	WORK_EMPNAME$ = STRING$(LEN(PR_EMP_MASTER::EMPNAME), 63%)

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, KEY #0% GE THIS_EMPNUM$, REGARDLESS
	USE
		CONTINUE 17710 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	WORK_EMPNAME$ = PR_EMP_MASTER::EMPNAME

17710	TEXT$ = LEFT(THIS_EMPNUM$ + SPACE$(10%), 10%) + " " + &
		LEFT(WORK_EMPNAME$ + SPACE$(18%), 18%) + " " + &
		FORMAT$(EMP_TOTAL_REG_HR, "#######.## ") + &
		FORMAT$(EMP_TOTAL_OVT_HR, "#######.## ") + &
		FORMAT$(EMP_TOTAL_REG_PAY, "#######.## ") + &
		FORMAT$(EMP_TOTAL_OVT_PAY, "#######.## ")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	STATE_TOTAL_REG_HR = STATE_TOTAL_REG_HR + EMP_TOTAL_REG_HR
	STATE_TOTAL_OVT_HR = STATE_TOTAL_OVT_HR + EMP_TOTAL_OVT_HR
	STATE_TOTAL_REG_PAY = STATE_TOTAL_REG_PAY + EMP_TOTAL_REG_PAY
	STATE_TOTAL_OVT_PAY = STATE_TOTAL_OVT_PAY + EMP_TOTAL_OVT_PAY

17790	THIS_EMPNUM$ = PR_TEMP.EMPNUM$

	EMP_TOTAL_REG_HR = 0.0
	EMP_TOTAL_OVT_HR = 0.0
	EMP_TOTAL_REG_PAY = 0.0
	EMP_TOTAL_OVT_PAY = 0.0

	RETURN

	%Page

 StateTotal:
	!******************************************************************
	! Print State total
	!******************************************************************

	GOTO 17890 IF STATE_TOTAL_REG_PAY = 0.0 AND &
		STATE_TOTAL_OVT_PAY = 0.0 AND &
		STATE_TOTAL_REG_HR = 0.0 AND &
		STATE_TOTAL_OVT_HR = 0.0

	TEXT$ = "          " + "State " + THIS_STATE$ + &
		" Total     " + &
		FORMAT$(STATE_TOTAL_REG_HR, "#######.## ") + &
		FORMAT$(STATE_TOTAL_OVT_HR, "#######.## ") + &
		FORMAT$(STATE_TOTAL_REG_PAY, "#######.## ") + &
		FORMAT$(STATE_TOTAL_OVT_PAY, "#######.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GRAND_TOTAL_REG_HR = GRAND_TOTAL_REG_HR + STATE_TOTAL_REG_HR
	GRAND_TOTAL_OVT_HR = GRAND_TOTAL_OVT_HR + STATE_TOTAL_OVT_HR
	GRAND_TOTAL_REG_PAY = GRAND_TOTAL_REG_PAY + STATE_TOTAL_REG_PAY
	GRAND_TOTAL_OVT_PAY = GRAND_TOTAL_OVT_PAY + STATE_TOTAL_OVT_PAY

17890	TITLE$(5%) = "For the State of " + PR_TEMP.STATE$

	IF THIS_STATE$ <> ""
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
	END IF

	THIS_STATE$ = PR_TEMP.STATE$

	STATE_TOTAL_REG_HR = 0.0
	STATE_TOTAL_OVT_HR = 0.0
	STATE_TOTAL_REG_PAY = 0.0
	STATE_TOTAL_OVT_PAY = 0.0

	RETURN

	%Page

 GrandTotal:
	!******************************************************************
	! Print State total
	!******************************************************************

	TEXT$ = "          " + "Grand Total        " + &
		FORMAT$(GRAND_TOTAL_REG_HR, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_OVT_HR, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_REG_PAY, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_OVT_PAY, "#######.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
