1	%TITLE "Payroll Time/Unit Report"
	%SBTTL "PR_RPRT_TRN_EXCESS"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:PR090
	!
	! Abstract:HELP
	!	.p
	!	The ^*Excess\* report prints out employees that
	!	are being subsidized at an hour rate over a piece rate
	!	for those employees using the "*X" (eXcess) pay type.
	!
	! Index:
	!	.x Excess>Report
	!	.x Report>Excess
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_EXCESS/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_EXCESS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_EXCESS.OBJ;*
	!
	! Author:
	!
	!	08/27/91 - JEFF BEARD
	!
	! Modification history:
	!
	!	09/09/91 - Kevin Handy
	!		Debugging, format changes &
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD    PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)	PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP	(PR_HIS_PAY)	PR_TRN_PAY_CDD		PR_HIS_PAY

	RECORD PR_TEMP_RECORD
		STRING JOB = 10
		STRING ACCOUNT = 18
		STRING EMP_NUM = 10
		GFLOAT REGULAR
		GFLOAT EXCESS
	END RECORD

	MAP (PR_TEMP)	PR_TEMP_RECORD	PR_TEMP

	DECLARE REAL	REG_TOTAL, EXCESS_TOTAL

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%))

	!++
	! Abstract:FLD01
	!	^*(01) Payroll Date\*
	!	.p
	!	The ^*Payroll Date\* field
	!	enters the payroll file date of the payroll which
	!	is to be printed.
	!	.p
	!	This field requires an entry. The format is MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Time Journal Report>Payroll Date
	!	.x Payroll Date>Time Journal Report
	!	.x Date>Time Journal Report
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
	USE_HISTORY% = 0%
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO 320

315	!
	! Open Pay History folder if regular folder not found
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
	!	CREATE TEMPORARY FILE
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)
	OPEN "PR_TEMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, TEMPORARY, &
		BUFFER 32%, MAP PR_TEMP, &
		PRIMARY KEY (PR_TEMP::JOB, &
			PR_TEMP::ACCOUNT, &
			PR_TEMP::EMP_NUM) &
			DUPLICATES, &
		ACCESS MODIFY, ALLOW NONE




	%PAGE

 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Excess Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		PRNT_DATE(BATCH_NO$, 8%)

	TITLE$(3%) = ""

	TITLE$(4%) = "Job Number "

	TITLE$(5%) = "           Account #     Emp #       Regular pay" + &
		"     Excess pay"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	REG_TOTAL = 0.0
	EXCESS_TOTAL = 0.0
	JOB_REG_TOT = 0.0
	JOB_EXCESS_TOT = 0.0

	TEMP.JOB$ = ""

	RESET #PR_TRN_PAY.CH%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
17030	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If history flag is set then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO GetNextRec IF EDIT$(PR_TRN_PAY::RTYPE, -1%) <> "X"
	GOTO GetNextRec IF EDIT$(PR_TRN_PAY::PTYPE, -1%) = "A"

	PR_TEMP::JOB		= PR_TRN_PAY::SUBACC
	PR_TEMP::ACCOUNT	= PR_TRN_PAY::ACCT
	PR_TEMP::EMP_NUM	= PR_TRN_PAY::EMPNUM
	PR_TEMP::REGULAR	= &
		FUNC_ROUND(PR_TRN_PAY::PIECE * PR_TRN_PAY::PIECE_RATE, 2%)
	PR_TEMP::EXCESS		= &
		FUNC_ROUND((PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR &
		* PR_TRN_PAY::FACTOR / 100) * PR_TRN_PAY::HOUR_RATE, 2%) &
		- PR_TEMP::REGULAR

17100	IF PR_TEMP::EXCESS > 0.0
	THEN
		PUT #PR_TEMP.CH%
	END IF

	GOTO GetNextRec

	%PAGE

	!*******************************************************************
	! Printing loop through generated file
	!*******************************************************************

17200	RESET #PR_TEMP.CH%

	TEMP.JOB$ = "12345678901234567890"

 PrintHeader:
17210	WHEN ERROR IN
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	IF TEMP.JOB$ = "12345678901234567890"
	THEN
		TEMP.JOB$ = PR_TEMP::JOB
		TEMP.ACC$ = PR_TEMP::ACCOUNT
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), PR_TEMP::JOB, 0%)
	END IF

	IF TEMP.JOB$ <> PR_TEMP::JOB
	THEN
		GOSUB AccountTotal
		GOSUB JobTotal
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

		TEMP.JOB$ = PR_TEMP::JOB
		TEMP.ACC$ = PR_TEMP::ACCOUNT
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), PR_TEMP::JOB, 0%)
	END IF


	IF TEMP.ACC$ <> PR_TEMP::ACCOUNT
	THEN
		GOSUB AccountTotal
		TEMP.ACC$ = PR_TEMP::ACCOUNT
	END IF

	TEXT$ = "             "  + &
		PR_TEMP::ACCOUNT + " " + &
		PR_TEMP::EMP_NUM + " " + &
		FORMAT$(PR_TEMP::REGULAR, "###,###.##") + " " + &
		FORMAT$(PR_TEMP::EXCESS, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	REG_TOTAL = REG_TOTAL + PR_TEMP::REGULAR
	EXCESS_TOTAL = EXCESS_TOTAL + PR_TEMP::EXCESS
	JOB_EXCESS_TOT = JOB_EXCESS_TOT + EXCESS_TOTAL
	JOB_REG_TOT     = JOB_REG_TOT + REG_TOTAL

17220	GOTO 17210

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB AccountTotal
	GOSUB JobTotal

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
	! Subtotal for one account
	!*******************************************************************
 AccountTotal:
	TEXT$ = SPACE$(16%)	+ &
		"Account  "	+ &
		TEMP.ACC$ + &
		FORMAT$(REG_TOTAL, "###,###.##")   + " " + &
		FORMAT$(EXCESS_TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	REG_TOTAL = 0.0
	EXCESS_TOTAL = 0.0

	RETURN

	!*******************************************************************
	! Print total for one job
	!*******************************************************************
 JobTotal:
	TEXT$ = PR_TEMP::JOB + SPACE$(21%) + &
		"Job TOTAL   " + &
		FORMAT$(JOB_REG_TOT, "###,###.##") + " " + &
		FORMAT$(JOB_EXCESS_TOT, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	JOB_REG_TOT = 0.0
	JOB_EXCESS_TOT = 0.0

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

