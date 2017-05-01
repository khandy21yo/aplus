1	%TITLE "Accounts Payable General Ledger to Accounts Payable Comparison"
	%SBTTL "AP_RPRT_37AUDT02"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2001 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:APAUD2
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Accounts Payable to General Ledger Comparison\* option provides
	!	the means to print a report which compares all transactions in the Accounts
	!	Payable Open file for a specified accounting period with all
	!	accounts payable transactions in the General Ledger file for the
	!	same period. The report compares the transactions in reference
	!	to each vendor in both files and displays any differences between
	!	the two files.
	!	.NOTE
	!	If you have Accounts Payable on more than one user account, but
	!	all update to the same General Ledger,
	!	the audit reports need to be run together in
	!	order to get useful reports.
	!	This is done by pointing to the other user(s) accounts in the
	!	device file (UT MAST TABLE DEVICE) by adding record(s) under
	!	the name "AP__OPEN__01" on up to "AP__OPEN__99".
	!	(Do not skip any numbers. Start with 01, 02, 03...)
	!	.END NOTE
	!
	! Index:
	!	.x Accounts Payable>Reports>AP to GL Audit
	!	.x Reports>AP to GL Audit Comparison
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_37AUDT02/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_37AUDT02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_37AUDT02.OBJ;*
	!
	! Author:
	!
	!	03/15/2001 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP	(AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_37CLOSE.HB"
	MAP (AP_37CLOSE) AP_37CLOSE_CDD AP_37CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL_ACCOUNT.HB"
	MAP	(AP_CONTROL_ACCOUNT)	AP_CONTROL_ACCOUNT_CDD	AP_CONTROL_ACCOUNT

	!
	! MAPs
	!
	MAP (AP_TEMP) &
		STRING	AP_TEMP.VENNUM = 10%, &
		STRING	AP_TEMP.SOURCE = 1%, &
		REAL	AP_TEMP.AMOUNT

	MAP (DP_OUTP_XUNSOL) &
		LONG	RRR_FLAG

	DECLARE INTEGER CONSTANT MAX_RANGE = 120%
	DIM AP_37CLOSE_FILE$(MAX_RANGE)

	!
	! External functions
	!
	EXTERNAL LONG		OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Assign channel number(s)
	!
	CALL ASSG_CHANNEL(AP_TEMP.CH%, STAT%)

	!
	! Look up device for the Temporary file
	!
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEST_YYYY_PP$, PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) For Period\*
	!	.b
	!	.lm +5
	!	The ^*For Period\* field allows specifying the General
	!	Ledger period for which the comparison should take place.
	!	If you leave this field blank, the report will be printed
	!	for the currently open period, as defined in the AP control
	!	file.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x AP to GL Comparison>For Period
	!	.x For Period>AP to GL Comparison
	!	.x AP to GL Comparison>Period
	!	.x Period>AP to GL Comparison
	!
	!--

	FROM_VENDOR$ = TRM$(UTL_REPORTX::OPTDEF(1%))

	!++
	! Abstract:FLD02
	!	^*(02) From Vendor\*
	!	.b
	!	.lm +5
	!	This field specifies a starting vendor number for the
	!	report, so that it is faster to check out problems later
	!	on.
	!	.lm -5
	!
	! Index:
	!	.x AP to GL Comparison>From Vendor
	!	.x From Vendor>AP to GL Comparison
	!
	!--

	TO_VENDOR$ = TRM$(UTL_REPORTX::OPTDEF(2%))

	!++
	! Abstract:FLD03
	!	^*(03) To Vendor\*
	!	.b
	!	.lm +5
	!	This field specifies an ending vendor number for the
	!	report, so that it is faster to check out problems later
	!	on.
	!	.lm -5
	!
	! Index:
	!	.x AP to GL Comparison>From Vendor
	!	.x From Vendor>AP to GL Comparison
	!
	!--

	CUR_PERIOD% = VAL%(RIGHT(PERIOD$, 5%))
	YEAR$ = LEFT(PERIOD$, 4%)
	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

	FIRST_RECORD% = 0%
	TEST_VENNUM$ = ""

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"

		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF AP_CONTROL::AP_ACCT = ""
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AP account not defined", &
			"ERR", "AP_ACCT", "ERROR_NOAPACCT")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram
	END IF

	SELECT AP_CONTROL::CLOSEFLAG

	CASE "1"
		CALL HELP_3MESSAGE(SCOPE, "AP close in process", &
			"ERR", "AP_CLOSE", "ERROR_CLOSE")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	CASE "2"
		CALL HELP_3MESSAGE(SCOPE, "AP Reset in process", &
			"ERR", "AP_RESET", "ERROR_RESET")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	CASE ELSE
		IF PERIOD$ = ""
		THEN
			CUR_PERIOD% = AP_CONTROL::LASTPERCLOSE + 1%
			YEAR$ = AP_CONTROL::YEAR

			IF CUR_PERIOD% > GL_PERIOD::FPFY
			THEN
				CUR_PERIOD% = 1%
				YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
			END IF

			TEST_YYYY_PP$, YYYY_PP$ = YEAR$ + &
				FORMAT$(CUR_PERIOD%, "<0>#")

			YYYY_PP$ = LEFT(YYYY_PP$,4%) + "_" + RIGHT(YYYY_PP$, 5%)
		END IF

	END SELECT

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

350	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "AP_TEMP.TMP" FOR OUTPUT AS FILE AP_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AP_TEMP, &
			PRIMARY KEY AP_TEMP.VENNUM DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, &
			ALLOW NONE
	USE
		FILENAME$ = "AP_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Open the AR Account Control file, and load the accounts into a string
	!
390	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL_ACCOUNT.OPN"
	USE
		CONTINUE 420
	END WHEN

400	WHEN ERROR IN
		GET #AP_CONTROL_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE 420
	END WHEN

	ACCT_LIST$ = ACCT_LIST$ + AP_CONTROL_ACCOUNT::ACCOUNT + ","
	GOTO 400

420	CLOSE AP_CONTROL_ACCOUNT.CH%

	%PAGE

1000	!******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file.  Reading Open file", 1%)

	GOSUB LoadApOpen
	CLOSE AP_OPEN.CH%
	CALL ASSG_FREECHANNEL(AP_OPEN.CH%)

	CALL READ_DEVICE("AP_37CLOSE", AP_37CLOSE.DEV$, STAT%)
	GOSUB LoadApClose

	FILE_LOOP% = 1%

1010	CALL READ_DEVICE("AP_OPEN_" + FORMAT$(FILE_LOOP%, "<0>#"), &
		AP_OPEN.DEV$, STAT%)
	AP_37CLOSE.DEV$ = AP_OPEN.DEV$

	GOTO 1200 IF STAT%

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file.  Reading Open file #" + &
		FORMAT$(FILE_LOOP%, "<0>#"), 1%)

1012	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_01.OPN"
	USE
		FILENAME$ = "AP_OPEN_01"
		CONTINUE HelpError
	END WHEN

	GOSUB LoadApOpen
	CLOSE AP_OPEN.CH%
	CALL ASSG_FREECHANNEL(AP_OPEN.CH%)

1014 !	WHEN ERROR IN
 !		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE_01.OPN"
 !	USE
 !		FILENAME$ = "AP_CLOSE_01"
 !		CONTINUE HelpError
 !	END WHEN

	GOSUB LoadApClose

	FILE_LOOP% = FILE_LOOP% + 1%

	GOTO 1010

	%PAGE

1200	!*******************************************************************
	! Build GL summary
	!*******************************************************************

	WHEN ERROR IN
		RESET #GL_YYYY_PP.CH%, KEY #0%
	USE
		CONTINUE ReportTitle
	ENd WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file.  Reading GL file", 1%)

1300	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	SELECT RRR_FLAG

	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
 !		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

 !		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
 !			LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG = 0%

	IF FROM_VENDOR$ <> ""
	THEN
		GOTO 1300 IF GL_YYYY_PP::XREFNO < FROM_VENDOR$
	END IF

	IF TO_VENDOR$ <> ""
	THEN
		GOTO 1300 IF GL_YYYY_PP::XREFNO > TO_VENDOR$
	END IF

	!
	! Test account against both the ap account and any accounts
	! defined in the control file
	!
	GOTO 1300 &
		IF (INSTR(1%, ACCT_LIST$, GL_YYYY_PP::ACCT) = 0%) AND &
		(COMP_STRING(GL_YYYY_PP::ACCT, AP_CONTROL::AP_ACCT) = 0%)

	AP_TEMP.AMOUNT = -GL_YYYY_PP::AMOUNT
	AP_TEMP.VENNUM = GL_YYYY_PP::XREFNO
	AP_TEMP.SOURCE = "G"

	PUT #AP_TEMP.CH%

	GOTO 1300

	%PAGE

1400	!*******************************************************************
	! Initialize the report title
	!*******************************************************************

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	TITLE$(1%) = "AP/GL Comparison Report"
	TITLE$(2%) = "Accounting Period Ended " + &
		MID(YYYY_PP$, 6%, 2%) + " " + LEFT(YYYY_PP$, 4%)
	TITLE$(3%) = ""

	TITLE$(4%) = "VendorNum  VendorName                      " + &
		"AP Amount     GL Amount       Dif +/-"
	TITLE$(5%) = ""

	LYT_LINE$ = "$VendorNum:010,$VendorName:036,VAPTotal:052," + &
		"VGLTotal:066,VAP/GLDifference:080"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		RESET #AP_TEMP.CH%, KEY #0%
	USE
		FILENAME$ = "AP_TEMP"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! If this Vendor Number different from the last one, and this is
	! also NOT the first record, then print out a total for this Vendor.
	!
	IF (AP_TEMP.VENNUM <> TEST_VENNUM$) AND (FIRST_RECORD%)
	THEN
		GOSUB VendorTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT

		AP_TOTAL, GL_TOTAL = 0.0
	END IF

	!
	! Add to the totals
	!
	AP_TOTAL = AP_TOTAL + AP_TEMP.AMOUNT IF AP_TEMP.SOURCE = "A"
	GL_TOTAL = GL_TOTAL + AP_TEMP.AMOUNT IF AP_TEMP.SOURCE = "G"

	!
	! Reset values of some variables
	!
	FIRST_RECORD% = -1%
	TEST_VENNUM$ = AP_TEMP.VENNUM

	!
	! Try for next record
	!
	GOTO 17100

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Handle end of report
	!
	IF (FIRST_RECORD%)
	THEN
		GOSUB VendorTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	TEXT$ = "     Grand Total" + SPACE$(22%) + &
		FORMAT$(AP_GRAND_TOTAL, " ##,###,###.##") + &
		FORMAT$(GL_GRAND_TOTAL, " ##,###,###.##") + &
		FORMAT$(AP_GRAND_TOTAL - GL_GRAND_TOTAL, " ##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	!
	! Finish up report
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close channel
	!
	CLOSE AP_TEMP.CH%

17910	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 VendorTotal:
	!******************************************************************
	! Subroutine to print out totals for a Vendor number
	!******************************************************************
	AP_VENDOR::VENNAM = STRING$(LEN(AP_VENDOR::VENNAM), 63%)

18000	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ TEST_VENNUM$, REGARDLESS
	USE
		CONTINUE 18010 IF ERR = 155%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

18010	TEXT$ = TEST_VENNUM$ + " " + &
		LEFT(AP_VENDOR::VENNAM, 25%) + "  " + &
		FORMAT$(AP_TOTAL, " ##,###,###.##") + &
		FORMAT$(GL_TOTAL, " ##,###,###.##") + &
		FORMAT$(AP_TOTAL - GL_TOTAL, " ##,###,###.##")

	AP_GRAND_TOTAL = AP_GRAND_TOTAL + AP_TOTAL
	GL_GRAND_TOTAL = GL_GRAND_TOTAL + GL_TOTAL

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	RETURN

	%PAGE

18100	!*******************************************************************
	! Load in AP Open file
	!*******************************************************************

 LoadApOpen:
	WHEN ERROR IN
		IF FROM_VENDOR$ = ""
		THEN
			RESET #AP_OPEN.CH%, KEY #0%
		ELSE
			FIND #AP_OPEN.CH%, KEY #0% GE FROM_VENDOR$, REGARDLESS
		END IF
	USE
		CONTINUE 18190
	END WHEN

	!
	! Set up to trap interrupt
	!
 !	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
 !		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG = 0%

	!
	! File-loading loop begins here
	!
18110	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 18190
	END WHEN

	!
	! Handle any special junk in RRR_FLAG
	!
	SELECT RRR_FLAG

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
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
 !		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

 !		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
 !			LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG = 0%

	GOTO 18190 IF AP_OPEN::VENNUM > TO_VENDOR$ AND TO_VENDOR$ <> ""

	IF LEFT(AP_OPEN::UPDATED, 6%) = TEST_YYYY_PP$
	THEN
		AP_TEMP.AMOUNT = (AP_OPEN::INVAMT - AP_OPEN::DISAMT) - &
			AP_OPEN::CKAMT
		AP_TEMP.VENNUM = AP_OPEN::VENNUM
		AP_TEMP.SOURCE = "A"

		PUT #AP_TEMP.CH%
	END IF

	!
	! Get the next record to put into the file
	!
	GOTO 18110

18190	RETURN

	%PAGE

18200	!*******************************************************************
	! Load in AP Closed file
	!*******************************************************************

 LoadApClose:

	CALL FIND_FILE(AP_37CLOSE.DEV$ + "AP_37CLOSE_*.LED", &
		AP_37CLOSE_FILE$(), 16%, "", "")

	AP_37CLOSE_FILE% = VAL%(AP_37CLOSE_FILE$(0%))

	IF AP_37CLOSE_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"General ledger files do not exist", 0%)
		GOTO ExitProgram
	ELSE
		AP_37CLOSE_FILE$(LOOP%) = &
			MID(AP_37CLOSE_FILE$(LOOP%), 12%, 6%) &
			FOR LOOP% = 1% TO AP_37CLOSE_FILE%
	END IF

	FOR AP_37CLOSE_LOOP% = AP_37CLOSE_FILE% TO 1% STEP -1%

		YYYY_PP$ = AP_37CLOSE_FILE$(AP_37CLOSE_LOOP%)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_37CLOSE.OPN"
		USE
			CONTINUE 18290 IF ERR = 5%
			FILENAME$ = "AP_37CLOSE"
			CONTINUE HelpError
		END WHEN

		WHEN ERROR IN
			IF FROM_VENDOR$ = ""
			THEN
				RESET #AP_37CLOSE.CH%, KEY #0%
			ELSE
				FIND #AP_37CLOSE.CH%, &
					KEY #0% GE FROM_VENDOR$, &
					REGARDLESS
			END IF
		USE
			CONTINUE 18290
		END WHEN

		!
		! Set up to trap interrupt
		!
 !		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
 !			LOC(OUTP_XUNSOL) BY VALUE, &
 !			LOC(SCOPE::SMG_KBID) BY VALUE)

		RRR_FLAG = 0%

		!
		! File-loading loop begins here
		!
18210		WHEN ERROR IN
			GET #AP_37CLOSE.CH%, REGARDLESS
		USE
			CONTINUE 18290
		END WHEN

		!
		! Handle any special junk in RRR_FLAG
		!
		SELECT RRR_FLAG

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
 !			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
			CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
				SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
 !			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
 !				LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

		!
		! Interupt
		!
		CASE SMG$K_TRM_F6, SMG$K_TRM_F20
 !			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			CALL MENU_3INTERRUPT(SCOPE)

 !			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
 !				LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

		END SELECT

		RRR_FLAG = 0%

		GOTO 18190 IF AP_37CLOSE::VENNUM > TO_VENDOR$ AND &
			TO_VENDOR$ <> ""

		IF LEFT(AP_37CLOSE::UPDATED, 6%) = TEST_YYYY_PP$
		THEN
			AP_TEMP.AMOUNT = (AP_37CLOSE::INVAMT - AP_37CLOSE::DISAMT) - &
				AP_37CLOSE::CKAMT
			AP_TEMP.VENNUM = AP_37CLOSE::VENNUM
			AP_TEMP.SOURCE = "A"

			PUT #AP_TEMP.CH%
		END IF

		!
		! Get the next record to put into the file
		!
		GOTO 18210

18290		CLOSE #AP_37CLOSE.CH%
		CALL ASSG_FREECHANNEL(AP_37CLOSE.CH%)
		AP_37CLOSE.CH% = 0%

	NEXT AP_37CLOSE_LOOP%

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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report AP_RPRT_37AUDT02
	!******************************************************************
	END
