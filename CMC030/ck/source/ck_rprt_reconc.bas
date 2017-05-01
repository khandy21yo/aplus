1	%TITLE "Check Reconciliation File Report"
	%SBTTL "CK_RPRT_RECONC"
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
	! ID:CK002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Check Reconciliation\* report will provide the
	!	following information:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Account
	!	.le
	!	Check _#
	!	.le
	!	Cancelled
	!	.le
	!	Outstanding
	!	.le
	!	Voided
	!	.le
	!	Error
	!	.le
	!	Check Amount
	!	.le
	!	Bank Amount
	!	.le
	!	Check Date
	!	.els
	!
	! Index:
	!	.x Report>Check Reconciliation
	!	.x Check Reconciliation>Report
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Author:
	!
	!	04/20/88 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_RPRT_RECONC/LINE
	!	$ LINK/EXECUTABLE=CK_EXE:*.EXE CK_RPRT_RECONC, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_RPRT_RECONC.OBJ;*
	!
	! Modification history:
	!
	!	05/24/91 - Kevin Handy
	!		Modified to close GL_PERIOD as soon as possible.
	!
	!	10/14/91 - Kevin Handy
	!		Added a REGARDLESS to the GET on GL_PERIOD.
	!
	!	11/06/91 - Kevin Handy
	!		Added "CUTOFF" period.
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Lose extra parameter to ASSG_FREECHANNEL.
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	05/01/97 - Kevin Handy
	!		Clean up source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.HB"
	MAP	(CK_CKMNT)	CK_CKMNT_CDD	CK_CKMNT

	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.HB"
	MAP (CK_CONTROL)	CK_CONTROL_CDD		CK_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	!
	! Define variables
	!
	RECORD TOTAL_GROUP_CDD
		INTEGER TOTAL_COUNT
		REAL CHKAMT
		REAL BNKAMT
	END RECORD

	DIM TOTAL_GROUP_CDD TOTAL_GROUP(4%)

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	ONLY_BANK$ = TRM$(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!	^*(01) Bank Code\*
	!	.b
	!	.lm +5
	!	The ^*Bank Code\* field enters the bank
	!	account code (set up in the Controlling File) which is to
	!	be printed in this report.
	!	.lm -5
	!
	! Index:
	!	.x Bank Code>Check Reconciliation Report
	!	.x Check Reconciliation Report>Bank Code
	!
	!--

	ONLY_STATUS$ = TRM$(UTL_REPORTX::OPTDEF(1%))
	ONLY_STATUS$ = "" IF INSTR(1%, "COVE", ONLY_STATUS$) = 0%

	!++
	! Abstract:FLD02
	!	^*(02) Status (C/O/V/E/)\*
	!	.b
	!	.lm +5
	!	The ^*Status\* field enters a code which
	!	will indicate what items are to print on the report.
	!	.b
	!	Valid entries are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	^*C\* = Cancelled
	!	.le
	!	^*O\* = Outstanding
	!	.le
	!	^*V\* = Void
	!	.le
	!	^*E\* = Error
	!	.le
	!	^*_*\* = All Items
	!	.lm -5
	!	.els
	!	A blank setting will also cause all items in the file to be
	!	printed.
	!
	! Index:
	!	.x Status>Check Reconciliation Report
	!	.x Check Reconciliation Report>Status
	!
	!--

	ONLY_TYPE$ = TRM$(UTL_REPORTX::OPTDEF(2%))
	ONLY_TYPE$ = "" IF INSTR(1%, "ACD", ONLY_TYPE$) = 0%

	!++
	! Abstract:FLD03
	!	^*(03) Type (C/D/A)\*
	!	.b
	!	.lm +5
	!	The ^*Type\* field enters a code which
	!	will indicate what items will print.
	!	.b
	!	Valid entries are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	^*C\* = Checks
	!	.le
	!	^*D\* = Deposits
	!	.le
	!	^*A\* = Adjustments
	!	.els
	!	.lm -5
	!	A blank setting will cause all items in the file to be
	!	printed.
	!
	! Index:
	!	.x Type>Check Reconciliation Report
	!	.x Check Reconciliation Report>Type
	!
	!--

	CUTOFF$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Cutoff Period\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Period\* field enters
	!	a cutoff period so that a clean report may be printed even
	!	though future periods have been read.
	!	.b
	!	A blank setting will cause all items in the file to be
	!	printed.
	!	.lm -5
	!
	! Index:
	!	.x Cutoff>Check Reconciliation Report
	!	.x Check Reconciliation Report>Cutoff
	!
	!--

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.OPN"
	USE
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

310	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

320	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.OPN"
		GET #CK_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "CK_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF CK_CONTROL::FLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "CK Purge in process", "ERR", "CK_PURGE", &
			"ERROR_PURGE")
		GOTO ExitProgram
	END IF

	CUR_PERIOD% = CK_CONTROL::PERIOD + 1%
	YEAR$ = CK_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Check Reconciliation Report"
	TITLE% = 2%

	IF ONLY_BANK$ <> ""
	THEN
		TITLE$(TITLE%) = "For Bank Code " + ONLY_BANK$
		TITLE% = TITLE% + 1%
	END IF

	TITLE$(TITLE%) = "Cutoff Period " + &
			YYYY_PP$ + " " + GL_PERIOD::PERIOD(CUR_PERIOD%)
	TITLE% = TITLE% + 1%

	IF ONLY_STATUS$ <> ""
	THEN
		TITLE$(TITLE%) = "For Status " + ONLY_STATUS$
		TITLE% = TITLE% + 1%
	END IF

	IF ONLY_TYPE$ <> ""
	THEN
		TITLE$(TITLE%) = "For Type " + ONLY_TYPE$
		TITLE% = TITLE% + 1%
	END IF

	TITLE$(TITLE%) = ""


	TITLEA$ = &
		"Account      Adj #   Can  Out  Voi  Err   Check Amount  " + &
		"   Bank Amount Date"

	TITLEC$ = &
		"Account      Chck #  Can  Out  Voi  Err   Check Amount  " + &
		"   Bank Amount Date"

	TITLED$ = &
		"Account      Dep #   Can  Out  Voi  Err   Check Amount  " + &
		"   Bank Amount Date"

	TITLE$(TITLE% + 1%) = &
		"                     ----- Status -----" + &
		SPACE$(31%) + "Check"
	TITLE$(TITLE% + 2%) = TITLEA$
	TITLE$(TITLE% + 3%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #CK_CKMNT.CH%
	USE
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	!
	! Initilize for first record
	!
	LAST_BANK$ = "1234567890"
	BANK_ACCT$ = "1234567890"
	CKNUM$ = "1234567890"
	ETYPE$ = "Z"
	BNKAMT = 0.0
	CHKAMT = 0.0
	CKDAT$ = ""

	!
	! Zero totals
	!
	FOR I% = 1% TO 4%
		TOTAL_GROUP(I%)::TOTAL_COUNT = 0%
		TOTAL_GROUP(I%)::CHKAMT = 0.0
		TOTAL_GROUP(I%)::BNKAMT = 0.0
	NEXT I%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #CK_CKMNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	!
	! Skip if past cutoff
	!
	IF (CUTOFF$ <> "")
	THEN
		GOTO 17020 IF (CK_CKMNT::GLDATE > CUTOFF$)
	END IF

	!
	! Check current record
	!
	GOTO 17020 IF (ONLY_BANK$ <> "") AND (CK_CKMNT::BANK_ACCT <> ONLY_BANK$)
	GOTO 17020 IF (ONLY_TYPE$ <> "") AND (CK_CKMNT::ETYPE <> ONLY_TYPE$)

	!
	! Print out one line if necessary
	!
	GOSUB PrintLine &
		IF (BANK_ACCT$ <> CK_CKMNT::BANK_ACCT) OR &
		(CKNUM$ <> CK_CKMNT::CKNUM) OR &
		(ETYPE$ <> CK_CKMNT::ETYPE)

	!
	! Summarize this item
	!
	IF CK_CKMNT::STYPE = "G"
	THEN
		CHKAMT = FUNC_ROUND(CHKAMT + CK_CKMNT::CKAMT, 2%)
	ELSE
		BNKAMT = FUNC_ROUND(BNKAMT + CK_CKMNT::CKAMT, 2%)
	END IF

	GOTO GetNextRec

	%PAGE

	!*******************************************************************
	! Print out one line of information, if requested
	!*******************************************************************
 PrintLine:

	!
	! Don't print if it is garbage used to prime the printer
	!
	GOTO SkipPrint IF BANK_ACCT$ = "1234567890"

	CHKAMT = FUNC_ROUND(CHKAMT, 2%)
	BNKAMT = FUNC_ROUND(BNKAMT, 2%)

	!
	! Figure out which flags need to be set
	!
	IF (CHKAMT = BNKAMT) AND (CHKAMT <> 0.0)	! Cancelled?
	THEN
		FL_C$ = "X"
	ELSE
		FL_C$ = " "
	END IF

	IF (CHKAMT <> 0.0) AND (BNKAMT = 0.0)		! Outstanding
	THEN
		FL_O$ = "X"
	ELSE
		FL_O$ = " "
	END IF

	IF (CHKAMT = 0.0) AND (BNKAMT = 0.0)		! Void
	THEN
		FL_V$ = "X"
	ELSE
		FL_V$ = " "
	END IF

	IF (CHKAMT <> BNKAMT) AND (BNKAMT <> 0.0)	! Error
	THEN
		FL_E$ = "X"
	ELSE
		FL_E$ = " "
	END IF

	!
	! If the user specified a status, then check that status
	!
	IF ONLY_STATUS$ <> ""
	THEN
		SELECT ONLY_STATUS$

		CASE "C"
			GOTO SkipPrint IF FL_C$ = " "

		CASE "O"
			GOTO SkipPrint IF FL_O$ = " "

		CASE "V"
			GOTO SkipPrint IF FL_V$ = " "

		CASE "E"
			GOTO SkipPrint IF FL_E$ = " "
		END SELECT
	END IF

	!
	! Go to a new page if necessary
	!
	IF (ETYPE$ <> LAST_ETYPE$) OR (BANK_ACCT$ <> LAST_BANK$)
	THEN
		GOSUB PrintTotals IF LAST_BANK$ <> "1234567890"

		!
		! Change the title for the following pages
		!
		SELECT ETYPE$

		CASE "A"
			TITLE$(TITLE% + 2%) = TITLEA$

		CASE "C"
			TITLE$(TITLE% + 2%) = TITLEC$

		CASE "D"
			TITLE$(TITLE% + 2%) = TITLED$
		END SELECT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 30000%)

		LAST_ETYPE$ = ETYPE$
		LAST_BANK$ = BANK_ACCT$

		!
		! Zero the totals
		!
		FOR I% = 1% TO 4%
			TOTAL_GROUP(I%)::TOTAL_COUNT = 0%
			TOTAL_GROUP(I%)::CHKAMT = 0.0
			TOTAL_GROUP(I%)::BNKAMT = 0.0
		NEXT I%

	END IF

	!
	! Handle totals
	!
	IF FL_C$ = "X"
	THEN
		TOTAL_GROUP(1%)::CHKAMT = TOTAL_GROUP(1%)::CHKAMT + CHKAMT
		TOTAL_GROUP(1%)::BNKAMT = TOTAL_GROUP(1%)::BNKAMT + BNKAMT
		TOTAL_GROUP(1%)::TOTAL_COUNT = TOTAL_GROUP(1%)::TOTAL_COUNT + 1%
	END IF

	IF FL_O$ = "X"
	THEN
		TOTAL_GROUP(2%)::CHKAMT = TOTAL_GROUP(2%)::CHKAMT + CHKAMT
		TOTAL_GROUP(2%)::BNKAMT = TOTAL_GROUP(2%)::BNKAMT + BNKAMT
		TOTAL_GROUP(2%)::TOTAL_COUNT = TOTAL_GROUP(2%)::TOTAL_COUNT + 1%
	END IF

	IF FL_V$ = "X"
	THEN
		TOTAL_GROUP(3%)::CHKAMT = TOTAL_GROUP(3%)::CHKAMT + CHKAMT
		TOTAL_GROUP(3%)::BNKAMT = TOTAL_GROUP(3%)::BNKAMT + BNKAMT
		TOTAL_GROUP(3%)::TOTAL_COUNT = TOTAL_GROUP(3%)::TOTAL_COUNT + 1%
	END IF

	IF FL_E$ = "X"
	THEN
		TOTAL_GROUP(4%)::CHKAMT = TOTAL_GROUP(4%)::CHKAMT + CHKAMT
		TOTAL_GROUP(4%)::BNKAMT = TOTAL_GROUP(4%)::BNKAMT + BNKAMT
		TOTAL_GROUP(4%)::TOTAL_COUNT = TOTAL_GROUP(4%)::TOTAL_COUNT + 1%
	END IF

	!
	! Print out this line
	!
	TEXT$ = BANK_ACCT$ + "    " + &
		CKNUM$ + "      " + &
		FL_C$ + "    " + &
		FL_O$ + "    " + &
		FL_V$ + "    " + &
		FL_E$ + "  " + &
		FORMAT$(CHKAMT, "###,###,###.##  ") + &
		FORMAT$(BNKAMT, "###,###,###.##  ") + &
		PRNT_DATE(CKDAT$, 6%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Set flags up
	!
 SkipPrint:
	BANK_ACCT$ = CK_CKMNT::BANK_ACCT + ""
	CKNUM$ = CK_CKMNT::CKNUM + ""
	ETYPE$ = CK_CKMNT::ETYPE + ""
	BNKAMT = 0.0
	CHKAMT = 0.0
	CKDAT$ = CK_CKMNT::CKDAT + ""

	RETURN

	%PAGE

 PrintTotals:
	!*******************************************************************
	! Print totals
	!*******************************************************************

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	FOR I% = 1% TO 4%
		!
		! Pick the right flag to print
		!
		FL1_C$ = " "
		FL1_O$ = " "
		FL1_V$ = " "
		FL1_E$ = " "

		SELECT I%
		CASE 1%
			FL1_C$ = "X"
		CASE 2%
			FL1_O$ = "X"
		CASE 3%
			FL1_V$ = "X"
		CASE 4%
			FL1_E$ = "X"
		END SELECT

		!
		! Print one total
		!
		TEXT$ = "Total     " + &
			FORMAT$(TOTAL_GROUP(I%)::TOTAL_COUNT, &
				"######      ") + &
			FL1_C$ + "    " + &
			FL1_O$ + "    " + &
			FL1_V$ + "    " + &
			FL1_E$ + "  " + &
			FORMAT$(TOTAL_GROUP(I%)::CHKAMT, &
				"###,###,###.##  ") + &
			FORMAT$(TOTAL_GROUP(I%)::BNKAMT, &
				"###,###,###.##  ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT I%

	RETURN

	%PAGE

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB PrintLine
	GOSUB PrintTotals

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

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
