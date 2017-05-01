1	%TITLE "Purge Cancelled Checks at End of Period"
	%SBTTL "CK_SPEC_PURGE"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! ID:CKPURG
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Purge Cancelled Checks\* option "closes
	!	out" a particular accounting period.
	!	.b
	!	Each time a Purge is completed the "Period" field in the Control
	!	file will increment to the next accounting period.
	!	.lm -5
	!
	! Index:
	!	.x Purge>Cancelled Checks
	!	.x Cancelled Checks>Purge
	!
	! Option:
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_SPEC_PURGE
	!	$ LINK/EXEC=CK_EXE:*.EXE CK_SPEC_PURGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_SPEC_PURGE.OBJ;*
	!
	! Author:
	!
	!	04/19/88 - Kevin Handy
	!
	! Modification history:
	!
	!	09/24/90 - Kevin Handy
	!		Changed "closeded" to "closed".
	!
	!	05/23/91 - Kevin Handy
	!		Close GL_PERIOD file as soon as possible.
	!
	!	06/03/91 - Frank F. Starman
	!		Correct totals for summary of checks purged. Was
	!		double.
	!
	!	10/15/91 - Kevin Handy
	!		Look for bug in purge where it purged deposits
	!		that shouldn't have been purged.  Didn't find any
	!		possible method.  Noticed Franks change (06/03/91)
	!		isn't quite right, but no time to fix.
	!
	!	11/14/91 - Kevin Handy
	!		Added cutoff period for reconcilation.
	!		Problem with bank/gl only purging the GL side
	!		appears to come from the READ.  If they re-read,
	!		and get an undefined check #, then abort out,
	!		it doesn't read everything back in again.
	!
	!	10/12/92 - Kevin Handy
	!		Modified to look at ETYPE during purge.
	!
	!	03/24/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	04/30/97 - Kevin Handy
	!		Use OUTP_INITFORM function.
	!
	!	08/27/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/12/98 - Kevin Handy
	!		Use 'FILENAME$' instead of 'SCOPE::PRG_PROGRAM'
	!		in error trapping.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Includes
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Maps
	!
	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.HB"
	MAP (CK_CONTROL)	CK_CONTROL_CDD		CK_CONTROL

	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.HB"
	MAP (CK_CONTROLACC)	CK_CONTROLACC_CDD	CK_CONTROLACC

	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.HB"
	MAP (CK_CKMNT)		CK_CKMNT_CDD		CK_CKMNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	!
	! Functions
	!
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	!
	! Dimension statements
	!
	RECORD GTOTAL_CDD
		STRING	BANK_ACCT = 6%
		REAL	DEPOSIT
		REAL	CHECK
		REAL	ADJUST
	END RECORD

	DIM GTOTAL_CDD KEPT_TOTAL(200%), PURGED_TOTAL(200%)

	%PAGE

	!*******************************************************************
	! Initilization
	!*******************************************************************

	ON ERROR GOTO 19000

	CALL READ_INITIALIZE

	CALL ASSG_CHANNEL(CK_CKMNT_NEW.CH%, STAT%)

	REPORT$ = "CKPURG"

505	!
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
	GL_PERIOD.CH% = 0%

510	!
	! Open control file.
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.MOD"
	USE
		FILENAME$ = "CK_CONTROL"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		GET #CK_CONTROL.CH%, RECORD 1%
	USE
		!
		! Handle record locks by sleeping for a while, then
		! trying again.
		!
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "CK_CONTROL"
		CONTINUE HelpError
	END WHEN

	CUR_PERIOD% = CK_CONTROL::PERIOD + 1%
	YEAR$ = CK_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

520	!
	! Open control account file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.OPN"
	USE
		FILENAME$ = "CK_CONTROLACC"
		CONTINUE HelpError
	END WHEN

530	!
	! Open reconciliation file (Create it if necessary)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.CRE"
	USE
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	%PAGE

700	!******************************************************************
	! Ask for the Period to be updated.
	!******************************************************************
	!
	! Paint the background
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_SCREEN_DATA%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SCREEN_DATA%, &
		"Purge Closed Items " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Purging for Period " + &
		YYYY_PP$ + " " + GL_PERIOD::PERIOD(CUR_PERIOD%), 4%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

	SCOPE::PRG_ITEM = "CONFIRM"

	!++
	! Abstract:CONFIRM
	!	^*Confirm\*
	!	.b
	!	.lm +5
	!	The ^*Confirm\* field allows for user confirmation to the accounting
	!	period which will be purged.
	!	.lm -5
	!
	! Index:
	!	.x Confirm>Purge
	!	.x Purge>Confirm
	!
	!--

	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", "Confirm Purging period " + &
		YYYY_PP$ + " - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	%PAGE

1000	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL


	%PAGE

2010	!
	! Open the new file to copy everything current over to.
	!
	CALL WRIT_CURPROTECTION(CK_CKMNT.PRO$, ST%)

	WHEN ERROR IN
		OPEN CK_CKMNT.DEV$ + "CK_CKMNT.NEW" FOR OUTPUT AS FILE CK_CKMNT_NEW.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP CK_CKMNT, &
			PRIMARY KEY &
			( &
				CK_CKMNT::BANK_ACCT, &
				CK_CKMNT::CKNUM, &
				CK_CKMNT::ETYPE &
			)	DUPLICATES, &
			ACCESS MODIFY, ALLOW MODIFY
	USE
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	CALL WRIT_CURPROTECTION(OLD_PROT$, ST%)


5000	!*******************************************************************
	! Purge out any information which already exists for this
	! period in the check reconciliation file.
	!*******************************************************************

	!
	! Set purge flag (Also unlocks the record)
	!
	CK_CONTROL::FLAG = "1"

	UPDATE #CK_CONTROL.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Purge in process. . .", 1% + 16%)

5005	RESET #CK_CKMNT.CH%

	!
	! Impossible check/bank numbers
	!
	THIS_BANK$ = "0123456789"
	THIS_CHECK$ = "0123456789"
	THIS_ETYPE$ = "ZZ"

	BNKAMT = 0.0
	CHKAMT = 0.0

	DEPAMT = 0.0
	CHEAMT = 0.0
	ADJAMT = 0.0

	TESTPERIOD$ = ""

5010	!
	! Pull up one record
	!
	WHEN ERROR IN
		GET #CK_CKMNT.CH%
	USE
		!
		! Handle record locks by sleeping for a while, then
		! trying again.
		!
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 7000 IF ERR = 11%
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	GOSUB TestCheck &
		IF (THIS_BANK$ <> CK_CKMNT::BANK_ACCT) OR &
			(THIS_CHECK$ <> CK_CKMNT::CKNUM) OR &
			(THIS_ETYPE$ <> CK_CKMNT::ETYPE)

	IF (CK_CKMNT::STYPE = "B")
	THEN
		BNKAMT = FUNC_ROUND(BNKAMT + CK_CKMNT::CKAMT, 2%)
	ELSE
		CHKAMT = FUNC_ROUND(CHKAMT + CK_CKMNT::CKAMT, 2%)
	END IF

	SELECT CK_CKMNT::ETYPE

	CASE "D"
		DEPAMT = FUNC_ROUND(DEPAMT + CK_CKMNT::CKAMT, 2%)

	CASE "C"
		CHEAMT = FUNC_ROUND(CHEAMT + CK_CKMNT::CKAMT, 2%)

	CASE ELSE
		ADJAMT = FUNC_ROUND(ADJAMT + CK_CKMNT::CKAMT, 2%)

	END SELECT

	TESTPERIOD$ = CK_CKMNT::GLDATE IF CK_CKMNT::GLDATE > TESTPERIOD$

	GOTO 5010

	%PAGE

 TestCheck:
6000	!*******************************************************************
	! Decide weither to copy this check over, or to leave it in the
	! old file to be lost after the rename.
	!*******************************************************************

	GOTO 6300 IF (THIS_BANK$ = "0123456789")	! 1st number

	!
	! Check Balances.  Purge if balances match, and it is not
	! loaded from the next GL period.
	!
	GOTO 6200 IF (BNKAMT = CHKAMT) AND (TESTPERIOD$ <= YYYY_PP$)

	!-------------------------------------------------------------------
	! We need to copy it over
	!-------------------------------------------------------------------

	!
	! Summarize for grand total
	!
	FOR I% = 1% TO KEPT_TOTAL%

		GOTO 6040 IF KEPT_TOTAL(I%)::BANK_ACCT = THIS_BANK$

		IF KEPT_TOTAL(I%)::BANK_ACCT > THIS_BANK$
		THEN
			FOR J% = KEPT_TOTAL% TO I% STEP -1%
				KEPT_TOTAL(J% + 1%) = KEPT_TOTAL(J%)
			NEXT J%
			KEPT_TOTAL% = KEPT_TOTAL% + 1%
			KEPT_TOTAL(I%)::BANK_ACCT = THIS_BANK$
			KEPT_TOTAL(I%)::DEPOSIT = 0.0
			KEPT_TOTAL(I%)::CHECK = 0.0
			KEPT_TOTAL(I%)::ADJUST = 0.0
			GOTO 6040
		END IF
	NEXT I%

	I%, KEPT_TOTAL% = KEPT_TOTAL% + 1%
	KEPT_TOTAL(I%)::BANK_ACCT = THIS_BANK$
	KEPT_TOTAL(I%)::DEPOSIT = 0.0
	KEPT_TOTAL(I%)::CHECK = 0.0
	KEPT_TOTAL(I%)::ADJUST = 0.0

6040	KEPT_TOTAL(I%)::DEPOSIT = &
		KEPT_TOTAL(I%)::DEPOSIT + DEPAMT
	KEPT_TOTAL(I%)::CHECK = &
		KEPT_TOTAL(I%)::CHECK + CHEAMT
	KEPT_TOTAL(I%)::ADJUST = &
		KEPT_TOTAL(I%)::ADJUST + ADJAMT

6110	!
	! Copy over all records for this particular item
	!
	GET #CK_CKMNT.CH%, KEY #0% GE THIS_BANK$ + THIS_CHECK$ + THIS_ETYPE$

	WHILE (CK_CKMNT::BANK_ACCT = THIS_BANK$) AND &
		(CK_CKMNT::CKNUM = THIS_CHECK$) AND &
		(CK_CKMNT::ETYPE = THIS_ETYPE$)

		!
		! Write out one record
		!
6120		PUT #CK_CKMNT_NEW.CH%

		!
		! Pull up next record
		!
6130		WHEN ERROR IN
			GET #CK_CKMNT.CH%
		USE
			!
			! Handle record locks by sleeping for a while, then
			! trying again.
			!
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 6300 IF ERR = 11%
			FILENAME$ = "CK_CKMNT"
			CONTINUE HelpError
		END WHEN
	NEXT

	!
	! And we will end up at the same point in the input file
	! that we were at when we first came in here.
	!
	GOTO 6300

6200	!-------------------------------------------------------------------
	! Coming here means that the record is to be purged from
	! the file.
	!-------------------------------------------------------------------

	!
	! Summarize for grand total
	!
	FOR I% = 1% TO PURGED_TOTAL%

		GOTO 6240 IF PURGED_TOTAL(I%)::BANK_ACCT = THIS_BANK$

		IF PURGED_TOTAL(I%)::BANK_ACCT > THIS_BANK$
		THEN
			FOR J% = PURGED_TOTAL% TO I% STEP -1%
				PURGED_TOTAL(J% + 1%) = PURGED_TOTAL(J%)
			NEXT J%
			PURGED_TOTAL% = PURGED_TOTAL% + 1%
			PURGED_TOTAL(I%)::BANK_ACCT = THIS_BANK$
			PURGED_TOTAL(I%)::DEPOSIT = 0.0
			PURGED_TOTAL(I%)::CHECK = 0.0
			PURGED_TOTAL(I%)::ADJUST = 0.0
			GOTO 6240
		END IF
	NEXT I%

	I%, PURGED_TOTAL% = PURGED_TOTAL% + 1%
	PURGED_TOTAL(I%)::BANK_ACCT = THIS_BANK$
	PURGED_TOTAL(I%)::DEPOSIT = 0.0
	PURGED_TOTAL(I%)::CHECK = 0.0
	PURGED_TOTAL(I%)::ADJUST = 0.0

6240	PURGED_TOTAL(I%)::DEPOSIT = &
		PURGED_TOTAL(I%)::DEPOSIT + DEPAMT/2.0
	PURGED_TOTAL(I%)::CHECK = &
		PURGED_TOTAL(I%)::CHECK + CHEAMT/2.0
	PURGED_TOTAL(I%)::ADJUST = &
		PURGED_TOTAL(I%)::ADJUST + ADJAMT/2.0

6300	!
	! Return back
	!
	THIS_BANK$ = CK_CKMNT::BANK_ACCT + ""
	THIS_CHECK$ = CK_CKMNT::CKNUM + ""
	THIS_ETYPE$ = CK_CKMNT::ETYPE + ""

	BNKAMT = 0.0
	CHKAMT = 0.0

	DEPAMT = 0.0
	CHEAMT = 0.0
	ADJAMT = 0.0

	TESTPERIOD$ = ""

	RETURN

	%PAGE

7000	!*******************************************************************
	! Print summary of the update.
	!*******************************************************************

	!
	! Handle last check number in the file
	!
	GOSUB TestCheck

	CLOSE CK_CKMNT.CH%, CK_CKMNT_NEW.CH%

7010	!
	! Rename the files around as necessary
	!
 !	KILL CK_CKMNT.DEV$ + "CK_CKMNT.MAS"

	SMG_STATUS% = LIB$DELETE_FILE(CK_CKMNT.DEV$ + "CK_CKMNT.MAS;*")

7020	NAME CK_CKMNT.DEV$ + "CK_CKMNT.NEW" AS CK_CKMNT.DEV$ + "CK_CKMNT.MAS"

7025	!
	! Kill all old ck_ckmnt.new files
	!
 !	KILL CK_CKMNT.DEV$ + "CK_CKMNT.NEW" FOR LOOP% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(CK_CKMNT.DEV$ + "CK_CKMNT.NEW;*")

7030	!
	! Reset control file
	!
	GET #CK_CONTROL.CH%, RECORD 1%

	CK_CONTROL::FLAG = "0"
	CK_CONTROL::PERIOD = CUR_PERIOD%
	CK_CONTROL::YEAR = YEAR$

	UPDATE #CK_CONTROL.CH%


	!
	! Print totals
	!
	TITLE$(1%) = "Purge Cancelled Checks"
	TITLE$(2%) = "For the GL Period " + YYYY_PP$
	TITLE$(3%) = "CK System"
	TITLE$(4%) = ""

	TITLE$(5%) = "Bank Code      Deposit         " + &
		"Check        Adjust         Total"
	TITLE$(6%) = ""

	!
	! Init totals
	!
	TOTAL_DEPOSIT = 0.0
	TOTAL_CHECK = 0.0
	TOTAL_ADJUST = 0.0

	!
	! Print out summary
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Summary of Checks Retained", 0%)

	FOR I% = 1% TO KEPT_TOTAL%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			KEPT_TOTAL(I%)::BANK_ACCT + "  " + &
			FORMAT$(KEPT_TOTAL(I%)::DEPOSIT, "###,###,###.##") + &
			FORMAT$(KEPT_TOTAL(I%)::CHECK, "###,###,###.##") + &
			FORMAT$(KEPT_TOTAL(I%)::ADJUST, "###,###,###.##") + &
			FORMAT$(KEPT_TOTAL(I%)::DEPOSIT + &
				KEPT_TOTAL(I%)::CHECK + &
				KEPT_TOTAL(I%)::ADJUST, "###,###,###.##"), &
			0%)

			TOTAL_DEPOSIT = TOTAL_DEPOSIT + KEPT_TOTAL(I%)::DEPOSIT
			TOTAL_CHECK = TOTAL_CHECK + KEPT_TOTAL(I%)::CHECK
			TOTAL_ADJUST = TOTAL_ADJUST + KEPT_TOTAL(I%)::ADJUST
	NEXT I%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Print out Grand total
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"Total   " + &
		FORMAT$(TOTAL_DEPOSIT, "###,###,###.##") + &
		FORMAT$(TOTAL_CHECK, "###,###,###.##") + &
		FORMAT$(TOTAL_ADJUST, "###,###,###.##") + &
		FORMAT$(TOTAL_DEPOSIT + &
			TOTAL_CHECK + &
			TOTAL_ADJUST, "###,###,###.##"), &
		0%)

	!
	! Init totals
	!
	TOTAL_DEPOSIT = 0.0
	TOTAL_CHECK = 0.0
	TOTAL_ADJUST = 0.0

	!
	! Print out summary
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Summary of Checks Purged", 0%)

	FOR I% = 1% TO PURGED_TOTAL%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			PURGED_TOTAL(I%)::BANK_ACCT + "  " + &
			FORMAT$(PURGED_TOTAL(I%)::DEPOSIT, "###,###,###.##") + &
			FORMAT$(PURGED_TOTAL(I%)::CHECK, "###,###,###.##") + &
			FORMAT$(PURGED_TOTAL(I%)::ADJUST, "###,###,###.##") + &
			FORMAT$(PURGED_TOTAL(I%)::DEPOSIT + &
				PURGED_TOTAL(I%)::CHECK + &
				PURGED_TOTAL(I%)::ADJUST, "###,###,###.##"), &
			0%)

			TOTAL_DEPOSIT = TOTAL_DEPOSIT + PURGED_TOTAL(I%)::DEPOSIT
			TOTAL_CHECK = TOTAL_CHECK + PURGED_TOTAL(I%)::CHECK
			TOTAL_ADJUST = TOTAL_ADJUST + PURGED_TOTAL(I%)::ADJUST
	NEXT I%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Print out Grand total
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"Total   " + &
		FORMAT$(TOTAL_DEPOSIT, "###,###,###.##") + &
		FORMAT$(TOTAL_CHECK, "###,###,###.##") + &
		FORMAT$(TOTAL_ADJUST, "###,###,###.##") + &
		FORMAT$(TOTAL_DEPOSIT + &
			TOTAL_CHECK + &
			TOTAL_ADJUST, "###,###,###.##"), &
		0%)

	!
	! Finish up report
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	GOTO ExitProgram

	%PAGE

	!*******************************************************************
	! Exit the program
	!*******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")


	%PAGE

 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
