1	%TITLE "Close Subledger"
	%SBTTL "SB_TRAN_CLOSE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SB_TRAN_CLOSE(LONG OPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		STRING SYSTEM)

	!
	! COPYRIGHT (C) 1987 BY
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	!
	! ABSTRACT:HELP
	!	.p
	!	This function is used to close subledger for
	!	account number
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_TRAN_CLOSE
	!	$ LIB FUNC_LIB:CMCFUN/REP SB_TRAN_CLOSE
	!	$ DELETE SB_TRAN_CLOSE.OBJ;*
	!
	! AUTHOR:
	!
	!	03/20/89 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	05/11/89 - B. Craig Larsen
	!		Added the OPT_ADDREC section of code to allow
	!		for bringing the Ending Balances forward to the
	!		new period records when closing a the current
	!		period.
	!
	!	05/24/91 - Kevin Handy
	!		Modified to close GL_PERIOD file as soon as possible.
	!
	!	08/26/92 - Frank F. Starman
	!		Replace GL_CHARTEX with SB_ACCOUNT
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/18/93 - Dan Perkins
	!		Unwound error trapping.
	!
	!	03/03/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/05/93 - Frank F. Starman
	!		Use RECORDS% variable to count updated records.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Lose extra parameter on ASSG_FREECHANNEL.
	!
	!	11/10/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	11/10/95 - Kevin Handy
	!		Modified to summarize the information from several
	!		GL_YYYY_PP records together, instead of updating
	!		SB_BALANCE for each record individually.
	!
	!	11/10/95 - Kevin Handy
	!		Modified to display records added, deleted, updated,
	!		instead of just total records.
	!
	!	12/15/95 - Kevin Handy
	!		Changed RIGHT(NUM1$()) to FORMAT$().
	!
	!	02/07/96 - Kevin Handy
	!		Clean up (Check).
	!
	!	08/26/97 - Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Added more error traps that were missing.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD		GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	COM (SB_TRAN_CLOSE_COM) ADD_RECORDS%, &
		UPD_RECORDS%, &
		DEL_RECORDS%, &
		SB_CONTROL.CH%, &
		SB_BALANCE.CH%, &
		SB_BALANCE_1.CH%, &
		GL_YYYY_PP.CH%, &
		SB_ACCOUNT.CH%, &
		YYYYPP$ = 6%, &
		YYYY_PP$ = 7%, &
		NEW_PERIOD$ = 6%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION OUTP_UNDEFCODES
	EXTERNAL LONG	FUNCTION OUTP_UNSOLICITED
	EXTERNAL LONG	FUNCTION SB_EXAM_SUBACCOUNT

	DECLARE	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM

	DECLARE LONG	EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

	SELECT OPT

	!
	! Remove batch number
	!
	CASE OPT_RESTART

		!
		! Not used
		!

	CASE OPT_OPENFILE

		CALL ENTR_3MESSAGE(SCOPE, "SB_TRAN_POST - Open Files", 1% + 4%)

200		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.MOD"

			GET #SB_CONTROL.CH%, KEY #0% EQ SYSTEM
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		!
		! Check control flag
		!
		IF SB_CONTROL::CONTROLFLAG <> "0" AND &
			SB_CONTROL::CONTROLFLAG <> "1"
		THEN
			TEXT$ = SPACE$(18%) + &
				"The status flag in the control file is " + &
				SB_CONTROL::CONTROLFLAG + "."

			CALL OUTP_LINE("", UTL_REPORTX,TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

		CUR_PERIOD% = VAL%(RIGHT(SB_CONTROL::PERIOD, 5%))
		YEAR$ = LEFT(SB_CONTROL::PERIOD, 4%)

210		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"

			GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS

			CLOSE #GL_PERIOD.CH%
			CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)
		USE
			FILENAME$ = "GL_PERIOD"
			CONTINUE HelpError
		END WHEN

		YYYYPP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")
		YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

		NEW_PERIOD% = CUR_PERIOD% + 1%

		IF NEW_PERIOD% > GL_PERIOD::FPFY
		THEN
			NEW_PERIOD% = 1%
			YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
		END IF

		NEW_PERIOD$ = YEAR$ + FORMAT$(NEW_PERIOD%, "<0>#")

		!
		! Open up batch control file and get a batch number
		!
		IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "SB_BALANCE", SYSTEM, &
			YYYYPP$, NEW_PERIOD$) <> CMC$_NORMAL
		THEN
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "SB_BALANCE.HIS", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

220		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
		USE
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		SB_BALANCE_1.CH% = SB_BALANCE.CH%
		SB_BALANCE.CH% = 0%

230		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.CRE"
		USE
			FILENAME$ = "SB_BALANCE_1"
			CONTINUE HelpError
		END WHEN

240		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
		USE
			CONTINUE 250 IF ERR = 5%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

250		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
		USE
			FILENAME$ = "SB_ACCOUNT"
			CONTINUE HelpError
		END WHEN

		!
		! Tell the control file we are ready to continue
		!
		ADD_RECORDS% = 0%
		UPD_RECORDS% = 0%
		DEL_RECORDS% = 0%

		SB_CONTROL::CONTROLFLAG = "1"
		SB_CONTROL::BATCH = BATCH_NUMBER
		SB_CONTROL::CDATE = DATE_TODAY
		SB_CONTROL::CTIME = TIME_NOW

260		WHEN ERROR IN
			UPDATE #SB_CONTROL.CH%
		USE
			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

	CASE OPT_ADDREC

400		!************************************************************
		! Zero the current balances with the current period in
		! the SB_BALANCE file.
		!************************************************************

		CALL ENTR_3MESSAGE(SCOPE, &
			"SB_TRAN_POST - Init Current Period", 1% + 4%)

		WHEN ERROR IN
			FIND #SB_BALANCE.CH%, KEY #1% EQ YYYYPP$ + SYSTEM
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 420 IF ERR = 155%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

 NextRec1:
410		WHEN ERROR IN
			GET #SB_BALANCE.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 420 IF ERR = 11%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		GOTO 420 IF SB_BALANCE::PERIOD <> YYYYPP$ OR &
			SB_BALANCE::SYSTEM <> SYSTEM

		!
		! Test subaccount number
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT(SB_CONTROL::SUBJECT, &
			SB_BALANCE::SUBACCOUNT, SB_SUBACCOUNT_EXAM)

		SELECT EXIT_STATUS

		CASE CMC$_NORMAL
			SB_BALANCE::AMOUNT = 0.0
			SB_BALANCE::UNITS = 0.0
			SB_BALANCE::HOURS = 0.0

			WHEN ERROR IN
				UPDATE #SB_BALANCE.CH%
			USE
				FILENAME$ = "SB_BALANCE"
				CONTINUE HelpError
			END WHEN

		CASE CMC$_UNDEFINED
			WHEN ERROR IN
				DELETE #SB_BALANCE.CH%
			USE
				FILENAME$ = "SB_BALANCE"
				CONTINUE HelpError
			END WHEN
			DEL_RECORDS% = DEL_RECORDS% + 1%

		CASE ELSE
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction

		END SELECT

		!
		! Try for next record
		!
		GOTO NextRec1

420		!
		! Update the current balances with current period in the
		! SB_BALANCE file from the GL_YYYY_PP file.
		!
		ACCT_WLD$ = ""
		COMMA$ = ""
		WHEN ERROR IN
			FIND #SB_ACCOUNT.CH%, KEY #0% EQ SYSTEM, REGARDLESS
		USE
			CONTINUE 425 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "SB_ACCOUNT"
			CONTINUE HelpError
		END WHEN

 NextAcct:
		WHEN ERROR IN
			GET #SB_ACCOUNT.CH%, REGARDLESS
		USE
			CONTINUE 425 IF ERR = 11% OR ERR = 9%
			FILENAME$ = "SB_ACCOUNT"
			CONTINUE HelpError
		END WHEN

		IF SB_ACCOUNT::SYSTEM = SYSTEM
		THEN
			ACCT_WLD$ = ACCT_WLD$ + COMMA$ + &
				TRM$(SB_ACCOUNT::ACCOUNT)
			COMMA$ = ","
			GOTO NextAcct
		END IF

425		IF ACCT_WLD$ = ""
		THEN
			TEXT$ = SPACE$(18%) + "Can't find WP account numbers."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

		!*********************************************************
		! Read the general ledger, acanning for valid job records
		!*********************************************************

		CALL ENTR_3MESSAGE(SCOPE, "SB_TRAN_POST - Scan GL", 1% + 4%)

		B_SUBACC$ = ""
		B_OPERATION$ = ""
		B_ACCT$ = ""

		B_AMOUNT = 0.0
		B_UNITS  = 0.0
		B_HOURS  = 0.0

		GL_YYYY_PP::SUBACC = ""

		WHEN ERROR IN
			FIND #GL_YYYY_PP.CH%, &
				KEY #1% GT GL_YYYY_PP::SUBACC, &
				REGARDLESS
		USE
			CONTINUE 460 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

 NextRec2:
430		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%, REGARDLESS
		USE
			CONTINUE 460 IF ERR = 11%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

450		IF COMP_STRING(GL_YYYY_PP::ACCT, ACCT_WLD$)
		THEN
			!
			! If we've gone to another group
			!
			IF (B_SUBACC$ <> GL_YYYY_PP::SUBACC) OR &
				(B_OPERATION$ <> GL_YYYY_PP::OPERATION) OR &
				(B_ACCT$ <> GL_YYYY_PP::ACCT)
			THEN
				GOSUB InsertBalance

				!
				! Remember the current group
				!
				B_SUBACC$ = GL_YYYY_PP::SUBACC
				B_OPERATION$ = GL_YYYY_PP::OPERATION
				B_ACCT$ = GL_YYYY_PP::ACCT
			END IF

			B_AMOUNT = B_AMOUNT + GL_YYYY_PP::AMOUNT
			B_UNITS  = B_UNITS  + GL_YYYY_PP::UNITS
			B_HOURS  = B_HOURS  + GL_YYYY_PP::HOURS

		END IF

		!
		! Try for next record
		!
		GOTO NextRec2

460		!
		! Load in the last balance record
		!
		GOSUB InsertBalance

		CLOSE GL_YYYY_PP.CH%, SB_ACCOUNT.CH%

	CASE OPT_CONFIRM

		EXIT_STATUS = OUTP_UNDEFCODES(OPT_CONFIRM, &
			TITLE(), UTL_REPORTX, "")

	CASE OPT_UPDATE

		CALL ENTR_3MESSAGE(SCOPE, &
			"SB_TRAN_POST - Update Balance", 1% + 4%)

		WHEN ERROR IN
			FIND #SB_BALANCE_1.CH%, &
				KEY #1% EQ YYYYPP$ + SYSTEM, &
				REGARDLESS
		USE
			CONTINUE ExitFunction IF ERR = 155%
			FILENAME$ = "SB_BALANCE_1"
			CONTINUE HelpError
		END WHEN

 NextRec3:
470		WHEN ERROR IN
			GET #SB_BALANCE_1.CH%, REGARDLESS
		USE
			CONTINUE ExitFunction IF ERR = 11%
			FILENAME$ = "SB_BALANCE_1"
			CONTINUE HelpError
		END WHEN

		GOTO ExitFunction IF SB_BALANCE::PERIOD <> YYYYPP$ OR &
			SB_BALANCE::SYSTEM <> SYSTEM

		BEG_AMOUNT = SB_BALANCE::BEG_AMOUNT + SB_BALANCE::AMOUNT
		BEG_UNITS  = SB_BALANCE::BEG_UNITS  + SB_BALANCE::UNITS
		BEG_HOURS  = SB_BALANCE::BEG_HOURS  + SB_BALANCE::HOURS

480		!
		! Look for the Record in the new period,
		! (last period closed + 2)
		!
		WHEN ERROR IN
			FIND #SB_BALANCE.CH%, &
				KEY #0% EQ SB_BALANCE::SYSTEM + &
				SB_BALANCE::SUBACCOUNT + &
				SB_BALANCE::OPERATION + &
				SB_BALANCE::ACCOUNT + &
				NEW_PERIOD$
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 490 IF ERR = 155%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

485		WHEN ERROR IN
			GET #SB_BALANCE.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		SB_BALANCE::BEG_AMOUNT	= BEG_AMOUNT
		SB_BALANCE::BEG_UNITS	= BEG_UNITS
		SB_BALANCE::BEG_HOURS	= BEG_HOURS

		WHEN ERROR IN
			UPDATE #SB_BALANCE.CH%
		USE
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		GOTO NextRec3

490		IF ABS(BEG_AMOUNT + BEG_UNITS + BEG_HOURS) <> 0.0
		THEN
			SB_BALANCE::PERIOD = NEW_PERIOD$

			SB_BALANCE::AMOUNT = 0.0
			SB_BALANCE::UNITS  = 0.0
			SB_BALANCE::HOURS  = 0.0

			SB_BALANCE::BEG_AMOUNT	= BEG_AMOUNT
			SB_BALANCE::BEG_UNITS	= BEG_UNITS
			SB_BALANCE::BEG_HOURS	= BEG_HOURS

			WHEN ERROR IN
				PUT #SB_BALANCE.CH%
			USE
				FILENAME$ = "SB_BALANCE"
				CONTINUE HelpError
			END WHEN

			ADD_RECORDS% = ADD_RECORDS% + 1%
		END IF

		GOTO NextRec3

	CASE OPT_CLOSEFILE

		CALL ENTR_3MESSAGE(SCOPE, "SB_TRAN_POST - Close files", 1% + 4%)

500		WHEN ERROR IN
			GET #SB_CONTROL.CH%, KEY #0% EQ SYSTEM

			SB_CONTROL::PERIOD	= NEW_PERIOD$
			SB_CONTROL::CONTROLFLAG	= "0"

			UPDATE #SB_CONTROL.CH%
			CLOSE #SB_CONTROL.CH%
		USE
			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		TEXT$ = SPACE$(9%) + FORMAT$(ADD_RECORDS%, "########") + &
			" Records added in " + YYYYPP$ + " Ledger"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%) &
			IF ADD_RECORDS%

		TEXT$ = SPACE$(9%) + FORMAT$(UPD_RECORDS%, "########") + &
			" Records updated in " + YYYYPP$ + " Ledger"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%) &
			IF UPD_RECORDS%

		TEXT$ = SPACE$(9%) + FORMAT$(DEL_RECORDS%, "########") + &
			" Records deleted in " + YYYYPP$ + " Ledger"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%) &
			IF DEL_RECORDS%

	CASE OPT_REPORT

		CALL ENTR_3MESSAGE(SCOPE, &
			"SB_TRAN_POST - Print Report", 1% + 4%)

		!
		! Print undefined code if any
		!
		TEXT$ = " Subacct#    Operation AccountNumber "

		EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), &
			UTL_REPORTX, TEXT$)

	CASE ELSE

		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:

	SB_TRAN_CLOSE = EXIT_STATUS

	EXIT FUNCTION

	%PAGE

 InsertBalance:
	!*******************************************************************
	! Insert information into the SB_BALANCE file
	!*******************************************************************

	GOTO 12490 IF B_SUBACC$ = ""

12450	!
	! Look for an existing balance record
	!
	WHEN ERROR IN
		GET #SB_BALANCE.CH%, KEY #0% EQ SYSTEM + &
			B_SUBACC$ + &
			B_OPERATION$ + &
			B_ACCT$ + YYYYPP$
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 12455 IF ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	!
	! Update existing balance record
	!
	SB_BALANCE::AMOUNT = SB_BALANCE::AMOUNT + B_AMOUNT
	SB_BALANCE::UNITS  = SB_BALANCE::UNITS  + B_UNITS
	SB_BALANCE::HOURS  = SB_BALANCE::HOURS  + B_HOURS

	WHEN ERROR IN
		UPDATE #SB_BALANCE.CH%
	USE
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	UPD_RECORDS% = UPD_RECORDS% + 1%

	GOTO 12490

12455	!
	! Balance record not found, so try to create a new one
	!

	!
	! Test subaccount number for being a defined account
	!
	EXIT_STATUS = SB_EXAM_SUBACCOUNT(SB_CONTROL::SUBJECT, &
		B_SUBACC$, SB_SUBACCOUNT_EXAM)

	SELECT EXIT_STATUS

	CASE CMC$_NORMAL

		!
		! Subaccount is defined, so create balance info
		!
		SB_BALANCE::SYSTEM	= SYSTEM
		SB_BALANCE::SUBACCOUNT	= B_SUBACC$
		SB_BALANCE::OPERATION	= B_OPERATION$
		SB_BALANCE::ACCOUNT	= B_ACCT$
		SB_BALANCE::PERIOD	= YYYYPP$

		SB_BALANCE::AMOUNT	= B_AMOUNT
		SB_BALANCE::UNITS	= B_UNITS
		SB_BALANCE::HOURS	= B_HOURS

		SB_BALANCE::BEG_AMOUNT	= 0.0
		SB_BALANCE::BEG_UNITS	= 0.0
		SB_BALANCE::BEG_HOURS	= 0.0

		WHEN ERROR IN
			PUT #SB_BALANCE.CH%
		USE
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		ADD_RECORDS% = ADD_RECORDS% + 1%

	CASE CMC$_UNDEFINED
		!
		! Undefined subaccount
		!
		TEXT$ = "*" + B_SUBACC$ + " " + &
			B_OPERATION$ +  " " + &
			B_ACCT$

		EXIT_STATUS = OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$)

	CASE ELSE
		!
		! Some unknown error
		!
		EXIT_STATUS = CMC$_UNTERROR
		GOTO ExitFunction

	END SELECT

12490	!
	! Reset amounts
	!
	B_AMOUNT = 0.0
	B_UNITS  = 0.0
	B_HOURS  = 0.0

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
