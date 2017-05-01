1	%TITLE "SB Posting to Sub Ledger System"
	%SBTTL "SB_TRAN_POST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SB_TRAN_POST(LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		GL_YYYY_PP_CDD GL_YYYY_PP_POST, &
		STRING GLPERIOD)

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
	!
	! Abstract:HELP
	!	.p
	!	This function is used to post to the SB balance file
	!
	! Index:
	!	POST
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	OPT
	!		This is used to indicate which option is in process.
	!
	!	BATCH_NUMBER
	!		Batch number assigned for this posting.
	!
	!	TITLE()
	!		The Title used in the-report process.
	!
	!	UTL_REPORTX
	!		Used in the-report process.
	!
	!	GLPERIOD
	!		Not used for anything.
	!
	!	FILE CHANNELS OPENED
	!		SB_BALANCE.SEQ%
	!		SB_BALANCE.CH%
	!		SB_ACCOUNT.CH%
	!
	! Outputs:
	!
	!	SB_TRAN_POST
	!		The exit status of the function.
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_TRAN_POST
	!	$ LIB FUNC_LIB:CMCFUN/REP SB_TRAN_POST
	!	$ DELETE SB_TRAN_POST.OBJ;*
	!
	! Author:
	!
	!	02/09/89 - B. Craig Larsen
	!
	! Modification History:
	!
	!	05/30/89 - B. Craig Larsen
	!		Modified to read the system from GL_CHARTEX.
	!
	!	08/26/92 - Frank F. Starman
	!		Replace GL_CHARTEX with SB_ACCOUNT
	!
	!	09/01/92 - Dan Perkins
	!		Fixed error in common statement holding GL_ACCOUNT.CH%
	!		open instead of SB_ACCOUNT.CH%.
	!		Trap error 11 on line 310.
	!		Changed lines in OPT_ADDREC to GL_YYYY_PP_POST
	!		instead of GL_YYYY_PP so we can get some informtion
	!		to build the SB_BALANCE record.
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/24/92 - Frank F. Starman
	!		Post to open period in SB_CONTROL file.
	!
	!	10/05/92 - Dan Perkins
	!		Open SB_ACCOUNT file first and exit function if
	!		there is no file.  Modify error trapping on lines
	!		320 and 455.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/30/92 - Frank F. Starman
	!		Check for subaccount.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/19/92 - Frank F. Starman
	!		Enable message if flag in the control file
	!		is not zero.
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/16/93 - Dan Perkins
	!		Skip SB_CONTROL stuff at line 210 if we can't
	!		find the record.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/12/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Lose commented out code.
	!
	!	10/29/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Map Statements
	!
	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE
	MAP (SB_BALANCE_SEQ)	SB_BALANCE_CDD		SB_BALANCE_SEQ

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE		SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_READ

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	COM (SB_TRAN_POST.BALANCE) &
		SB_BALANCE.CH%, &
		SB_BALANCE.SEQ%, &
		SB_ACCOUNT.CH%, &
		SB_CONTROL.CH%, &
		SBPERIOD$ = 6%, &
		ONE_RECORD%

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED
	EXTERNAL LONG	FUNCTION	SB_EXAM_SUBACCOUNT

	DECLARE LONG EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

	SELECT OPT

	CASE OPT_CHECK

		ONE_RECORD% = 0%

200		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
		USE
			CONTINUE 210 IF ERR = 5%
			FILENAME$ = "SB_ACCOUNT"
			CONTINUE HelpError
		END WHEN

210		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
			GET #SB_CONTROL.CH%, KEY #0% EQ "JC"
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 220 IF ERR = 5% OR ERR = 155%
			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		!
		! Check control flag
		!
		IF SB_CONTROL::CONTROLFLAG <> "0"
		THEN
			TEXT$ = SPACE$(18%) + "The status flag in the " + &
				"subaccount control " + &
				"file is " + SB_CONTROL::CONTROLFLAG + "."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

220		CALL ASSG_CHANNEL(SB_BALANCE.SEQ%, STAT%)

		WHEN ERROR IN
			OPEN "SB_BALANCE.SEQ" AS FILE SB_BALANCE.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP SB_BALANCE_SEQ, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "SB_BALANCE.SEQ"
			CONTINUE HelpError
		END WHEN

	!
	! Create posting array
	!
	CASE OPT_ADDREC

300		FOR I% = 1% TO 3%

			SELECT I%

			CASE 1%
				SUBJECT$ = "J"
				SYSTEM$ = "JC"
			CASE 2%
				SUBJECT$ = "S"
				SYSTEM$ = "SA"
			CASE 3%
				SUBJECT$ = "E"
				SYSTEM$ = "EL"
			END SELECT

			GOTO SARecord IF SB_EXAM_SUBACCOUNT(SUBJECT$, &
				GL_YYYY_PP_POST::SUBACC, &
				SB_SUBACCOUNT_READ) = CMC$_NORMAL

305		NEXT I%
		SUBJECT$ = " "
		SYSTEM$  = "  "

 SARecord:
		!
		! Start to generate a SA record
		!
		SB_BALANCE::SUBACCOUNT	= GL_YYYY_PP_POST::SUBACC
		SB_BALANCE::OPERATION	= GL_YYYY_PP_POST::OPERATION
		SB_BALANCE::HOURS	= GL_YYYY_PP_POST::HOURS
		SB_BALANCE::ACCOUNT	= GL_YYYY_PP_POST::ACCT
		SB_BALANCE::AMOUNT	= GL_YYYY_PP_POST::AMOUNT
		SB_BALANCE::UNITS	= GL_YYYY_PP_POST::UNITS

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


310		WHEN ERROR IN
			RESET #SB_ACCOUNT.CH%
		USE
			CONTINUE ExitFunction IF ERR = 11% OR ERR = 155% OR ERR = 9%
			FILENAME$ = "SB_ACCOUNT"
			CONTINUE HelpError
		END WHEN

 NextAcct:
315		WHEN ERROR IN
			GET #SB_ACCOUNT.CH%, REGARDLESS
		USE
			CONTINUE ExitFunction IF ERR = 11% OR ERR = 155% OR ERR = 9%
			FILENAME$ = "SB_ACCOUNT"
			CONTINUE HelpError
		END WHEN

320		IF COMP_STRING(GL_YYYY_PP_POST::ACCT, SB_ACCOUNT::ACCOUNT)
		THEN
			IF SYSTEM$ = "  "
			THEN
				TEXT$ = SPACE$(18%) + "Undefined subaccount " + &
					SB_BALANCE::SUBACCOUNT
				CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF

			GOTO NextAcct IF SB_ACCOUNT::SYSTEM <> SYSTEM$
			!
			! Read period
			!
			WHEN ERROR IN
				GET #SB_CONTROL.CH%, &
					KEY #0% EQ SYSTEM$, &
					REGARDLESS
			USE
				CONTINUE 315 IF ERR = 155% OR ERR = 9%
				FILENAME$ = "SB_CONTROL"
				CONTINUE HelpError
			END WHEN

			SB_BALANCE_SEQ = SB_BALANCE
			SB_BALANCE_SEQ::PERIOD		= SB_CONTROL::PERIOD
			SB_BALANCE_SEQ::SYSTEM		= SYSTEM$
			SB_BALANCE_SEQ::BEG_AMOUNT	= 0.0
			SB_BALANCE_SEQ::BEG_UNITS	= 0.0
			SB_BALANCE_SEQ::BEG_HOURS	= 0.0

			WHEN ERROR IN
				PUT #SB_BALANCE.SEQ%
			USE
				CONTINUE 315 IF ERR = 155% OR ERR = 9%
				FILENAME$ = "SB_CONTROL"
				CONTINUE HelpError
			END WHEN

			ONE_RECORD% = -1%
			!EXIT_STATUS = CMC$_WARNING
		ELSE
			GOTO NextAcct
		END IF

	CASE OPT_POSTFILE

		GOTO ExitFunction IF ONE_RECORD% = 0%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "SB_BALANCE.HIS", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

455		%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.CRE"

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		RECORDS%, RECORDS_ADD% = 0%

		RESET #SB_BALANCE.SEQ%

460		WHEN ERROR IN
			GET #SB_BALANCE.SEQ%
		USE
			CONTINUE 485 IF ERR = 11%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		IF SUBOPT AND SUBOPT_REVERSE
		THEN
			SB_BALANCE_SEQ::AMOUNT	= - SB_BALANCE_SEQ::AMOUNT
			SB_BALANCE_SEQ::UNITS	= - SB_BALANCE_SEQ::UNITS
			SB_BALANCE_SEQ::HOURS	= - SB_BALANCE_SEQ::HOURS
			SB_BALANCE_SEQ::PERIOD	=   SBPERIOD$
		END IF

470		!
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

		WHEN ERROR IN
			GET #SB_BALANCE.CH%, KEY #0% EQ &
				SB_BALANCE_SEQ::SYSTEM + &
				SB_BALANCE_SEQ::SUBACCOUNT + &
				SB_BALANCE_SEQ::OPERATION + &
				SB_BALANCE_SEQ::ACCOUNT + &
				SB_BALANCE_SEQ::PERIOD
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 480 IF ERR = 155%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		SB_BALANCE::AMOUNT = SB_BALANCE::AMOUNT + &
			SB_BALANCE_SEQ::AMOUNT

		SB_BALANCE::UNITS = SB_BALANCE::UNITS + &
			SB_BALANCE_SEQ::UNITS

		SB_BALANCE::HOURS = SB_BALANCE::HOURS + &
			SB_BALANCE_SEQ::HOURS

		WHEN ERROR IN
			UPDATE #SB_BALANCE.CH%
		USE
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%

		GOTO 460

480		!
		! Put record into SB_BALANCE file
		!
		SB_BALANCE = SB_BALANCE_SEQ

		WHEN ERROR IN
			PUT #SB_BALANCE.CH%
		USE
			CONTINUE 460 IF ERR = 134%
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN

		RECORDS_ADD% = RECORDS_ADD% + 1%

		GOTO 460

485		CLOSE #SB_BALANCE.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Updated Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS_ADD%, "########") + &
			" Posted Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:
	SB_TRAN_POST = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

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
