1	%TITLE "Posting to IC System"
	%SBTTL "IC_TRAN_POST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_TRAN_POST(LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		IC_TRANSACTION_CDD IC_TRANSACTION_POST, &
		STRING ICPERIOD)

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
	!	.b
	!	.lm +5
	!	This function is used to post to the IC ledger file,
	!	and in the balance file.
	!	.b
	!	Pass 0
	!	.lm +5
	!	Read Inventory control file
	!	.lm -5
	!	Pass 1
	!	.lm +5
	!	Remove records with the current batch number from Transaction
	!	and Balance with
	!	.lm -5
	!	Pass 2
	!	.lm +5
	!	Generate the totals.
	!	.lm -5
	!	Pass 3
	!	.lm +5
	!	Display transmittal
	!	.lm -5
	!	Pass 4
	!	.lm +5
	!	Add records into the transaction file.
	!	Update quantity into the balance files.
	!	.lm -5
	!	Pass 5
	!	.lm +5
	!	Prints summary
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	OPT
	!		This is used to indicate which pass is in process.
	!
	!	BATCH_NUMBER
	!		Batch number assigned for this posting.
	!
	!	FILE CHANNELS OPENED
	!		IC_TRANSACTION.CH%
	!		IC_35BALANCE.CH%
	!		UTL_TRANSTYPE.CH%
	!
	! Outputs:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_TRAN_POST
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_TRAN_POST
	!	$ DELETE IC_TRAN_POST.OBJ;*
	!
	! Author:
	!
	!	09/23/87 - B. Craig Larsen
	!
	! Modification History:
	!
	!	11/03/87 - Frank Starman
	!		Modified so that this function removes prior
	!		updated totals in the first pass.
	!
	!	05/11/88 - Frank Starman
	!		Add into the Balance File (Store file doesn't exist)
	!
	!	11/06/90 - Kevin Handy
	!		Fixed bug in restarting a post.  If there were
	!		no records posted into the file, this function
	!		would generate an error stopping the post and telling
	!		them to try again (which would cause the same error
	!		because there was still no records posted for that
	!		batch).
	!
	!	12/19/90 - Val James Allen
	!		Modified to drop last two arguments and use transaction
	!		field information.
	!
	!	12/20/90 - Val James Allen
	!		Modified to post transactions to current period
	!		running balance (RBALANCE) field (reversed)
	!
	!	09/16/91 - Frank F. Starman
	!		Set RBALANCE to zero if this will be a new record
	!		in the Balance file.
	!
	!	10/09/91 - Frank F. Starman
	!		Check for keyboard input after marking file.
	!
	!	01/01/92 - Frank F. Starman
	!		Post to IC_35HISTORY file.
	!
	!	02/25/92 - Kevin Handy
	!		Added error messages instead of just printing
	!		Frank's "Aborted" message without any
	!		explination, which makes things impossible
	!		to figure out.
	!
	!	06/03/92 - Frank F. Starman
	!		Fixed bug while posting to IC_35HISTORY and
	!		record already exists.
	!
	!	06/15/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/24/93 - Dan Perkins
	!		Added "IC" to error text messages so user would
	!		know from what system they come.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/14/93 - Frank F. Starman
	!		Print warning if process had beed interrupted.
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	10/19/96 - Kevin Handy
	!		Reformat source code
	!
	!	11/27/96 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/28/99 - Kevin Handy
	!		Clean up source code
	!
	!	09/08/2000 - Kevin Handy
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

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Memory MAPs
	!
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY
	MAP (IC_35HISTORY_SEQ)	IC_35HISTORY_CDD	IC_35HISTORY_SEQ
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE
	MAP (IC_35BALANCE_SEQ)	IC_35BALANCE_CDD	IC_35BALANCE_SEQ
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION
	MAP (DP_OUTP_XUNSOL)	RRR_FLAG%

	!
	! Common memory areas
	!
	COM (IC_TRAN_POST.CH) &
		IC_TRANSACTION.CH%, &
		IC_TRANSACTION.SEQ%, &
		IC_35BALANCE.CH%, &
		IC_35BALANCE.SEQ%, &
		IC_35HISTORY.CH%, &
		IC_35HISTORY.SEQ%

	COM (IC_TRAN_POST.COM) &
		STRING	START_DATE = 8%, &
		STRING	END_DATE = 8%, &
		STRING	PERIOD_DESCR = 20%, &
		LONG	OUT_DATE


	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED

	DECLARE	LONG	EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

	!
	! Set record counter to zero
	!
	RECORDS% = 0%

	SELECT OPT

	!
	! Remove batch number
	!
	CASE OPT_RESTART

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"IC_TRANSACTION_" + ICPERIOD + ".LED", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		YYYYPP$ = ICPERIOD

		!
		! Open IC transaction file
		!
100		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.CRE"
		USE
			!
			! Cannot lock file
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		!
		! Do any records have this batch number?
		!
105		WHEN ERROR IN
			FIND #IC_TRANSACTION.CH%, KEY #1% EQ BATCH_NUMBER
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 120 IF ERR = 155%
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

110		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitKeyFunction
			END IF
		END IF

		WHEN ERROR IN
			GET #IC_TRANSACTION.CH%
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 120 IF ERR = 11%
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

		IF IC_TRANSACTION::BATCH = BATCH_NUMBER
		THEN
			DELETE #IC_TRANSACTION.CH%
			RECORDS% = RECORDS% + 1%
			GOTO 110
		END IF

120		CLOSE #IC_TRANSACTION.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		TEXT$ = FORMAT$(RECORDS%, "         ######## ") + &
			"Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		IF RECORDS% <> 0%
		THEN
			TEXT$ = "%Warning: Needed to run inventory " + &
				"resynchronization process."
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		END IF

	CASE OPT_CHECK

200		!
		! Open IC control file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
			GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		USE
			!
			! Cannot lock file
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "IC_CONTROL"
			CONTINUE HelpError
		END WHEN

		CLOSE IC_CONTROL.CH%
		CALL ASSG_FREECHANNEL(IC_CONTROL.CH%)

		!
		! Check if not posting to previous period
		!
		IF (ICPERIOD < IC_CONTROL::PERIOD)
		THEN
			TEXT$ = "%IC period " + ICPERIOD + " is not open"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

		!
		! Check control flag
		!
		IF IC_CONTROL::CONTROLFLAG <> "0"
		THEN
			TEXT$ = "%The IC control file status flag is " + &
				IC_CONTROL::CONTROLFLAG

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

		!
		! Check if posting to open period
		!
		IF READ_PERIOD("READ", IC_CONTROL::ERA, ICPERIOD, &
			PERIOD_DESCR, STAT$, START_DATE, END_DATE, 0%)
		THEN
			TEXT$ = "%IC Period " + ICPERIOD + &
				" doesn't exist for accounting era " + &
				IC_CONTROL::ERA + "."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

210		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
			GET #IC_TRANSACTION.CH%, KEY #1% EQ BATCH_NUMBER
		USE
			!
			! Cannot lock file
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 220 IF (ERR = 5%) OR (ERR = 155%)
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

		TEXT$ = "%IC Batch Number exists in Transaction File"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

220		CALL ASSG_CHANNEL(IC_TRANSACTION.SEQ%, STAT%)

		WHEN ERROR IN
			OPEN "IC_TRANSACTION.SEQ" AS FILE IC_TRANSACTION.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP IC_TRANSACTION, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "IC_TRANSACTION.SEQ"
			CONTINUE HelpError
		END WHEN

230		CALL ASSG_CHANNEL(IC_35BALANCE.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "IC_35BALANCE.SEQ" AS FILE IC_35BALANCE.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP IC_35BALANCE_SEQ, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "IC_35BALANCE.SEQ"
			CONTINUE HelpError
		END WHEN

240		CALL ASSG_CHANNEL(IC_35HISTORY.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "IC_35HISTORY.SEQ" AS FILE IC_35HISTORY.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP IC_35HISTORY_SEQ, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "IC_35HISTORY.SEQ"
			CONTINUE HelpError
		END WHEN

		!
		! Assume dates will be OK
		!
		OUT_DATE = CMC$_NORMAL

	!
	! Create posting array
	!
	CASE OPT_ADDREC

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitKeyFunction
			END IF
		END IF

		!
		! Check date range
		!
		IF (SUBOPT AND SUBOPT_CHECK)
		THEN
			EXIT_STATUS, OUT_DATE = CMC$_DATEOUT IF &
				(IC_TRANSACTION_POST::TRANS_DATE < START_DATE) OR &
				(IC_TRANSACTION_POST::TRANS_DATE > END_DATE)
		END IF

		!
		! Create transaction array
		!
		IC_TRANSACTION = IC_TRANSACTION_POST
		PUT #IC_TRANSACTION.SEQ%

		!
		! Create balance array
		!
		IF (IC_TRANSACTION_POST::TYPE_A <> "") AND &
			(IC_TRANSACTION_POST::QUANTITY_A <> 0.0)
		THEN
			BALANCE_IC% = BALANCE_IC% + 1%

			IC_35BALANCE_SEQ::PRODUCT	= &
				IC_TRANSACTION_POST::PRODUCT
			IC_35BALANCE_SEQ::LOCATION	= &
				IC_TRANSACTION_POST::LOCATION
			IC_35BALANCE_SEQ::TRANSTYPE	= &
				IC_TRANSACTION_POST::TYPE_A
			IC_35BALANCE_SEQ::PBALANCE	= &
				IC_TRANSACTION_POST::QUANTITY_A

			PUT #IC_35BALANCE.SEQ%

			IC_35HISTORY_SEQ::PRODUCT	= &
				IC_TRANSACTION_POST::PRODUCT
			IC_35HISTORY_SEQ::LOCATION	= &
				IC_TRANSACTION_POST::LOCATION
			IC_35HISTORY_SEQ::TRANSTYPE	= &
				IC_TRANSACTION_POST::TYPE_A
			IC_35HISTORY_SEQ::CROSSREF	= &
				IC_TRANSACTION_POST::CROSS_REF
			IC_35HISTORY_SEQ::SUBACCT	= &
				IC_TRANSACTION_POST::SUBACCOUNT
			IC_35HISTORY_SEQ::PQUANTITY(0%)	= &
				IC_TRANSACTION_POST::QUANTITY_A
			IC_35HISTORY_SEQ::PRICEAMT(0%)	= &
				IC_TRANSACTION_POST::PRICE
			IC_35HISTORY_SEQ::COSTAMT(0%)	= &
				IC_TRANSACTION_POST::COST

			PUT #IC_35HISTORY.SEQ%

		END IF

		IF (IC_TRANSACTION_POST::TYPE_B <> "") AND &
			(IC_TRANSACTION_POST::QUANTITY_B <> 0.0)
		THEN
			BALANCE_IC% = BALANCE_IC% + 1%

			IC_35BALANCE_SEQ::PRODUCT	= &
				IC_TRANSACTION_POST::PRODUCT
			IC_35BALANCE_SEQ::LOCATION	= &
				IC_TRANSACTION_POST::LOCATION
			IC_35BALANCE_SEQ::TRANSTYPE	= &
				IC_TRANSACTION_POST::TYPE_B
			IC_35BALANCE_SEQ::PBALANCE	= &
				IC_TRANSACTION_POST::QUANTITY_B

			PUT #IC_35BALANCE.SEQ%

			IC_35HISTORY_SEQ::PRODUCT	= &
				IC_TRANSACTION_POST::PRODUCT
			IC_35HISTORY_SEQ::LOCATION	= &
				IC_TRANSACTION_POST::LOCATION
			IC_35HISTORY_SEQ::TRANSTYPE	= &
				IC_TRANSACTION_POST::TYPE_B
			IC_35HISTORY_SEQ::CROSSREF	= &
				IC_TRANSACTION_POST::CROSS_REF
			IC_35HISTORY_SEQ::SUBACCT	= &
				IC_TRANSACTION_POST::SUBACCOUNT
			IC_35HISTORY_SEQ::PQUANTITY(0%)	= &
				IC_TRANSACTION_POST::QUANTITY_B
			IC_35HISTORY_SEQ::PRICEAMT(0%)	= 0.0
			IC_35HISTORY_SEQ::COSTAMT(0%)	= 0.0

			PUT #IC_35HISTORY.SEQ%
		END IF

	!
	! Pass 3
	!
	CASE OPT_CONFIRM
		!
		! Confirm
		!
		IF OUT_DATE <> CMC$_NORMAL
		THEN
			TEXT$ = "%Date out of date range in posting to IC"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
		END IF

	!
	! Post to the IC transaction file and the Balance file
	!
	CASE OPT_POSTFILE

		YYYYPP$ = ICPERIOD

400		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"IC_TRANSACTION_" + ICPERIOD + ".LED", &
			"", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitKeyFunction
			END IF
		END IF

		!
		! Open IC transaction file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.CRE"
		USE
			!
			! Cannot lock file
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		!
		! Go to the beginning of the temporary IC Transaction file
		!
410		RESET #IC_TRANSACTION.SEQ%

420		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitKeyFunction
			END IF
		END IF

		WHEN ERROR IN
			GET #IC_TRANSACTION.SEQ%
			PUT #IC_TRANSACTION.CH%
		USE
			CONTINUE 430 IF ERR = 11%
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 420

430		CLOSE IC_TRANSACTION.CH%
		CLOSE IC_TRANSACTION.SEQ%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
			"Posted IC Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

455		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "IC_35BALANCE.HIS", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitKeyFunction
			END IF
		END IF

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"
		USE
			!
			! Cannot lock file
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "IC_35BALANCE"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		RECORDS%, RECORDS_ADD% = 0%

		!
		! Start at the beginning of the Balances file
		!
		WHEN ERROR IN
			RESET #IC_35BALANCE.SEQ%
		USE
			FILENAME$ = "IC_35BALANCE"
			CONTINUE HelpError
		END WHEN

460		WHEN ERROR IN
			GET #IC_35BALANCE.SEQ%
		USE
			CONTINUE 485 IF ERR = 11%
			FILENAME$ = "IC_35BALANCE"
			CONTINUE HelpError
		END WHEN

470		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitKeyFunction
			END IF
		END IF

		WHEN ERROR IN
			GET #IC_35BALANCE.CH%, KEY #0% EQ &
				IC_35BALANCE_SEQ::PRODUCT + &
				IC_35BALANCE_SEQ::LOCATION + &
				IC_35BALANCE_SEQ::TRANSTYPE
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 480 IF ERR = 155%
			FILENAME$ = "IC_35BALANCE"
			CONTINUE HelpError
		END WHEN

		IC_35BALANCE::PBALANCE = IC_35BALANCE::PBALANCE + &
			IC_35BALANCE_SEQ::PBALANCE
		IC_35BALANCE::RBALANCE = IC_35BALANCE::RBALANCE + &
			(-IC_35BALANCE_SEQ::PBALANCE)

		WHEN ERROR IN
			UPDATE #IC_35BALANCE.CH%
		USE
			FILENAME$ = "IC_35BALANCE"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%

		GOTO 460

480		!
		! Can't find record; create new one
		!
		IC_35BALANCE = IC_35BALANCE_SEQ
		IC_35BALANCE::BBALANCE = 0.0
		IC_35BALANCE::RBALANCE = 0.0

		PUT #IC_35BALANCE.CH%
		RECORDS_ADD% = RECORDS_ADD% + 1%

		GOTO 460

485		CLOSE #IC_35BALANCE.CH%
		CLOSE #IC_35BALANCE.SEQ%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
			"Updated IC Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS_ADD%, "######## ") + &
			"Posted IC Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

555		YYYY$ = LEFT(ICPERIOD, 4%)
		CYCLE% = VAL%(RIGHT(ICPERIOD, 5%))

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "IC_35HISTORY_" + YYYY$ + &
			".HIS", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitKeyFunction
			END IF
		END IF

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.CRE"
		USE
			!
			! Cannot lock file
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		RECORDS%, RECORDS_ADD% = 0%

		!
		! Start at the beginning of the history file
		!
		WHEN ERROR IN
			RESET #IC_35HISTORY.SEQ%
		USE
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

560		WHEN ERROR IN
			GET #IC_35HISTORY.SEQ%
		USE
			CONTINUE 585 IF ERR = 11%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

570		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitKeyFunction
			END IF
		END IF

		WHEN ERROR IN
			GET #IC_35HISTORY.CH%, KEY #0% EQ &
				IC_35HISTORY_SEQ::PRODUCT + &
				IC_35HISTORY_SEQ::LOCATION + &
				IC_35HISTORY_SEQ::TRANSTYPE + &
				IC_35HISTORY_SEQ::CROSSREF + &
				IC_35HISTORY_SEQ::SUBACCT
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 580 IF ERR = 155%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_35HISTORY::PQUANTITY(CYCLE%) = &
			IC_35HISTORY::PQUANTITY(CYCLE%) + &
			IC_35HISTORY_SEQ::PQUANTITY(0%)
		IC_35HISTORY::PRICEAMT(CYCLE%) = &
			IC_35HISTORY::PRICEAMT(CYCLE%) + &
			IC_35HISTORY_SEQ::PRICEAMT(0%)
		IC_35HISTORY::COSTAMT(CYCLE%) = &
			IC_35HISTORY::COSTAMT(CYCLE%) + &
			IC_35HISTORY_SEQ::COSTAMT(0%)

		UPDATE #IC_35HISTORY.CH%

		RECORDS% = RECORDS% + 1%

		GOTO 560

580		!
		! Can't find record; create new one
		!
		IC_35HISTORY = IC_35HISTORY_SEQ
		FOR I% = 1% TO 12%
			IC_35HISTORY::PQUANTITY(I%) = 0.0
			IC_35HISTORY::PRICEAMT(I%) = 0.0
			IC_35HISTORY::COSTAMT(I%)  = 0.0
		NEXT I%

		IC_35HISTORY::PQUANTITY(CYCLE%) = IC_35HISTORY_SEQ::PQUANTITY(0%)
		IC_35HISTORY::PRICEAMT(CYCLE%) = IC_35HISTORY_SEQ::PRICEAMT(0%)
		IC_35HISTORY::COSTAMT(CYCLE%) = IC_35HISTORY_SEQ::COSTAMT(0%)

		IC_35HISTORY::PQUANTITY(0%)= 0.0
		IC_35HISTORY::PRICEAMT(0%) = 0.0
		IC_35HISTORY::COSTAMT(0%)  = 0.0

		PUT #IC_35HISTORY.CH%
		RECORDS_ADD% = RECORDS_ADD% + 1%

		GOTO 560

585		CLOSE #IC_35HISTORY.CH%
		CLOSE #IC_35HISTORY.SEQ%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
			"Updated IC Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS_ADD%, "######## ") + &
			"Posted IC Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:
	IC_TRAN_POST = EXIT_STATUS
	EXIT FUNCTION

 ExitKeyFunction:
	TEXT$ = "%Abort Key Typed"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	GOTO ExitFunction

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	TEXT$ = "%Untrapped Error (" + NUM1$(ERL) + ") " + ERT$(ERR)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), &
		UTL_REPORTX, TITLE(), 0%)

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
