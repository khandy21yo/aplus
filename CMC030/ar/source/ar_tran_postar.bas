1	%TITLE "Accounts Receivable Journal Posting Function"
	%SBTTL "AR_TRAN_POSTAR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_TRAN_POSTAR(LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		AR_OPEN_CDD AR_OPEN_POST, &
		AR_OPEN_DIST_CDD AR_OPEN_DIST_POST, &
		AR_SALTAXLED_CDD AR_SALTAXLED_POST, &
		STRING GLPERIOD)

	!
	! COPYRIGHT (C) 1989 BY
	! Computer Management Center
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
	!	AR>POST
	!	POST>AR
	!	.b
	!	.lm +5
	!	This function is used to post records in the
	!	Accounts Receivable Sales and/or Cash Receipts Journals
	!	to the AR Open Item Ledger, AR Open Distribution Ledger,
	!	and possibly the AR Sales Tax Ledger.
	!	AR_TRAN_POSTAR also will remove records from
	!	these ledgers if restarting an interrupted post.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	OPT
	!	SUBOPT
	!	BATCH_NUMBER
	!	TITLE()
	!	UTL_REPORTX
	!	AR_OPEN_POST
	!	AR_OPEN_DIST_POST
	!	AR_SALTAXLED_POST
	!	GLPERIOD
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_TRAN_POSTAR
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_TRAN_POSTAR
	!	$ DELETE AR_TRAN_POSTAR.OBJ;*
	!
	! Author:
	!
	!	06/27/89 - Aaron Redd
	!
	! Modification history:
	!
	!	08/07/91 -  Frank F. Starman
	!		Change message if period has been closed.
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformt source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FOMAT$().
	!
	!	02/13/97 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Lose the business of posting to LB, since
	!		nobody is currently using it.
	!
	!	12/28/99 - Kevin Handy
	!		Clean up sourcve code
	!
	!	07/21/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Map statements
	!
	MAP	(AR_CONTROL)		AR_CONTROL_CDD		AR_CONTROL
	MAP	(AR_OPEN)		AR_OPEN_CDD		AR_OPEN
	MAP	(AR_OPEN_DIST)		AR_OPEN_DIST_CDD	AR_OPEN_DIST
	MAP	(AR_SALTAXLED)		AR_SALTAXLED_CDD	AR_SALTAXLED
	MAP	(DP_OUTP_XUNSOL)				RRR_FLAG%

	!
	! Common memory areas
	!
	COM (AR_TRAN_POSTAR.COM) AR_OPEN.CH%, &
		AR_OPEN_DIST.CH%, &
		AR_SALTAXLED.CH%, &
		AR_OPEN.SEQ%, &
		AR_OPEN_DIST.SEQ%, &
		AR_SALTAXLED.SEQ%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED

	!
	! Declare variables and/or constants
	!
	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Assume success (Dangerous as that might be)
	!
	EXIT_STATUS = CMC$_NORMAL

	!
	! Set the record counter to zero
	!
	RECORDS% = 0%


	SELECT OPT

	!
	! Remove batch number
	!
	CASE OPT_RESTART

		!
		! Remove the posted records from the AR OPEN file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"AR_OPEN.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open AR_OPEN file
		!
1100		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.CRE"
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Do any AR_OPEN records have this batch number?
		!
1110		WHEN ERROR IN
			FIND #AR_OPEN.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 154%	! Record Locked
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1130 IF ERR = 155%
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input at the beginning of the loop
		!
1120		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		!
		! Get and delete all records with this batch number
		!
		WHEN ERROR IN
			GET #AR_OPEN.CH%
			IF (AR_OPEN::BATCH = BATCH_NUMBER)
			THEN
				DELETE #AR_OPEN.CH%
				RECORDS% = RECORDS% + 1%
				GOTO 1120
			END IF
		USE
			IF ERR = 154%	! Record Locked
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1130 IF ERR = 11%
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

		!
		! Close the cleaned-up AR_OPEN file
		!
1130		CLOSE #AR_OPEN.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user just how many records we deleted
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Now remove interrupted post records from AR_OPEN_DIST file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"AR_OPEN_DIST.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open AR_OPEN_DIST file
		!
1200		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.CRE"
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "AR_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Reset the record counter to zero
		!
		RECORDS% = 0%

		!
		! Do any Open Distribution records also have this batch number?
		!
1210		WHEN ERROR IN
			FIND #AR_OPEN_DIST.CH%, KEY #3% EQ BATCH_NUMBER
		USE
			IF ERR = 154%	! Record Locked
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 155%
			FILENAME$ = "AR_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input inside the loop
		!
1220		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		!
		! Grab and delete any Distribution records with this batch#
		!
		WHEN ERROR IN
			GET #AR_OPEN_DIST.CH%
			IF (AR_OPEN_DIST::BATCH = BATCH_NUMBER)
			THEN
				DELETE #AR_OPEN_DIST.CH%
				RECORDS% = RECORDS% + 1%
				GOTO 1220
			END IF
		USE
			IF ERR = 154%	! Record Locked
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 11%
			FILENAME$ = "AR_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

		!
		! Now close the clean AR Open Dist file
		!
1230		CLOSE #AR_OPEN_DIST.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Finally, remove interrupted post records from AR_SALTAXLED
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"AR_SALTAXLED.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open AR_SALTAXLED file
		!
1300		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.CRE"
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "AR_SALTAXLED"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Reset the record counter to zero
		!
		RECORDS% = 0%

		!
		! Do any Sales Tax Ledger records also have this batch number?
		!
1310		WHEN ERROR IN
			FIND #AR_SALTAXLED.CH%, KEY #1% EQ BATCH_NUMBER
		USE
			CONTINUE 1330 IF ERR = 155%
			FILENAME$ = "AR_SALTAXLED"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input inside the loop
		!
1320		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		!
		! Search and destroy any SalTax records with this batch#
		! (POST Toasties)
		!
		WHEN ERROR IN
			GET #AR_SALTAXLED.CH%
			IF (AR_SALTAXLED::BATCH = BATCH_NUMBER)
			THEN
				DELETE #AR_SALTAXLED.CH%
				RECORDS% = RECORDS% + 1%
				GOTO 1320
			END IF
		USE
			IF ERR = 154%	! Record Locked
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 11%
			FILENAME$ = "AR_SALTAXLED"
			CONTINUE HelpError
		END WHEN

		!
		! Now close the clean AR Sales Tax Ledger
		!
1330		CLOSE #AR_SALTAXLED.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_CHECK

		!
		! Get AR_CONTROL file info
		!
2010		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
			GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "AR_CONTROL"
			CONTINUE HelpError
		END WHEN

		CLOSE AR_CONTROL.CH%

		IF GLPERIOD <= AR_CONTROL::YEAR + &
			FORMAT$(AR_CONTROL::LASTPERCLOSE, "<0>#")
		THEN
			TEXT$ = SPACE$(18%) + "AR " + GLPERIOD + &
				" has been closed"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), &
				TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

2100		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
			GET #AR_OPEN.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			IF ERR = 154%	! Record Locked
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 2110 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2110		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.OPN"
			GET #AR_OPEN_DIST.CH%, KEY #3% EQ BATCH_NUMBER
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			IF ERR = 154%	! Record Locked
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 2120 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "AR_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2120		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.OPN"
			GET #AR_SALTAXLED.CH%, KEY #1% EQ BATCH_NUMBER
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			IF ERR = 154%	! Record Locked
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 2200 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "AR_SALTAXLED"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2200		CALL ASSG_CHANNEL(AR_OPEN.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "AR_OPEN.SEQ" AS FILE AR_OPEN.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP AR_OPEN, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

2210		CALL ASSG_CHANNEL(AR_OPEN_DIST.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "AR_OPEN_DIST.SEQ" AS FILE AR_OPEN_DIST.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP AR_OPEN_DIST, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "AR_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

2220		CALL ASSG_CHANNEL(AR_SALTAXLED.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "AR_SALTAXLED.SEQ" AS FILE AR_SALTAXLED.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP AR_SALTAXLED, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "AR_SALTAXLED"
			CONTINUE HelpError
		END WHEN

	!
	! Create posting array
	!
	CASE OPT_ADDREC

		!
		! Check unsolicited input
		!
3000		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		SELECT (SUBOPT AND 1023%)

		!
		! Put the AR Open Register posting record into the temp file
		!
		CASE SUBOPT_REGISTER
			AR_OPEN = AR_OPEN_POST
			WHEN ERROR IN
				PUT #AR_OPEN.SEQ%
			USE
				FILENAME$ = "AR_OPEN"
				CONTINUE HelpError
			END WHEN
		!
		! Put the AR Distribution posting record in the temporary file
		!
3010		CASE SUBOPT_LINEITEM
			AR_OPEN_DIST = AR_OPEN_DIST_POST
			WHEN ERROR IN
				PUT #AR_OPEN_DIST.SEQ%
			USE
				FILENAME$ = "AR_OPEN_DIST"
				CONTINUE HelpError
			END WHEN

		!
		! Put the AR Sales Tax Ledger posting record in the temp file
		!
3020		CASE SUBOPT_LEDGER
			AR_SALTAXLED = AR_SALTAXLED_POST
			WHEN ERROR IN
				PUT #AR_SALTAXLED.SEQ%
			USE
				FILENAME$ = "AR_SALTAXLED"
				CONTINUE HelpError
			END WHEN

		END SELECT

	CASE OPT_CONFIRM
		!
		! Not used
		!

	CASE OPT_POSTFILE

		SELECT (SUBOPT AND 1023%)

		!
		! Post all records from the OPEN temp file to AR_OPEN.LED
		!
		CASE SUBOPT_REGISTER

			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, &
				"AR_OPEN.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Open item file
			!
4100			WHEN ERROR IN
				%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.CRE"
			USE
				IF ERR = 138%	! File Locked
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "AR_OPEN"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4110			RESET #AR_OPEN.SEQ%

			!
			! Check unsolicited input as we loop through
			!
4120			IF RRR_FLAG%
			THEN
				IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
				THEN
					EXIT_STATUS = CMC$_UNTERROR
					GOTO ExitFunction
				END IF
			END IF

			!
			! Get a record from the temporary file and put
			! it in AR_OPEN
			!
			WHEN ERROR IN
				GET #AR_OPEN.SEQ%
				PUT #AR_OPEN.CH%
			USE
				CONTINUE 4130 IF ERR = 11%
				FILENAME$ = "AR_OPEN"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4120

			!
			! Done; Close the files (killing the temporary)
			!
4130			CLOSE #AR_OPEN.CH%
			CLOSE #AR_OPEN.SEQ%

		!
		! Post all records from the Dist temp file to AR_OPEN_DIST.LED
		!
		CASE SUBOPT_LINEITEM
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"AR_OPEN_DIST.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Open Distribution file
			!
4200			WHEN ERROR IN
				%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.CRE"
			USE
				IF ERR = 138%	! File Locked
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "AR_OPEN_DIST"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4210			RESET #AR_OPEN_DIST.SEQ%

			!
			! Check unsolicited input as we loop through
			!
4220			IF RRR_FLAG%
			THEN
				IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
				THEN
					EXIT_STATUS = CMC$_UNTERROR
					GOTO ExitFunction
				END IF
			END IF

			!
			! Get a record from the temp and put it in AR_OPEN_DIST
			!
			WHEN ERROR IN
				GET #AR_OPEN_DIST.SEQ%
				PUT #AR_OPEN_DIST.CH%
			USE
				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "AR_OPEN_DIST"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #AR_OPEN_DIST.CH%
			CLOSE #AR_OPEN_DIST.SEQ%

		!
		! Post all records from the Sales Tax temp file to AR_SALTEXLED
		!
		CASE SUBOPT_LEDGER
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"AR_SALTAXLED.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Sales Tax Ledger
			!
4300			WHEN ERROR IN
				%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.CRE"
			USE
				IF ERR = 138%	! File Locked
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "AR_SALTAXLED"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4310			RESET #AR_SALTAXLED.SEQ%

			!
			! Check unsolicited input as we loop through
			!
4320			IF RRR_FLAG%
			THEN
				IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
				THEN
					EXIT_STATUS = CMC$_UNTERROR
					GOTO ExitFunction
				END IF
			END IF

			!
			! Get a record from the temporary file and put it in AR_OPEN
			!
			WHEN ERROR IN
				GET #AR_SALTAXLED.SEQ%
				PUT #AR_SALTAXLED.CH%
			USE
				CONTINUE 4330 IF ERR = 11%
				FILENAME$ = "AR_SALTAXLED"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4320

			!
			! Done; Close the files (killing the temporary)
			!
4330			CLOSE #AR_SALTAXLED.CH%
			CLOSE #AR_SALTAXLED.SEQ%

		END SELECT

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user how many records we posted
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Posted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:

	AR_TRAN_POSTAR = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), &
		UTL_REPORTX, TITLE(), 0%)

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Handle the untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
