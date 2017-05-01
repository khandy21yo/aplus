1	%TITLE "Accounts Payable Journal Posting Function"
	%SBTTL "AP_TRAN_POSTAP"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_TRAN_POSTAP( &
		LONG		OPT, &
		LONG		SUBOPT, &
		STRING		BATCH_NUMBER, &
		STRING		TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		AP_OPEN_CDD	AP_OPEN_POST, &
		AP_OPEN_DIST_CDD AP_OPEN_DIST_POST, &
		STRING		GLPERIOD)

	!
	! COPYRIGHT (C) 1989 BY
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
	!
	!	This function is used to post records in the
	!	Accounts Payable Purchasing and/or Cash Disbursements
	!	Journals to the AP Open Item and AP Open Distribution
	!	files.  It also will remove records from these AP_OPEN
	!	files if restarting an interrupted post.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_TRAN_POSTAP
	!	$ LIB FUNC_LIB:CMCFUN/REP AP_TRAN_POSTAP
	!	$ DELETE AP_TRAN_POSTAP.OBJ;*
	!
	! Author:
	!
	!	06/21/89 - Aaron Redd
	!
	! Modification history:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source code closer to 80 columns.
	!		Change RIGHT(NUM1$()) for FORMAT$().
	!
	!	08/29/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	08/01/2003 - Kevin Handy
	!		Change "TranExist" from fatal to warning.
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
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"

	!
	! Map statements
	!
	MAP (AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL
	MAP (AP_OPEN)		AP_OPEN_CDD		AP_OPEN
	MAP (AP_OPEN_DIST)	AP_OPEN_DIST_CDD	AP_OPEN_DIST
	MAP (DP_OUTP_XUNSOL)				RRR_FLAG%

	!
	! Common memory areas
	!
	COM (AP_TRAN_POSTAP.COM) AP_OPEN.CH%, &
		AP_OPEN_DIST.CH%, &
		AP_OPEN.SEQ%, &
		AP_OPEN_DIST.SEQ%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED

	!
	! Declare variables and/or constants
	!
	DECLARE	LONG		EXIT_STATUS

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

	%PAGE

	SELECT OPT

	!
	! Remove batch number
	!
	CASE OPT_RESTART

		!
		! Remove the posted records from the AP OPEN file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"AP_OPEN.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open AP_OPEN file
		!
100		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.CRE"
		USE
			FILENAME$ = "AP_OPEN"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Set record counter to zero
		!
		RECORDS% = 0%

		!
		! Do any AP_OPEN records have this batch number?
		!
105		WHEN ERROR IN
			FIND #AP_OPEN.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 120 IF ERR = 155%
			FILENAME$ = "AP_OPEN"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input at the beginning of the loop
		!
110		IF RRR_FLAG%
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
			GET #AP_OPEN.CH%
			GOTO 120 IF (AP_OPEN::BATCH <> BATCH_NUMBER)
			DELETE #AP_OPEN.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 120 IF ERR = 11%
			FILENAME$ = "AP_OPEN"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 110

		!
		! Close the cleaned-up AP_OPEN file
		!
120		CLOSE #AP_OPEN.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user just how many records we deleted
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Remove interrupted post records from AP_OPEN_DIST file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"AP_OPEN_DIST.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open AP_OPEN_DIST file
		!
150		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.CRE"
		USE
			FILENAME$ = "AP_OPEN_DIST"
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
155		WHEN ERROR IN
			FIND #AP_OPEN_DIST.CH%, KEY #1% EQ BATCH_NUMBER
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 170 IF ERR = 155%
			FILENAME$ = "AP_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input inside the loop
		!
160		IF RRR_FLAG%
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
			GET #AP_OPEN_DIST.CH%
			GOTO 170 IF (AP_OPEN_DIST::BTHNUM <> BATCH_NUMBER)
			DELETE #AP_OPEN_DIST.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 170 IF ERR = 11%
			FILENAME$ = "AP_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 160

		!
		! Now close the clean AP Open Dist file
		!
170		CLOSE #AP_OPEN_DIST.CH%

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
		! Get AP_CONTROL file info
		!
200		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
			GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
			CLOSE AP_CONTROL.CH%
		USE
			CONTINUE 210 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "AP_CONTROL"
			CONTINUE HelpError
		END WHEN

		IF GLPERIOD <= &
			(AP_CONTROL::YEAR + &
			FORMAT$(AP_CONTROL::LASTPERCLOSE, "<0>#"))
		THEN
			TEXT$ = SPACE$(18%) + "AP Period " + &
				LEFT(GLPERIOD, 4%) + "_" + &
				RIGHT(GLPERIOD, 5%) + " has been closed!"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

210		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
			GET #AP_OPEN.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 220 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "AP_OPEN"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

220		TEXT$ = ""	! Try to defeat error trap bug?

		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.OPN"
			GET #AP_OPEN_DIST.CH%, KEY #1% EQ BATCH_NUMBER
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 230 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "AP_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

230		CALL ASSG_CHANNEL(AP_OPEN.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "AP_OPEN.SEQ" AS FILE AP_OPEN.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP AP_OPEN, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "AP_OPEN.SEQ"
			CONTINUE HelpError
		END WHEN

240		CALL ASSG_CHANNEL(AP_OPEN_DIST.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "AP_OPEN_DIST.SEQ" AS FILE AP_OPEN_DIST.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP AP_OPEN_DIST, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "AP_OPEN_DIST.SEQ"
			CONTINUE HelpError
		END WHEN

	!
	! Create posting array
	!
	CASE OPT_ADDREC

		!
		! Check unsolicited input
		!
300		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		SELECT (SUBOPT AND 1023%)

		!
		! Put the AP Open posting record into the temporary file
		!
		CASE SUBOPT_REGISTER
			AP_OPEN = AP_OPEN_POST
			WHEN ERROR IN
				PUT #AP_OPEN.SEQ%
			USE
				FILENAME$ = "AP_OPEN.SEQ"
				CONTINUE HelpError
			END WHEN

		!
		! Put the AP Distribution posting record in the temporary file
		!
310		CASE SUBOPT_LINEITEM
			AP_OPEN_DIST = AP_OPEN_DIST_POST
			WHEN ERROR IN
				PUT #AP_OPEN_DIST.SEQ%
			USE
				FILENAME$ = "AP_OPEN_DIST.SEQ"
				CONTINUE HelpError
			END WHEN

		END SELECT

	CASE OPT_CONFIRM
		!
		! Not used in AP
		!

	CASE OPT_POSTFILE

		!
		! Post line items to AP_OPEN_DIST
		!
		GOTO LineItems &
			IF (SUBOPT AND 1023%) = SUBOPT_LINEITEM

		!
		! Get ready to put the temporary records into the AP Open file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"AP_OPEN.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open the AP Open item file
		!
400		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.CRE"
		USE
			FILENAME$ = "AP_OPEN"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Set the record counter to zero
		!
		RECORDS% = 0%

		!
		! Start at the beginning of the temporary file
		!
410		RESET #AP_OPEN.SEQ%

		!
		! Check unsolicited input as we loop through
		!
420		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		!
		! Get a record from the temporary file and put it in AP_OPEN
		!
		WHEN ERROR IN
			GET #AP_OPEN.SEQ%
			PUT #AP_OPEN.CH%
		USE
			CONTINUE 430 IF ERR = 11%
			FILENAME$ = "AP_OPEN"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 420

		!
		! Done; Close the files (killing the temporary)
		!
430		CLOSE #AP_OPEN.CH%
		CLOSE #AP_OPEN.SEQ%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user how many records we posted
		!
		TEXT$ = SPACE$(9%) + &
			FORMAT$(RECORDS%, "########") + " Posted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! We've had just too much fun
		!
		GOTO ExitFunction

 LineItems:
		!
		! Now get ready to put records into the AP Open Dist file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"AP_OPEN_DIST.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open the AP Open item file
		!
450		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.CRE"
		USE
			FILENAME$ = "AP_OPEN_DIST"
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
		! Start at the beginning of the temporary file
		!
460		RESET #AP_OPEN_DIST.SEQ%

		!
		! Check unsolicited input as we loop through
		!
470		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		!
		! Get a record from the temporary file and put it in AP_OPEN
		!
		WHEN ERROR IN
			GET #AP_OPEN_DIST.SEQ%
			PUT #AP_OPEN_DIST.CH%
		USE
			CONTINUE 480 IF ERR = 11%
			CONTINUE TranExist IF ERR = 134%
			FILENAME$ = "AP_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 470

		!
		! Done; Close the files (killing the temporary)
		!
480		CLOSE #AP_OPEN_DIST.CH%
		CLOSE #AP_OPEN_DIST.SEQ%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user we posted the records
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

	AP_TRAN_POSTAP = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 TranExist:
	TEXT$ = "%Transaction key " + AP_OPEN_DIST::TRANKEY + " already exist."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
 !	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

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
