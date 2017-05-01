1	%TITLE "Manufacturing WIP Order Posting Function"
	%SBTTL "WP_TRAN_POST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_TRAN_POST( &
		LONG		OPT, &
		LONG		SUBOPT, &
		STRING		BATCH_NUMBER, &
		STRING		TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		JC_JOB_CDD JC_JOB_POST, &
		WP_REGLINE_CDD WP_REGLINE_POST)

	!
	! COPYRIGHT (C) 1991 BY
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
	!	.b
	!	.lm +5
	!	This function is used to post records in the
	!	Manufacturing WIP Order Register.
	!	WP_TRAN_POST will also remove records from
	!	these ledgers if restarting an interrupted post.
	!	.lm -5
	!
	! Index:
	!	.x Post>Manufacturing WIP Order
	!	.x Manufacturing WIP Order>Post
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
	!	JC_JOB_POST
	!	WP_REGLINE_POST
	!	GLPERIOD
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_TRAN_POST
	!	$ LIB FUNC_LIB:CMCFUN/REP WP_TRAN_POST
	!	$ DELETE WP_TRAN_POST.OBJ;*
	!
	! Author:
	!
	!	05/29/91 - Val James Allen
	!
	! Modification history:
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/12/95 - Kevin Handy
	!		Modified to that it doesn't die when WP_CONTROL
	!		doesn't exist, since we may not be using WP in PS.
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/28/99 - Kevin Handy
	!		Clean up source code
	!
	!	04/26/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	MAP	(DP_OUTP_XUNSOL)	RRR_FLAG%

	!
	! Common memory areas
	!
	COM (WP_TRAN_POST.COM) &
		SB_SUBACCOUNT.CH%, &
		WP_REGLINE.CH%, &
		SB_SUBACCOUNT.SEQ%, &
		WP_REGLINE.SEQ%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED

	!
	! Declare variables and/or constants
	!
	DECLARE	LONG	EXIT_STATUS

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

	%PAGE

	SELECT OPT

	!
	! Remove batch number
	!
	CASE OPT_RESTART

		!
		! Now remove interrupted post records from SB_SUBACCOUNT file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "SB_SUBACCOUNT.HIS", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open SB_SUBACCOUNT file
		!
1200		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 155%
			FILENAME$ = "SB_SUBACCOUNT"
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
		! Do any records have this batch number?
		!
		WHEN ERROR IN
			RESET #SB_SUBACCOUNT.CH%
		USE
			CONTINUE 1230 IF ERR = 155%
			FILENAME$ = "SB_SUBACCOUNT"
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
			GET #SB_SUBACCOUNT.CH%
		USE
			IF ERR =  154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 11%
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN

		IF (JC_JOB::BATCH = BATCH_NUMBER)
		THEN
			WHEN ERROR IN
				DELETE #SB_SUBACCOUNT.CH%
			USE
				CONTINUE 1230 IF ERR = 11%
				FILENAME$ = "SB_SUBACCOUNT"
				CONTINUE HelpError
			END WHEN
			RECORDS% = RECORDS% + 1%
		END IF
		GOTO 1220

		!
		! Now close the register header file
		!
1230		CLOSE #SB_SUBACCOUNT.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Now remove interrupted post records from WP_REGLINE file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "WP_REGLINE.HIS", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open WP_REGLINE file
		!
1300		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.CRE"
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 155%
			FILENAME$ = "WP_REGLINE"
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
		WHEN ERROR IN
			FIND #WP_REGLINE.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR =  154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 155%
			FILENAME$ = "WP_REGLINE"
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
		! Grab and delete any Distribution records with this batch#
		!
		WHEN ERROR IN
			GET #WP_REGLINE.CH%
			GOTO 1330 IF (WP_REGLINE::BATCH <> BATCH_NUMBER)
			DELETE #WP_REGLINE.CH%
		USE
			IF ERR =  154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 11%
			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 1320

		!
		! Now close the register line file
		!
1330		CLOSE #WP_REGLINE.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_CHECK
2000		!
		! Open WP control file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.OPN"
			GET #WP_CONTROL.CH%, RECORD 1%, REGARDLESS
			CLOSE WP_CONTROL.CH%
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2110 IF ERR = 5%
			FILENAME$ = "WP_CONTROL"
			CONTINUE HelpError
		END WHEN

		CALL ASSG_FREECHANNEL(WP_CONTROL.CH%)

		!
		! Check control flag
		!
		IF WP_CONTROL::STATUS_FLAG <> "0"
		THEN
			TEXT$ = SPACE$(18%) + &
				"The WP control file status flag is " + &
				WP_CONTROL::STATUS_FLAG + "."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

2110		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"

			GET #WP_REGLINE.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			IF ERR =  154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 2200 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2200		CALL ASSG_CHANNEL(SB_SUBACCOUNT.SEQ%, STAT%)

		WHEN ERROR IN
			OPEN "SB_SUBACCOUNT.SEQ" AS FILE SB_SUBACCOUNT.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP SB_SUBACCOUNT, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "SB_SUBACCOUNT.SEQ"
			CONTINUE HelpError
		END WHEN

2210		CALL ASSG_CHANNEL(WP_REGLINE.SEQ%, STAT%)

		WHEN ERROR IN
			OPEN "WP_REGLINE.SEQ" AS FILE WP_REGLINE.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP WP_REGLINE, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "WP_REGLINE.SEQ"
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
		! Put the Register header posting record into the temp file
		!
		CASE SUBOPT_REGISTER
			JC_JOB = JC_JOB_POST

3010			WHEN ERROR IN
				PUT #SB_SUBACCOUNT.SEQ%
			USE
				FILENAME$ = "SB_SUBACCOUNT.SEQ"
				CONTINUE HelpError
			END WHEN

		!
		! Put the Register line posting record in the temporary file
		!
		CASE SUBOPT_LINEITEM
			WP_REGLINE = WP_REGLINE_POST

3020			WHEN ERROR IN
				PUT #WP_REGLINE.SEQ%
			USE
				FILENAME$ = "WP_REGLINE.SEQ"
				CONTINUE HelpError
			END WHEN

		END SELECT

	CASE OPT_CONFIRM
		!
		! Not used
		!

	CASE OPT_POSTFILE

		SELECT SUBOPT

		!
		! Post all records from the OPEN temp file to SB_SUBACCOUNT.HIS
		!
		CASE SUBOPT_REGISTER

			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, &
				"SB_SUBACCOUNT.HIS", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the Register header file
			!
4100			WHEN ERROR IN
				%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"
			USE
				IF ERR = 138%
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "SB_SUBACCOUNT"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
			WHEN ERROR IN
				RESET #SB_SUBACCOUNT.SEQ%
			USE
				FILENAME$ = "SB_SUBACCOUNT"
				CONTINUE HelpError
			END WHEN

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
			! Get a record from the temporary file
			! and put it in JC_JOB
			!
			WHEN ERROR IN
				GET #SB_SUBACCOUNT.SEQ%

				PUT #SB_SUBACCOUNT.CH%
			USE
				IF ERR =  154%
				THEN
					SLEEP 1%
					RETRY
				END IF

				CONTINUE 4120 IF ERR = 134%
				CONTINUE 4130 IF ERR = 11%
				FILENAME$ = "SB_SUBACCOUNT"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4120

			!
			! Done; Close the files (killing the temporary)
			!
4130			CLOSE #SB_SUBACCOUNT.CH%
			CLOSE #SB_SUBACCOUNT.SEQ%

		!
		! Post all records to WP_REGLINE.HIS
		!
		CASE SUBOPT_LINEITEM
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"WP_REGLINE.HIS", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the Register line file
			!
4200			WHEN ERROR IN
				%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.CRE"
			USE
				IF ERR = 138%
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "WP_REGLINE"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
			WHEN ERROR IN
				RESET #WP_REGLINE.SEQ%
			USE
				FILENAME$ = "WP_REGLINE"
				CONTINUE HelpError
			END WHEN

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

			WHEN ERROR IN
				GET #WP_REGLINE.SEQ%

				PUT #WP_REGLINE.CH%
			USE
				IF ERR =  154%
				THEN
					SLEEP 1%
					RETRY
				END IF

				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "WP_REGLINE"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #WP_REGLINE.CH%
			CLOSE #WP_REGLINE.SEQ%

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
	WP_TRAN_POST = EXIT_STATUS
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

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Handle the untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
