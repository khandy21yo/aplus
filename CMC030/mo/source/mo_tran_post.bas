1	%TITLE "Manufacturing Sales Order Posting Function"
	%SBTTL "MO_TRAN_POST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_TRAN_POST(LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		MO_REGLINE_CDD MO_REGLINE_POST, &
		MO_REGLINEOPT_CDD MO_REGLINEOPT_POST)

	!
	! COPYRIGHT (C) 1990 BY
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
	!	Manufacturing Sales Order Register.
	!	MO_TRAN_POST also will remove records from
	!	these ledgers if restarting an interrupted post.
	!	.lm -5
	!
	! Index:
	!	.x Post>Manufacturing Sales Order
	!	.x Manufacturing Sales Order>Post
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
	!	MO_REGLINE_POST
	!	MO_REGLINEOPT_POST
	!	GLPERIOD
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_TRAN_POST
	!	$ LIB FUNC_LIB:CMCFUN/REP MO_TRAN_POST
	!	$ DELETE MO_TRAN_POST.OBJ;*
	!
	! Author:
	!
	!	03/28/91 - Val James Allen
	!
	! Modification history:
	!
	!	04/01/93 - Frank F. Starman
	!		Remove MO_REGHEADER
	!
	!	04/03/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/12/95 - Kevin Handy
	!		Allow non-existance of MO_CONTROL, when PS is
	!		posting, they may not be interfaces to MO.
	!
	!	07/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/28/99 - Kevin Handy
	!		Clean up source code
	!
	!	12/21/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[MO.OPEN]MO_CONTROL.HB"

	!
	! Map statements
	!
	MAP (MO_REGLINE)		MO_REGLINE_CDD		MO_REGLINE
	MAP (MO_REGLINEOPT)		MO_REGLINEOPT_CDD	MO_REGLINEOPT
	MAP (DP_OUTP_XUNSOL)		RRR_FLAG%
	MAP (MO_CONTROL)		MO_CONTROL_CDD		MO_CONTROL

	!
	! Common memory areas
	!
	COM (MO_TRAN_POST.COM) &
		MO_REGLINE.CH%, &
		MO_REGLINEOPT.CH%, &
		MO_REGLINE.SEQ%, &
		MO_REGLINEOPT.SEQ%

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
		! Now remove interrupted post records from MO_REGLINE file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"MO_REGLINE.HIS", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open MO_REGLINE file
		!
1300		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.CRE"
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 155%
			FILENAME$ = "MO_REGLINE"
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
			FIND #MO_REGLINE.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 155%
			FILENAME$ = "MO_REGLINE"
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
			GET #MO_REGLINE.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 11%
			FILENAME$ = "MO_REGLINE"
			CONTINUE HelpError
		END WHEN

		IF (MO_REGLINE::BATCH = BATCH_NUMBER)
		THEN
			WHEN ERROR IN
				DELETE #MO_REGLINE.CH%
			USE
				CONTINUE 1330 IF ERR = 11%
				FILENAME$ = "MO_REGLINE"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 1320
		END IF

		!
		! Now close the register line file
		!
1330		CLOSE #MO_REGLINE.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Now remove interrupted post records from MO_REGLINEOPT file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"MO_REGLINEOPT.HIS", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open MO_REGLINEOPT file
		!
1400		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.CRE"
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1430 IF ERR = 155%
			FILENAME$ = "MO_REGLINEOPT"
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
			FIND #MO_REGLINEOPT.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1430 IF ERR = 155%
			FILENAME$ = "MO_REGLINEOPT"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input inside the loop
		!
1420		IF RRR_FLAG%
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
			GET #MO_REGLINEOPT.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1430 IF ERR = 11%
			FILENAME$ = "MO_REGLINEOPT"
			CONTINUE HelpError
		END WHEN

		IF (MO_REGLINEOPT::BATCH = BATCH_NUMBER)
		THEN
			WHEN ERROR IN
				DELETE #MO_REGLINEOPT.CH%
			USE
				FILENAME$ = "MO_REGLINEOPT"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 1420
		END IF

		!
		! Now close the register line option file
		!
1430		CLOSE #MO_REGLINEOPT.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_CHECK
2000		!
		! Open OE control file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_CONTROL.OPN"
			GET #MO_CONTROL.CH%, RECORD 1%, REGARDLESS
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2110 IF ERR = 5%
			FILENAME$ = "MO_CONTROL"
			CONTINUE HelpError
		END WHEN

		CLOSE MO_CONTROL.CH%
		CALL ASSG_FREECHANNEL(MO_CONTROL.CH%)

		!
		! Check control flag
		!
		IF MO_CONTROL::STATUS_FLAG <> "0"
		THEN
			TEXT$ = SPACE$(18%) + &
				"The MO control file status flag is " + &
				MO_CONTROL::STATUS_FLAG + "."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

2110		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.OPN"
			GET #MO_REGLINE.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2120 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "MO_REGLINE"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2120		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.OPN"
			GET #MO_REGLINEOPT.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2210 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "MO_REGLINEOPT"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2210		CALL ASSG_CHANNEL(MO_REGLINE.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "MO_REGLINE.SEQ" AS FILE MO_REGLINE.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP MO_REGLINE, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "MO_REGLINE.SEQ"
			CONTINUE HelpError
		END WHEN

2220		CALL ASSG_CHANNEL(MO_REGLINEOPT.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "MO_REGLINEOPT.SEQ" AS FILE MO_REGLINEOPT.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP MO_REGLINEOPT, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "MO_REGLINEOPT.SEQ"
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
		! Put the Register line posting record in the temporary file
		!
		CASE SUBOPT_LINEITEM
			MO_REGLINE = MO_REGLINE_POST

3020			WHEN ERROR IN
				PUT #MO_REGLINE.SEQ%
			USE
				FILENAME$ = "MO_REGLINE.SEQ"
				CONTINUE HelpError
			END WHEN

		!
		! Put the Register line Option posting record in the temporary file
		!
		CASE SUBOPT_LINEITEMOPT
			MO_REGLINEOPT = MO_REGLINEOPT_POST

3030			WHEN ERROR IN
				PUT #MO_REGLINEOPT.SEQ%
			USE
				FILENAME$ = "MO_REGLINEOPT.SEQ"
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
		! Post all records to MO_REGLINE.HIS
		!
		CASE SUBOPT_LINEITEM
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"MO_REGLINE.HIS", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the Register line file
			!
4200			WHEN ERROR IN
				%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.CRE"
			USE
				IF ERR = 138%	! Locked File
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "MO_REGLINE"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
			RESET #MO_REGLINE.SEQ%

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
			! Get a record from the temp and put it in MO_REGLINEOPT
			!
			WHEN ERROR IN
				GET #MO_REGLINE.SEQ%
				PUT #MO_REGLINE.CH%
			USE
				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "MO_REGLINE"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #MO_REGLINE.CH%
			CLOSE #MO_REGLINE.SEQ%

		!
		! Post all records to MO_REGLINEOPT.HIS
		!
		CASE SUBOPT_LINEITEMOPT
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"MO_REGLINEOPT.HIS", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the Register line option file
			!
4300			WHEN ERROR IN
				%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.CRE"
			USE
				IF ERR = 138%	! Locked File
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "MO_REGLINEOPT"
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
				RESET #MO_REGLINEOPT.SEQ%
			USE
				FILENAME$ = "MO_REGLINEOPT"
				CONTINUE HelpError
			END WHEN

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
			! Get a record from the temp and put it in MO_REGLINEOPT
			!
			WHEN ERROR IN
				GET #MO_REGLINEOPT.SEQ%
				PUT #MO_REGLINEOPT.CH%
			USE
				CONTINUE 4330 IF ERR = 11%
				FILENAME$ = "MO_REGLINEOPT"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4320

			!
			! Done; Close the files (killing the temporary)
			!
4330			CLOSE #MO_REGLINEOPT.CH%
			CLOSE #MO_REGLINEOPT.SEQ%

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

	MO_TRAN_POST = EXIT_STATUS
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
	RESUME HelpError

	END FUNCTION
