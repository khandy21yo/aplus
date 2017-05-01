1	%TITLE "Requisition Posting Function"
	%SBTTL "WP_TRAN_POSTREQ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_TRAN_POSTREQ(LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		WP_REQREGISTER_CDD WP_REQREGISTER_POST)

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
	!	Manufacturing WIP Requistion Register.
	!	WP_TRAN_POSTREQ also will remove records from
	!	these ledgers if restarting an interrupted post.
	!	.lm -5
	!
	! Index:
	!	.x Post>Manufacturing WIP Requistion
	!	.x Manufacturing WIP Requisition>Post
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
	!	WP_REQREGISTER_POST
	!	GLPERIOD
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_TRAN_POSTREQ
	!	$ LIB FUNC_LIB:CMCFUN/REP WP_TRAN_POSTREQ
	!	$ DELETE WP_TRAN_POSTREQ.OBJ;*
	!
	! Author:
	!
	!	07/30/91 - Val James "I'm outa here" Allen
	!
	! Modification history:
	!
	!	03/24/93 - Frank F. Starman
	!		Check for invalid requisition number.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/12/95 - Kevin Handy
	!		Don't require existance of WP_CONTROL, because
	!		PS may not interface to WP.
	!
	!	10/31/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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
	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"

	!
	! Map statements
	!
	MAP	(WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	MAP	(DP_OUTP_XUNSOL)	RRR_FLAG%
	MAP	(WP_CONTROL)		WP_CONTROL_CDD		WP_CONTROL

	!
	! Common memory areas
	!
	COM (WP_TRAN_POSTREQ.COM) &
		WP_REQREGISTER.CH%, &
		WP_REQREGISTER.SEQ%

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
		! Now remove interrupted post records from WP_REQREGISTER file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"WP_REQREGISTER.HIS", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open WP_REQREGISTER file
		!
1300		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.CRE"
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "WP_REQREGISTER"
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
			FIND #WP_REQREGISTER.CH%, KEY #3% EQ BATCH_NUMBER
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 155%
			FILENAME$ = "WP_REQREGISTER"
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
			GET #WP_REQREGISTER.CH%
			GOTO 1330 IF (WP_REQREGISTER::BATCH <> BATCH_NUMBER)
			DELETE #WP_REQREGISTER.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 11%
			FILENAME$ = "WP_REQREGISTER"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 1320

		!
		! Now close the register line file
		!
1330		CLOSE #WP_REQREGISTER.CH%

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
			TEXT$ = SPACE$(18%) + "The WP control file status flag" + &
				" is " + WP_CONTROL::STATUS_FLAG + "."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

2110		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
			GET #WP_REQREGISTER.CH%, KEY #3% EQ BATCH_NUMBER
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 2210 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "WP_REQREGISTER"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2210		CALL ASSG_CHANNEL(WP_REQREGISTER.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "WP_REQREGISTER.SEQ" AS FILE WP_REQREGISTER.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP WP_REQREGISTER, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "WP_REQREGISTER.SEQ"
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

			IF WP_REQREGISTER::RECTYP   = "01" AND &
				(WP_REQREGISTER::REQNUM= "" AND &
				WP_REQREGISTER::REQLIN= "0000")
			THEN
				TEXT$ = SPACE$(18%) + WP_REQREGISTER::JOB + " " + &
					WP_REQREGISTER::LLINE + &
					" has blank requisition #"
				CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF

			WP_REQREGISTER = WP_REQREGISTER_POST

3020			WHEN ERROR IN
				PUT #WP_REQREGISTER.SEQ%
			USE
				FILENAME$ = "WP_REQREGISTER.SEQ"
				CONTINUE HelpError
			END WHEN

		END SELECT

	CASE OPT_CONFIRM
		!
		! Not used
		!

	CASE OPT_POSTFILE

		SELECT SUBOPT

		CASE SUBOPT_LINEITEM
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"WP_REQREGISTER.HIS", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the Register line file
			!
4200			WHEN ERROR IN
				%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.CRE"
			USE
				IF ERR = 138%
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "WP_REQREGISTER"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
			RESET #WP_REQREGISTER.SEQ%

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
				GET #WP_REQREGISTER.SEQ%
				PUT #WP_REQREGISTER.CH%
			USE
				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "WP_REQREGISTER"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #WP_REQREGISTER.CH%
			CLOSE #WP_REQREGISTER.SEQ%

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

	WP_TRAN_POSTREQ = EXIT_STATUS
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
