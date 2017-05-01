1	%TITLE "Purchase Order Posting Function"
	%SBTTL "PO_TRAN_POSTPO"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_TRAN_POSTPO(LONG OPT, LONG SUBOPT, &
		STRING BATCH_NUMBER, STRING TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		PO_REG_LINE_CDD	PO_REG_LINE_POST, &
		PO_REG_SUB_LINE_CDD PO_REG_SUB_LINE_POST)

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
	!	PO>POST
	!	POST>PO
	!	.b
	!	.lm +5
	!	This function is used to post records in the
	!	PO Journals to the PO Register.
	!	PO_TRAN_POSTPO also will remove records from
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
	!	PO_REG_LINE_POST
	!	PO_REG_SUB_LINE_POST
	!	GLPERIOD
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_TRAN_POSTPO
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_TRAN_POSTPO
	!	$ DELETE PO_TRAN_POSTPO.OBJ;*
	!
	! Author:
	!
	!	07/18/90 - Kevin Handy
	!
	! Modification history:
	!
	!	03/04/92 - Dan Perkins
	!		Changed key number in line 2100 form KEY #2 to
	!		KEY #3 resulting from changes in file opens.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/25/92 - Kevin Handy
	!		Added more error messages in an attempt to lose
	!		"Post Aborted" message without any reasons,
	!		which Frank prefers.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/21/96 - Kevin Handy
	!		Reformat source code
	!
	!	11/27/96 - Kevin Handy
	!		Lose several gratituous %PAGE directives
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Lose LB includes, which were never used.
	!
	!	12/19/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Map statements
	!
	MAP (PO_REG_LINE)		PO_REG_LINE_CDD		PO_REG_LINE
	MAP (PO_REG_SUB_LINE)		PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE
	MAP (DP_OUTP_XUNSOL)					RRR_FLAG%

	!
	! Common memory areas
	!
	COM (PO_TRAN_POSTPO.COM) &
		PO_REG_LINE.CH%, &
		PO_REG_SUB_LINE.CH%, &
		PO_REG_LINE.SEQ%, &
		PO_REG_SUB_LINE.SEQ%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED

	!
	! Declare variables and/or constants
	!
	DECLARE	LONG EXIT_STATUS

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
		! Now remove interrupted post records from PO_REG_SUB_LINE file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"PO_REG_SUB_LINE.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open PO_REG_SUB_LINE file
		!
1200		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.CRE"
		USE
			FILENAME$ = "PO_REG_SUB_LINE"
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
			FIND #PO_REG_SUB_LINE.CH%, KEY #1% EQ BATCH_NUMBER
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 155%
			FILENAME$ = "PO_REG_SUB_LINE"
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
			GET #PO_REG_SUB_LINE.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 11%
			FILENAME$ = "PO_REG_SUB_LINE"
			CONTINUE HelpError
		END WHEN

		GOTO 1230 IF (PO_REG_SUB_LINE::BATCH <> BATCH_NUMBER)

		WHEN ERROR IN
			DELETE #PO_REG_SUB_LINE.CH%
		USE
			FILENAME$ = "PO_REG_SUB_LINE"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 1220

		!
		! Now close the clean PO Open Dist file
		!
1230		CLOSE #PO_REG_SUB_LINE.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted PO Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_CHECK

2100		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
			GET #PO_REG_LINE.CH%, KEY #3% EQ BATCH_NUMBER
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2110 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "PO_REG_LINE"
			CONTINUE HelpError
		END WHEN

		TEXT$ = "%Batch number already exists in PO_REG_LINE file"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2110		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"
			GET #PO_REG_SUB_LINE.CH%, KEY #1% EQ BATCH_NUMBER
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2120 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "PO_REG_SUB_LINE"
			CONTINUE HelpError
		END WHEN

		TEXT$ = "%Batch number already exists in PO_REG_SUB_LINE file"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2120	!

2200		CALL ASSG_CHANNEL(PO_REG_LINE.SEQ%, STAT%)

		WHEN ERROR IN
			OPEN "PO_REG_LINE.SEQ" AS FILE PO_REG_LINE.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP PO_REG_LINE, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "PO_REG_LINE.SEQ"
			CONTINUE HelpError
		END WHEN

2210		CALL ASSG_CHANNEL(PO_REG_SUB_LINE.SEQ%, STAT%)

		WHEN ERROR IN
			OPEN "PO_REG_SUB_LINE.SEQ" AS FILE PO_REG_SUB_LINE.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP PO_REG_SUB_LINE, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "PO_REG_SUB_LINE.SEQ"
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
		! Put the PO LINE Register posting record into the temp file
		!
		CASE SUBOPT_REGISTER
			PO_REG_LINE = PO_REG_LINE_POST

			WHEN ERROR IN
				PUT #PO_REG_LINE.SEQ%
			USE
				FILENAME$ = "OPT_POST"
				CONTINUE HelpError
			END WHEN

		!
		! Put the PO SUBLINE posting record in the temporary file
		!
		CASE SUBOPT_LINEITEM
			PO_REG_SUB_LINE = PO_REG_SUB_LINE_POST
			PUT #PO_REG_SUB_LINE.SEQ%

		END SELECT

	CASE OPT_CONFIRM
		!
		! Not used
		!

	CASE OPT_POSTFILE

		SELECT (SUBOPT AND 1023%)

		!
		! Post all records from the OPEN temp file to PO_REG_LINE.LED
		!
		CASE SUBOPT_REGISTER

			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, &
				"PO_REG_LINE.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the PO LINE item file
			!
4100			WHEN ERROR IN
				%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.CRE"
			USE
				FILENAME$ = "PO_REG_LINE"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4110			RESET #PO_REG_LINE.SEQ%

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
			! Get a record from the temporary file and put it
			! in PO_REG_LINE
			!
			WHEN ERROR IN
				GET #PO_REG_LINE.SEQ%
			USE
				CONTINUE 4130 IF ERR = 11%
				CONTINUE 4120 IF ERR = 134%
				FILENAME$ = "PO_REG_LINE"
				CONTINUE HelpError
			END WHEN

			!
			! NOTE: A duplicate record error here just causes
			! that record to be skipped.  It should really try
			! and see if it can pull any new and usefull information
			! from the two records.
			!
			WHEN ERROR IN
				PUT #PO_REG_LINE.CH%
			USE
				CONTINUE 4120 IF ERR = 134%
				FILENAME$ = "PO_REG_LINE"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4120

			!
			! Done; Close the files (killing the temporary)
			!
4130			CLOSE #PO_REG_LINE.CH%
			CLOSE #PO_REG_LINE.SEQ%

		!
		! Post all records from the Dist temp file
		! to PO_REG_SUB_LINE.LED
		!
		CASE SUBOPT_LINEITEM
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"PO_REG_SUB_LINE.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the PO SUBLINE Distribution file
			!
4200			WHEN ERROR IN
				%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.CRE"
			USE
				FILENAME$ = "PO_REG_SUB_LINE"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4210			RESET #PO_REG_SUB_LINE.SEQ%

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
			! Get a record from the temp and put it in
			! PO_REG_SUB_LINE
			!
			WHEN ERROR IN
				GET #PO_REG_SUB_LINE.SEQ%
				PUT #PO_REG_SUB_LINE.CH%
			USE
				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "PO_REG_SUB_LINE"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #PO_REG_SUB_LINE.CH%
			CLOSE #PO_REG_SUB_LINE.SEQ%

		END SELECT

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user how many records we posted
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Posted PO Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:

	PO_TRAN_POSTPO = EXIT_STATUS
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

32000	!******************************************************************
	! End of function PO_TRAN_POSTPO
	!******************************************************************
	END FUNCTION
