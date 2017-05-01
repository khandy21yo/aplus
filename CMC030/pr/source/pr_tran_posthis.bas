1	%TITLE "Payroll Posting Function"
	%SBTTL "PR_TRAN_POSTHIS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_TRAN_POSTHIS(LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		PR_HIS_PAY_CDD PR_HIS_PAY_POST, &
		PR_HIS_DED_CDD PR_HIS_DED_POST, &
		PR_HIS_CHECK_CDD PR_HIS_CHECK_POST, &
		STRING PRPERIOD)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	PR_HIS_PAY_POST
	!	PR_HIS_DED_POST
	!	PR_HIS_CHECK_POST
	!	PRPERIOD
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_TRAN_POSTHIS
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_TRAN_POSTHIS
	!	$ DELETE PR_TRAN_POSTHIS.OBJ;*
	!
	! Author:
	!
	!	12/15/92 - Kevin Handy
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Removed code for PR_CONTROL, which isn't used in
	!		this function.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/20/95 - Kevin Handy
	!		Don't update check number if one already exists.
	!
	!	08/01/95 - Kevin Handy
	!		Fix bug with updating check numbers (04/20/95).
	!
	!	10/25/96 - Kevin handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/05/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.HB"
	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.HB"
	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Map statements
	!
	MAP	(PR_HIS_PAY)		PR_HIS_PAY_CDD		PR_HIS_PAY
	MAP	(PR_HIS_DED)		PR_HIS_DED_CDD		PR_HIS_DED
	MAP	(PR_HIS_CHECK)		PR_HIS_CHECK_CDD	PR_HIS_CHECK
	MAP	(DP_OUTP_XUNSOL)				RRR_FLAG%

	DECLARE PR_HIS_CHECK_CDD PR_HIS_CHECK_TEMP

	!
	! Common memory areas
	!
	COM (PR_TRAN_POSTHIS.COM) &
		PR_HIS_PAY.CH%, &
		PR_HIS_DED.CH%, &
		PR_HIS_CHECK.CH%, &
		PR_HIS_PAY.SEQ%, &
		PR_HIS_DED.SEQ%, &
		PR_HIS_CHECK.SEQ%

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
	RECORDS1% = 0%
	BATCH_NO$ = PRPERIOD

	%PAGE

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
			"PR_HIS_PAY.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open PR_HIS_PAY file
		!
1100		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.CRE"
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "PR_HIS_PAY"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Do any PR_HIS_PAY records have this batch number?
		!
1110		WHEN ERROR IN
			RESET #PR_HIS_PAY.CH%
		USE
			CONTINUE 1130 IF ERR = 155%
			FILENAME$ = "PR_HIS_PAY"
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
			GET #PR_HIS_PAY.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1130 IF ERR = 11%
			FILENAME$ = "PR_HIS_PAY"
			CONTINUE HelpError
		END WHEN

		IF (PR_HIS_PAY::BATCH = BATCH_NUMBER)
		THEN
			WHEN ERROR IN
				DELETE #PR_HIS_PAY.CH%
			USE
				FILENAME$ = "PR_HIS_PAY"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
		END IF

		GOTO 1120

		!
		! Close the cleaned-up PR_HIS_PAY file
		!
1130		CLOSE #PR_HIS_PAY.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user just how many records we deleted
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Now remove interrupted post records from PR_HIS_DED file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"PR_HIS_DED.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open PR_HIS_DED file
		!
1200		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.CRE"
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "PR_HIS_DED"
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
			RESET #PR_HIS_DED.CH%
		USE
			CONTINUE 1230 IF ERR = 155%
			FILENAME$ = "PR_HIS_DED"
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
			GET #PR_HIS_DED.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 11%
			FILENAME$ = "PR_HIS_DED"
			CONTINUE HelpError
		END WHEN

		IF (PR_HIS_DED::BATCH = BATCH_NUMBER)
		THEN
			WHEN ERROR IN
				DELETE #PR_HIS_DED.CH%
			USE
				FILENAME$ = "PR_HIS_DED"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
		END IF

		GOTO 1220

		!
		! Now close the clean AR Open Dist file
		!
1230		CLOSE #PR_HIS_DED.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Finally, remove interrupted post records from PR_HIS_CHECK
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"PR_HIS_CHECK.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open PR_HIS_CHECK file
		!
1300		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.CRE"
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "PR_HIS_CHECK"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Do any Sales Tax Ledger records also have this batch number?
		!
1310 !

		!
		! Check unsolicited input inside the loop
		!
1320 !

		!
		! Now close the clean AR Sales Tax Ledger
		!
1330		CLOSE #PR_HIS_CHECK.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_CHECK

2100 !

2110 !

2120 !

2200		CALL ASSG_CHANNEL(PR_HIS_PAY.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "PR_HIS_PAY.SEQ" AS FILE PR_HIS_PAY.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP PR_HIS_PAY, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "PR_HIS_PAY"
			CONTINUE HelpError
		END WHEN

2210		CALL ASSG_CHANNEL(PR_HIS_DED.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "PR_HIS_DED.SEQ" AS FILE PR_HIS_DED.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP PR_HIS_DED, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "PR_HIS_DED"
			CONTINUE HelpError
		END WHEN

2220		CALL ASSG_CHANNEL(PR_HIS_CHECK.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "PR_HIS_CHECK.SEQ" AS FILE PR_HIS_CHECK.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP PR_HIS_CHECK, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
			USE
				FILENAME$ = "PR_HIS_CHECK"
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
			PR_HIS_PAY = PR_HIS_PAY_POST
			WHEN ERROR IN
				PUT #PR_HIS_PAY.SEQ%
			USE
				FILENAME$ = "PR_HIS_PAY"
				CONTINUE HelpError
			END WHEN

		!
		! Put the AR Distribution posting record in the temporary file
		!
3010		CASE SUBOPT_LINEITEM
			PR_HIS_DED = PR_HIS_DED_POST
			WHEN ERROR IN
				PUT #PR_HIS_DED.SEQ%
			USE
				FILENAME$ = "PR_HIS_DED"
				CONTINUE HelpError
			END WHEN

		!
		! Put the AR Sales Tax Ledger posting record in the temp file
		!
3020		CASE SUBOPT_LEDGER
			PR_HIS_CHECK = PR_HIS_CHECK_POST
			WHEN ERROR IN
				PUT #PR_HIS_CHECK.SEQ%
			USE
				FILENAME$ = "PR_HIS_CHECK"
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
		! Post all records from the OPEN temp file to PR_HIS_PAY.LED
		!
		CASE SUBOPT_REGISTER

			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, &
				"PR_HIS_PAY.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Open item file
			!
4100			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.CRE"
			USE
				IF ERR = 138%	! Locked File
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "PR_HIS_PAY"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4110			RESET #PR_HIS_PAY.SEQ%

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
			! Get a record from the temporary file and put it in PR_HIS_PAY
			!
			WHEN ERROR IN
				GET #PR_HIS_PAY.SEQ%
				PUT #PR_HIS_PAY.CH%
			USE
				CONTINUE 4130 IF ERR = 11%
				FILENAME$ = "PR_HIS_PAY"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4120

			!
			! Done; Close the files (killing the temporary)
			!
4130			CLOSE #PR_HIS_PAY.CH%
			CLOSE #PR_HIS_PAY.SEQ%

		!
		! Post all records from the Dist temp file to PR_HIS_DED.LED
		!
		CASE SUBOPT_LINEITEM
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"PR_HIS_DED.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Open Distribution file
			!
4200			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.CRE"
			USE
				IF ERR = 138%	! Locked File
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "PR_HIS_DED"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4210			RESET #PR_HIS_DED.SEQ%

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
			! Get a record from the temp and put it in PR_HIS_DED
			!
			WHEN ERROR IN
				GET #PR_HIS_DED.SEQ%
				PUT #PR_HIS_DED.CH%
			USE
				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "PR_HIS_DED"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #PR_HIS_DED.CH%
			CLOSE #PR_HIS_DED.SEQ%

		!
		! Post all records from the Sales Tax temp file to PR_SALTEXLED
		!
		CASE SUBOPT_LEDGER
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"PR_HIS_CHECK.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Sales Tax Ledger
			!
4300			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.CRE"
			USE
				IF ERR = 138%	! Locked File
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "PR_HIS_CHECK"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4310			RESET #PR_HIS_CHECK.SEQ%

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
			! Get a record from the temporary file and put it in PR_HIS_PAY
			!
			WHEN ERROR IN
				GET #PR_HIS_CHECK.SEQ%
			USE
				CONTINUE 4330 IF ERR = 11%
				FILENAME$ = "PR_HIS_CHECK"
				CONTINUE HelpError
			END WHEN

			PR_HIS_CHECK_TEMP = PR_HIS_CHECK

4322			WHEN ERROR IN
				GET #PR_HIS_CHECK.CH%, &
					KEY #0% EQ PR_HIS_CHECK_TEMP::EMPNUM + &
					PR_HIS_CHECK::PR_END_DATE
			USE
				IF ERR = 154%	! Locked Block
				THEN
					SLEEP 5%
					RETRY
				END IF
				CONTINUE 4324
			END WHEN

			IF TRM$(PR_HIS_CHECK::CHECK) = ""
			THEN
				PR_HIS_CHECK::CHECK = PR_HIS_CHECK_TEMP::CHECK
				PR_HIS_CHECK::CHECK_DATE = PR_HIS_CHECK_TEMP::CHECK_DATE
				PR_HIS_CHECK::UPDATE_FLAG = PR_HIS_CHECK_TEMP::UPDATE_FLAG
				PR_HIS_CHECK::BATCH = PR_HIS_CHECK_TEMP::BATCH
				UPDATE #PR_HIS_CHECK.CH%
				RECORDS1% = RECORDS1% + 1%
			END IF

			GOTO 4320

4324			PUT #PR_HIS_CHECK.CH%
			RECORDS% = RECORDS% + 1%
			GOTO 4320

			!
			! Done; Close the files (killing the temporary)
			!
4330			CLOSE #PR_HIS_CHECK.CH%
			CLOSE #PR_HIS_CHECK.SEQ%

		END SELECT

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user how many records we posted
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS1%, "########") + " Updated Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Posted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:

	PR_TRAN_POSTHIS = EXIT_STATUS
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
