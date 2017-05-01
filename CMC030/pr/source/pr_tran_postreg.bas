1	%TITLE "Payroll Posting Function"
	%SBTTL "PR_TRAN_POSTREG"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_TRAN_POSTREG(LONG OPT, &
		LONG		SUBOPT, &
		STRING		BATCH_NUMBER, &
		STRING		TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		PR_REG_ERNDED_CDD	PR_REG_ERNDED_POST, &
		PR_REG_TAXES_CDD PR_REG_TAXES_POST, &
		STRING		PRPERIOD)

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
	!	PR_REG_ERNDED_POST
	!	PR_REG_TAXES_POST
	!	PRPERIOD
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_TRAN_POSTREG
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_TRAN_POSTREG
	!	$ DELETE PR_TRAN_POSTREG.OBJ;*
	!
	! Author:
	!
	!	12/24/92 - Kevin Handy
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
	!		Update to V3.6 coding standards
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/22/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Map statements
	!
	MAP	(PR_REG_ERNDED)		PR_REG_ERNDED_CDD	PR_REG_ERNDED
	MAP	(PR_REG_TAXES)		PR_REG_TAXES_CDD	PR_REG_TAXES
	MAP	(DP_OUTP_XUNSOL)				RRR_FLAG%

	DECLARE PR_REG_TAXES_CDD PR_REG_TAXES_TEMP
	DECLARE PR_REG_ERNDED_CDD PR_REG_ERNDED_TEMP

	!
	! Common memory areas
	!
	COM (PR_TRAN_POSTREG.COM) &
		PR_REG_ERNDED.CH%, &
		PR_REG_TAXES.CH%, &
		PR_REG_ERNDED.SEQ%, &
		PR_REG_TAXES.SEQ%

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
	YYYY$ = LEFT(PRPERIOD, 4%)

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
			"PR_REG_ERNDED.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open PR_REG_ERNDED file
		!
1100 !		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.CRE"
 !
 !		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
 !			TITLE(), UTL_REPORTX, "", "", "", "")
 !
 !		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Do any PR_REG_ERNDED records have this batch number?
		!
1110 !		RESET #PR_REG_ERNDED.CH%

		!
		! Check unsolicited input at the beginning of the loop
		!
1120 !		IF RRR_FLAG%
 !		THEN
 !			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
 !			THEN
 !				EXIT_STATUS = CMC$_UNTERROR
 !				GOTO ExitFunction
 !			END IF
 !		END IF

		!
		! Get and delete all records with this batch number
		!
 !		GET #PR_REG_ERNDED.CH%

 !		IF (PR_REG_ERNDED::BATCH = BATCH_NUMBER)
 !		THEN
 !			DELETE #PR_REG_ERNDED.CH%
 !			RECORDS% = RECORDS% + 1%
 !		END IF

 !		GOTO 1120

		!
		! Close the cleaned-up PR_REG_ERNDED file
		!
1130 !		CLOSE #PR_REG_ERNDED.CH%
 !
 !		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
 !			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user just how many records we deleted
		!
 !		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Deleted Records"
 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Now remove interrupted post records from PR_REG_TAXES file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"PR_REG_TAXES.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open PR_REG_TAXES file
		!
1200 !		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.CRE"
 !
 !		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
 !			TITLE(), UTL_REPORTX, "", "", "", "")
 !
 !		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Reset the record counter to zero
		!
 !		RECORDS% = 0%

		!
		! Do any Open Distribution records also have this batch number?
		!
1210 !		RESET #PR_REG_TAXES.CH%

		!
		! Check unsolicited input inside the loop
		!
1220 !		IF RRR_FLAG%
 !		THEN
 !			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
 !			THEN
 !				EXIT_STATUS = CMC$_UNTERROR
 !				GOTO ExitFunction
 !			END IF
 !		END IF

		!
		! Grab and delete any Distribution records with this batch#
		!
 !		GET #PR_REG_TAXES.CH%
 !
 !		IF (PR_REG_TAXES::BATCH = BATCH_NUMBER)
 !		THEN
 !			DELETE #PR_REG_TAXES.CH%
 !			RECORDS% = RECORDS% + 1%
 !		END IF
 !
 !		GOTO 1220

		!
		! Now close the clean AR Open Dist file
		!
1230 !		CLOSE #PR_REG_TAXES.CH%
 !
 !		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
 !			TITLE(), UTL_REPORTX, "", "", "", "")
 !
		!
		! Print out the number of deleted records
		!
 !		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Deleted Records"
 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_CHECK

2100 !		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
 !		GET #PR_REG_ERNDED.CH%, KEY #2% EQ BATCH_NUMBER
 !
 !		EXIT_STATUS = CMC$_WARNING
 !		GOTO ExitFunction

2110 !		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
 !		GET #PR_REG_TAXES.CH%, KEY #3% EQ BATCH_NUMBER
 !
 !		EXIT_STATUS = CMC$_WARNING
 !		GOTO ExitFunction

2120 !
2200		CALL ASSG_CHANNEL(PR_REG_ERNDED.SEQ%, STAT%)

		WHEN ERROR IN
			OPEN "PR_REG_ERNDED.SEQ" AS FILE PR_REG_ERNDED.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP PR_REG_ERNDED, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "PR_REG_ERNDED"
			CONTINUE HelpError
		END WHEN

2210		CALL ASSG_CHANNEL(PR_REG_TAXES.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "PR_REG_TAXES.SEQ" AS FILE PR_REG_TAXES.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP PR_REG_TAXES, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "PR_REG_TAXES"
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
			PR_REG_ERNDED = PR_REG_ERNDED_POST
			WHEN ERROR IN
				PUT #PR_REG_ERNDED.SEQ%
			USE
				FILENAME$ = "PR_REG_ERNDED"
				CONTINUE HelpError
			END WHEN

		!
		! Put the AR Distribution posting record in the temporary file
		!
3010		CASE SUBOPT_LINEITEM
			PR_REG_TAXES = PR_REG_TAXES_POST
			WHEN ERROR IN
				PUT #PR_REG_TAXES.SEQ%
			USE
				FILENAME$ = "PR_REG_TAXES"
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
		! Post all records from the OPEN temp file to PR_REG_ERNDED.LED
		!
		CASE SUBOPT_REGISTER

			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, &
				"PR_REG_ERNDED.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Open item file
			!
4100			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.CRE"
			USE
				IF ERR = 138%	! Locked File
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "PR_REG_ERNDED"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4110			RESET #PR_REG_ERNDED.SEQ%

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
			! Get a record from the temporary file and put it in PR_REG_ERNDED
			!
			WHEN ERROR IN
				GET #PR_REG_ERNDED.SEQ%
			USE
				CONTINUE 4130 IF ERR = 11%
				FILENAME$ = "PR_REG_ERNDED"
				CONTINUE HelpError
			END WHEN

			PR_REG_ERNDED_TEMP = PR_REG_ERNDED

4122			WHEN ERROR IN
				GET #PR_REG_ERNDED.CH%, &
					KEY #0% EQ PR_REG_ERNDED_TEMP::EMPNUM + &
					PR_REG_ERNDED_TEMP::ETYPE + &
					PR_REG_ERNDED_TEMP::CODE
			USE
				IF ERR = 154%	! Locked Block
				THEN
					SLEEP 5%
					RETRY
				END IF

				CONTINUE 4124 IF ERR = 155%
				FILENAME$ = "PR_REG_ERNDED"
				CONTINUE HelpError
			END WHEN

			FOR I% = 0% TO 3%
				PR_REG_ERNDED::QTR_DOLL(I%) = &
					FUNC_ROUND(PR_REG_ERNDED::QTR_DOLL(I%) + &
					PR_REG_ERNDED_TEMP::QTR_DOLL(I%), 2%)
				PR_REG_ERNDED::REG_HRS(I%) = &
					FUNC_ROUND(PR_REG_ERNDED::REG_HRS(I%) + &
					PR_REG_ERNDED_TEMP::REG_HRS(I%), 2%)
				PR_REG_ERNDED::PRE_HRS(I%) = &
					FUNC_ROUND(PR_REG_ERNDED::PRE_HRS(I%) + &
					PR_REG_ERNDED_TEMP::PRE_HRS(I%), 2%)
				PR_REG_ERNDED::UNITS(I%) = &
					FUNC_ROUND(PR_REG_ERNDED::UNITS(I%) + &
					PR_REG_ERNDED_TEMP::UNITS(I%), 2%)
			NEXT I%

			UPDATE #PR_REG_ERNDED.CH%
			RECORDS1% = RECORDS1% + 1%
			GOTO 4120

4124			PUT #PR_REG_ERNDED.CH%
			RECORDS% = RECORDS% + 1%
			GOTO 4120

			!
			! Done; Close the files (killing the temporary)
			!
4130			CLOSE #PR_REG_ERNDED.CH%
			CLOSE #PR_REG_ERNDED.SEQ%

		!
		! Post all records from the Dist temp file to PR_REG_TAXES.LED
		!
		CASE SUBOPT_LINEITEM
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"PR_REG_TAXES.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Open Distribution file
			!
4200			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.CRE"
			USE
				IF ERR = 138%	! Locked File
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "PR_REG_TAXES"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4210			RESET #PR_REG_TAXES.SEQ%

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
			! Get a record from the temp and put it in PR_REG_TAXES
			!
			WHEN ERROR IN
				GET #PR_REG_TAXES.SEQ%
			USE
				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "PR_REG_TAXES"
				CONTINUE HelpError
			END WHEN

			PR_REG_TAXES_TEMP = PR_REG_TAXES

4222			WHEN ERROR IN
				GET #PR_REG_TAXES.CH%, &
					KEY #0% EQ PR_REG_TAXES_TEMP::EMPNUM + &
					PR_REG_TAXES_TEMP::TTYPE + &
					PR_REG_TAXES_TEMP::CODE
			USE
				IF ERR = 154%	! Locked Block
				THEN
					SLEEP 5%
					RETRY
				END IF

				CONTINUE 4224
			END WHEN

			FOR I% = 0% TO 3%
				PR_REG_TAXES::TAX(I%) = &
					FUNC_ROUND(PR_REG_TAXES::TAX(I%) + &
					PR_REG_TAXES_TEMP::TAX(I%), 2%)
				PR_REG_TAXES::REPORTABLE(I%) = &
					FUNC_ROUND(PR_REG_TAXES::REPORTABLE(I%) + &
					PR_REG_TAXES_TEMP::REPORTABLE(I%), 2%)
				PR_REG_TAXES::TAXABLE(I%) = &
					FUNC_ROUND(PR_REG_TAXES::TAXABLE(I%) + &
					PR_REG_TAXES_TEMP::TAXABLE(I%), 2%)
				PR_REG_TAXES::WKWRK(I%) = &
					PR_REG_TAXES::WKWRK(I%) + &
					PR_REG_TAXES_TEMP::WKWRK(I%)

			NEXT I%

			UPDATE #PR_REG_TAXES.CH%
			RECORDS1% = RECORDS1% + 1%
			GOTO 4220

4224			PUT #PR_REG_TAXES.CH%
			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #PR_REG_TAXES.CH%
			CLOSE #PR_REG_TAXES.SEQ%

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

	PR_TRAN_POSTREG = EXIT_STATUS
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
