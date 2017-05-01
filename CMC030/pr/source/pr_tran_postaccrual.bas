1	%TITLE "Payroll Posting Function"
	%SBTTL "PR_TRAN_POSTACCRUAL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_TRAN_POSTACCRUAL(LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		PR_EMP_ACCRUAL_CDD PR_EMP_ACCRUAL_POST, &
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
	!	PR_EMP_ACCRUAL_POST
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
	!	$ BAS PR_SOURCE:PR_TRAN_POSTACCRUAL
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_TRAN_POSTACCRUAL
	!	$ DELETE PR_TRAN_POSTACCRUAL.OBJ;*
	!
	! Author:
	!
	!	12/23/92 - Kevin Handy
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
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
	%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.HB"
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Map statements
	!
	MAP	(PR_CONTROL)		PR_CONTROL_CDD		PR_CONTROL
	MAP	(PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL
	MAP	(DP_OUTP_XUNSOL)				RRR_FLAG%

	DECLARE PR_EMP_ACCRUAL_CDD PR_EMP_ACCRUAL_TEMP

	!
	! Common memory areas
	!
	COM (PR_TRAN_POSTACCRUAL.COM) &
		PR_EMP_ACCRUAL.CH%, &
		PR_EMP_ACCRUAL.SEQ%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION OUTP_UNSOLICITED

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
		! Remove the posted records from the AR OPEN file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"PR_EMP_ACCRUAL.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open PR_EMP_ACCRUAL file
		!
1100		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.CRE"
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "PR_EMP_ACCRUAL"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Close the cleaned-up PR_EMP_ACCRUAL file
		!
1130		CLOSE #PR_EMP_ACCRUAL.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

	CASE OPT_CHECK

2100 !		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
 !		GET #PR_EMP_ACCRUAL.CH%, KEY #2% EQ BATCH_NUMBER
 !
 !		EXIT_STATUS = CMC$_WARNING
 !		GOTO ExitFunction

2110	!

2200		CALL ASSG_CHANNEL(PR_EMP_ACCRUAL.SEQ%, STAT%)

		WHEN ERROR IN
			OPEN "PR_EMP_ACCRUAL.SEQ" AS FILE PR_EMP_ACCRUAL.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP PR_EMP_ACCRUAL, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "PR_EMP_ACCRUAL"
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

		!
		! Put the AR Open Register posting record into the temp file
		!
		PR_EMP_ACCRUAL = PR_EMP_ACCRUAL_POST
		WHEN ERROR IN
			PUT #PR_EMP_ACCRUAL.SEQ%
		USE
			FILENAME$ = "PR_EMP_ACCRUAL"
			CONTINUE HelpError
		END WHEN

	CASE OPT_CONFIRM
		!
		! Not used
		!

	CASE OPT_POSTFILE

		!
		! Get ready
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"PR_EMP_ACCRUAL.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open the AR Open item file
		!
4100		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.CRE"
		USE
			IF ERR = 138%	! Locked File
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "PR_EMP_ACCRUAL"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
			BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Start at the beginning of the temporary file
		!
4110		RESET #PR_EMP_ACCRUAL.SEQ%

		!
		! Check unsolicited input as we loop through
		!
4120		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		!
		! Get a record from the temporary file and put it in PR_EMP_ACCRUAL
		!
		WHEN ERROR IN
			GET #PR_EMP_ACCRUAL.SEQ%
		USE
			CONTINUE 4130 IF ERR = 11%
			FILENAME$ = "PR_EMP_ACCRUAL"
			CONTINUE HelpError
		END WHEN

		PR_EMP_ACCRUAL_TEMP = PR_EMP_ACCRUAL

4122		!
		! Find record in original file.  If we get any kind of an error
		! here, something is sooo verryyy wrong, that there is really
		! no possible source of recovery.
		!
		GET #PR_EMP_ACCRUAL.CH%, &
			KEY #0% EQ PR_EMP_ACCRUAL_TEMP::EMPNUM + &
			PR_EMP_ACCRUAL_TEMP::ATYPE

		PR_EMP_ACCRUAL::HOURSUNA = FUNC_ROUND(PR_EMP_ACCRUAL::HOURSUNA + &
			PR_EMP_ACCRUAL_TEMP::HOURSUNA, 2%)
		PR_EMP_ACCRUAL::HOURSAVA = FUNC_ROUND(PR_EMP_ACCRUAL::HOURSAVA + &
			PR_EMP_ACCRUAL_TEMP::HOURSAVA, 2%)
		PR_EMP_ACCRUAL::DOLLARUNA = FUNC_ROUND(PR_EMP_ACCRUAL::DOLLARUNA + &
			PR_EMP_ACCRUAL_TEMP::DOLLARUNA, 2%)
		PR_EMP_ACCRUAL::DOLLARAVA = FUNC_ROUND(PR_EMP_ACCRUAL::DOLLARAVA + &
			PR_EMP_ACCRUAL_TEMP::DOLLARAVA, 2%)
		PR_EMP_ACCRUAL::BATCH = PR_EMP_ACCRUAL_TEMP::BATCH

		UPDATE #PR_EMP_ACCRUAL.CH%
		RECORDS% = RECORDS% + 1%
		GOTO 4120

		!
		! Done; Close the files (killing the temporary)
		!
4130		CLOSE #PR_EMP_ACCRUAL.CH%
		CLOSE #PR_EMP_ACCRUAL.SEQ%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user how many records we posted
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Updated Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:

	PR_TRAN_POSTACCRUAL = EXIT_STATUS
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
