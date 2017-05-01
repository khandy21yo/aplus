1	%TITLE "Pacific Pride Daily Transaction Posting Function"
	%SBTTL "PP_TRAN_POST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG	PP_TRAN_POST( &
		LONG		OPT, &
		STRING		BATCH_NUMBER, &
		STRING		TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		PP_MONTHLY_CDD	PP_MONTHLY_POST, &
		STRING		YYYY_PP)

	!
	! COPYRIGHT (C) 1993 BY
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
	!	Daily Transaction Journal.
	!	PP__TRAN__POST also will remove records from
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
	!	BATCH_NUMBER
	!	TITLE()
	!	UTL_REPORTX
	!	PP_MONTHLY_POST
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_TRAN_POST
	!	$ LIB FUNC_LIB:CMCFUN/REP PP_TRAN_POST
	!	$ DELETE PP_TRAN_POST.OBJ;*
	!
	! Author:
	!
	!	01/13/93 - Dan Perkins
	!
	! Modification history:
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/20/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.HB"

	!
	! Map statements
	!
	MAP (DP_OUTP_XUNSOL)	RRR_FLAG%
	MAP (PP_MONTHLY)	PP_MONTHLY_CDD		PP_MONTHLY

	!
	! Common memory areas
	!
	COM (PP_TRAN_POST.COM)	PP_MONTHLY.CH%, &
				PP_MONTHLY.SEQ%

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
		! Now remove interrupted post records from PP_MONTHLY file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "PP_MONTHLY.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open PP_MONTHLY file
		!
1200		YYYY_PP$ = YYYY_PP
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.CRE"
		USE
			CONTINUE 1230 IF ERR = 155%
			FILENAME$ = "PP_MONTHLY"
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
			FIND #PP_MONTHLY.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 155%
			FILENAME$ = "PP_MONTHLY"
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
			GET #PP_MONTHLY.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 11%
			FILENAME$ = "PP_MONTHLY"
			CONTINUE HelpError
		END WHEN

		GOTO 1230 IF PP_MONTHLY::BATCH <> BATCH_NUMBER

		WHEN ERROR IN
			DELETE #PP_MONTHLY.CH%
		USE
			FILENAME$ = "PP_MONTHLY"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 1220

		!
		! Now close the monthly file
		!
1230		CLOSE #PP_MONTHLY.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL


	CASE OPT_CHECK

2100		YYYY_PP$ = YYYY_PP

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.OPN"

			GET #PP_MONTHLY.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2200 IF ERR = 155% OR ERR = 5%
			FILENAME$ = "PP_MONTHLY"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2200		CALL ASSG_CHANNEL(PP_MONTHLY.SEQ%, STAT%)

		WHEN ERROR IN
			OPEN "PP_MONTHLY.SEQ" AS FILE PP_MONTHLY.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP PP_MONTHLY, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "PP_MONTHLY.SEQ"
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

		PP_MONTHLY = PP_MONTHLY_POST

3010		WHEN ERROR IN
			PUT #PP_MONTHLY.SEQ%
		USE
			FILENAME$ = "PP_MONTHLY.SEQ"
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
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			"PP_MONTHLY.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		!
		! Open the Register header file
		!
4100		YYYY_PP$ = YYYY_PP

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.CRE"
		USE
			FILENAME$ = "PP_MONTHLY"
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
			RESET #PP_MONTHLY.SEQ%
		USE
			FILENAME$ = "PP_MONTHLY"
			CONTINUE HelpError
		END WHEN

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
		! Get a record from the temporary file and put it
		! in PP_MONTHLY
		!
		WHEN ERROR IN
			GET #PP_MONTHLY.SEQ%
			PUT #PP_MONTHLY.CH%
		USE
			CONTINUE 4130 IF ERR = 11%
			FILENAME$ = "PP_MONTHLY.SEQ"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 4120

		!
		! Done; Close the files (killing the temporary)
		!
4130		CLOSE #PP_MONTHLY.CH%
		CLOSE #PP_MONTHLY.SEQ%

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
	PP_TRAN_POST = EXIT_STATUS

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Handle the untrapped errors
	!
	RESUME HelpError

	END FUNCTION
