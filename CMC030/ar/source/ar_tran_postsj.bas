1	%TITLE "Accounts Receivable Journal Posting Function"
	%SBTTL "AR_TRAN_POSTSJ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_TRAN_POSTSJ( &
		LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		AR_SJH_CDD AR_SJH_POST, &
		AR_SJL_CDD AR_SJL_POST, &
		STRING BATCH_NO)

	!
	! COPYRIGHT (C) 1993 BY
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho  83402.
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
	!	This function is used to post records into the
	!	Accounts Receivable Sales Journals.
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
	!	AR_SJH_POST
	!	AR_SJL_POST
	!	BATCH_NO
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_TRAN_POSTSJ
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_TRAN_POSTSJ
	!	$ DELETE AR_TRAN_POSTSJ.OBJ;*
	!
	! Author:
	!
	!	02/16/93 - Kevin Handy
	!
	! Modification history:
	!
	!	02/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/14/96 - Kevin Handy
	!		Reformat source code.
	!
	!	02/13/97 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Remove large quanity of commented out code.
	!		Lose error traps for commented out code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/07/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Map statements
	!
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL
	MAP (AR_SJH)		AR_SJH_CDD		AR_SJH
	MAP (AR_SJL)		AR_SJL_CDD		AR_SJL
	MAP (DP_OUTP_XUNSOL)				RRR_FLAG%

	!
	! Common memory areas
	!
	COM (AR_TRAN_POSTSJ.COM) &
		AR_SJH.CH%, &
		AR_SJL.CH%, &
		AR_SJH.SEQ%, &
		AR_SJL.SEQ%

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
			"AR_SJH.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open AR_SJH file
		!
1100		BATCH_NO$ = BATCH_NO
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.CRE"
		USE
			!
			! Cannot open file(s) exclusively for post
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "AR_SJH"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Close the cleaned-up AR_SJH file
		!
1130		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Now remove interrupted post records from AR_SJL file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"AR_SJL.LED", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open AR_SJL file
		!
1200		BATCH_NO$ = BATCH_NO
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.CRE"
		USE
			!
			! Cannot open file(s) exclusively for post
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "AR_SJL"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Now close the clean AR Open Dist file
		!
1230		CLOSE #AR_SJL.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

	CASE OPT_CHECK

2100		BATCH_NO$ = BATCH_NO
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.OPN"
		USE
			!
			! Cannot open file(s) exclusively for post
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2200 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "AR_SJH"
			CONTINUE HelpError
		END WHEN

2110		BATCH_NO$ = BATCH_NO
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.OPN"
		USE
			!
			! Cannot open file(s) exclusively for post
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2200 IF (ERR = 155%) OR (ERR = 5%)
			FILENAME$ = "AR_SJL"
			CONTINUE HelpError
		END WHEN

2200		CALL ASSG_CHANNEL(AR_SJH.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "AR_SJH.SEQ" AS FILE AR_SJH.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP AR_SJH, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "AR_SJH"
			CONTINUE HelpError
		END WHEN

2210		CALL ASSG_CHANNEL(AR_SJL.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "AR_SJL.SEQ" AS FILE AR_SJL.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP AR_SJL, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "AR_SJL"
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
			AR_SJH = AR_SJH_POST
			WHEN ERROR IN
				PUT #AR_SJH.SEQ%
			USE
				FILENAME$ = "AR_SJH"
				CONTINUE HelpError
			END WHEN

		!
		! Put the AR Distribution posting record in the temporary file
		!
3010		CASE SUBOPT_LINEITEM
			AR_SJL = AR_SJL_POST
			WHEN ERROR IN
				PUT #AR_SJL.SEQ%
			USE
				FILENAME$ = "AR_SJL"
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
		! Post all records from the OPEN temp file to AR_SJH.LED
		!
		CASE SUBOPT_REGISTER

			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, &
				"AR_SJH.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Open item file
			!
4100			BATCH_NO$ = BATCH_NO
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.CRE"
			USE
				!
				! Cannot open file(s) exclusively for post
				!
				IF ERR = 138%
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "AR_SJH"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4110			RESET #AR_SJH.SEQ%

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
			! Get a record from the temporary file and put it in AR_SJH
			!
			WHEN ERROR IN
				GET #AR_SJH.SEQ%
				PUT #AR_SJH.CH%
			USE
				CONTINUE 4130 IF ERR = 11%
				FILENAME$ = "AR_SJH"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4120

			!
			! Done; Close the files (killing the temporary)
			!
4130			CLOSE #AR_SJH.CH%
			CLOSE #AR_SJH.SEQ%

		!
		! Post all records from the Dist temp file to AR_SJL.LED
		!
		CASE SUBOPT_LINEITEM
			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"AR_SJL.LED", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AR Open Distribution file
			!
4200			BATCH_NO$ = BATCH_NO
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.CRE"
			USE
				!
				! Cannot open file(s) exclusively for post
				!
				IF ERR = 138%
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "AR_SJL"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4210			RESET #AR_SJL.SEQ%

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
			! Get a record from the temp and put it in AR_SJL
			!
			WHEN ERROR IN
				GET #AR_SJL.SEQ%
				PUT #AR_SJL.CH%
			USE
				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "AR_SJL"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #AR_SJL.CH%
			CLOSE #AR_SJL.SEQ%

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

	AR_TRAN_POSTSJ = EXIT_STATUS
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
