1	%TITLE "Sales Order Posting Function"
	%SBTTL "OE_TRAN_POST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_TRAN_POST( &
		LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		OE_REGHEADER_CDD OE_REGHEADER_POST, &
		OE_REGLINE_CDD OE_REGLINE_POST)

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
	!	This function posts records in the
	!	Sales Order Register.
	!	OE_TRAN_POST also will remove records from
	!	these ledgers if restarting an interrupted post.
	!	.lm -5
	!
	! Index:
	!	.x Post>Sales Order
	!	.x Sales Order>Post
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
	!	OE_REGHEADER_POST
	!	OE_REGLINE_POST
	!	GLPERIOD
	!
	! Output:
	!
	!	Returned Value
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_TRAN_POST
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_TRAN_POST
	!	$ DELETE OE_TRAN_POST.OBJ;*
	!
	! Author:
	!
	!	08/07/90 - Frank F. Starman
	!
	! Modification history:
	!
	!	10/08/91 - Frank F. Starman
	!		Check keyboard input after marking file.
	!
	!	10/16/91 - Frank F. Starman
	!		Display error message if err 134 while dumping
	!		records in the Register file.
	!		Open register files with .CRE extention. Posting
	!		will be slower, but doesn't need to wait for
	!		users sitting in the OE journals to unlock file.
	!
	!	04/12/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/05/93 - Frank F. Starman
	!		Trap error 134 at line 4120.
	!
	!	11/02/94 - Kevin Handy
	!		Modified to update SHIPVIA field with the last
	!		one used if a header record already exists.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/01/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	02/07/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/28/99 - Kevin Handy
	!		Clean up source code
	!
	!	07/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/25/2000 - Kevin Handy
	!		On "duplicate record" on 4120, go to next
	!		record in sequential file, and not quit
	!		adding remaining headers. (kbj)
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
	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.HB"

	!
	! Map statements
	!
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	MAP (DP_OUTP_XUNSOL)	RRR_FLAG%
	MAP (OE_CONTROL)	OE_CONTROL_CDD		OE_CONTROL

	!
	! Common memory areas
	!
	COM (OE_TRAN_POST.COM) OE_REGHEADER.CH%, &
		OE_REGLINE.CH%, &
		OE_REGHEADER.SEQ%, &
		OE_REGLINE.SEQ%

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
		! Now remove interrupted post records from OE_REGLINE file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"OE_REGHEADER.HIS", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open OE_REGLINE file
		!
1200		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.CRE"
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 155%
			FILENAME$ = "OE_REGHEADER"
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
			FIND #OE_REGHEADER.CH%, &
				KEY #4% EQ BATCH_NUMBER
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 155%
			FILENAME$ = "OE_REGHEADER"
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
			GET #OE_REGHEADER.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1230 IF ERR = 11%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

		IF (OE_REGHEADER::BATCH = BATCH_NUMBER)
		THEN
			DELETE #OE_REGHEADER.CH%
			RECORDS% = RECORDS% + 1%
			GOTO 1220
		END IF

		!
		! Now close the register header file
		!
1230		CLOSE #OE_REGHEADER.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Print out the number of deleted records
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Now remove interrupted post records from OE_REGLINE file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"OE_REGLINE.HIS", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open OE_REGLINE file
		!
1300		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.CRE"
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "OE_REGLINE"
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
			FIND #OE_REGLINE.CH%, KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 155%
			FILENAME$ = "OE_REGLINE"
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
			GET #OE_REGLINE.CH%
			GOTO 1330 IF (OE_REGLINE::BATCH <> BATCH_NUMBER)
			DELETE #OE_REGLINE.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1330 IF ERR = 11%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 1320

		!
		! Now close the register line file
		!
1330		CLOSE #OE_REGLINE.CH%

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
		! Open OE control file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.OPN"
			GET #OE_CONTROL.CH%, &
				RECORD 1%, &
				REGARDLESS
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "OE_CONTROL"
			CONTINUE HelpError
		END WHEN

		CLOSE OE_CONTROL.CH%
		CALL ASSG_FREECHANNEL(OE_CONTROL.CH%)

		!
		! Check control flag
		!
		IF OE_CONTROL::STATUS_FLAG <> "0"
		THEN
			TEXT$ = SPACE$(18%) + &
				"The OE control file status flag is " + &
				OE_CONTROL::STATUS_FLAG + "."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF


2100		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2110 IF (ERR = 5%)
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

		WHEN ERROR IN
			GET #OE_REGHEADER.CH%, &
				KEY #2% EQ BATCH_NUMBER
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 2110 IF (ERR = 155%)
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2110		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 2200 IF (ERR = 5%)
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		WHEN ERROR IN
			GET #OE_REGLINE.CH%, &
				KEY #1% EQ BATCH_NUMBER
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 2200 IF (ERR = 155%)
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction

2120	!

2200		CALL ASSG_CHANNEL(OE_REGHEADER.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "OE_REGHEADER.SEQ" AS FILE OE_REGHEADER.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP OE_REGHEADER, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "OE_REGHEADER.SEQ"
			CONTINUE HelpError
		END WHEN

2210		CALL ASSG_CHANNEL(OE_REGLINE.SEQ%, STAT%)
		WHEN ERROR IN
			OPEN "OE_REGLINE.SEQ" AS FILE OE_REGLINE.SEQ%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				MAP OE_REGLINE, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "OE_REGLINE.SEQ"
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
			OE_REGHEADER = OE_REGHEADER_POST
3010			WHEN ERROR IN
				PUT #OE_REGHEADER.SEQ%
			USE
				FILENAME$ = "OE_REGHEADER.SEQ"
				CONTINUE HelpError
			END WHEN

		!
		! Put the Register line posting record in the temporary file
		!
		CASE SUBOPT_LINEITEM
			OE_REGLINE = OE_REGLINE_POST
3020			WHEN ERROR IN
				PUT #OE_REGLINE.SEQ%
			USE
				FILENAME$ = "OE_REGLINE.SEQ"
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
		! Post all records from the OPEN temp file to OE_REGHEADER.HIS
		!
		CASE SUBOPT_REGISTER

			!
			! Get ready
			!
4100			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"OE_REGHEADER.HIS", "", "", "")

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
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.CRE"

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
			WHEN ERROR IN
				RESET #OE_REGHEADER.SEQ%
			USE
				FILENAME$ = "OE_REGHEADER"
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
			! Get a record from the temporary file and put
			! it in OE_REGHEADER
			!
			WHEN ERROR IN
				GET #OE_REGHEADER.SEQ%
				PUT #OE_REGHEADER.CH%
			USE
				CONTINUE 4125 IF ERR = 134%
				CONTINUE 4130 IF ERR = 11%
				FILENAME$ = "OE_REGHEADER"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4120

4125			!
			! If the record already exists in the master file,
			! then update fields as necessary
			!
			KEEP_SHIPVIA$ = OE_REGHEADER::SHIPVIA

			WHEN ERROR IN
				GET #OE_REGHEADER.CH%, &
					KEY #0% EQ OE_REGHEADER::ORDNUM
			USE
				IF ERR = 154%
				THEN
					SLEEP 1%
					RETRY
				END IF

				CONTINUE 4120
			END WHEN

			OE_REGHEADER::SHIPVIA = KEEP_SHIPVIA$ &
				IF KEEP_SHIPVIA$ <> "  "

			WHEN ERROR IN
				UPDATE #OE_REGHEADER.CH%
			USE
				CONTINUE 4120
			END WHEN

			GOTO 4120

			!
			! Done; Close the files (killing the temporary)
			!
4130			CLOSE #OE_REGHEADER.CH%
			CLOSE #OE_REGHEADER.SEQ%

		!
		! Post all records to OE_REGLINE.HIS
		!
		CASE SUBOPT_LINEITEM
			!
			! Get ready
			!
4200			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"OE_REGLINE.HIS", "", "", "")

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
			! Open the Register line file
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.CRE"
			USE
				IF ERR = 138%
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "OE_REGLINE"
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
				RESET #OE_REGLINE.SEQ%
			USE
				FILENAME$ = "OE_REGLINE"
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

			!
			! Get a record from the temp and put it in OE_REGLINE
			!
			WHEN ERROR IN
				GET #OE_REGLINE.SEQ%
				PUT #OE_REGLINE.CH%
			USE
				IF ERR = 154%
				THEN
					SLEEP 1%
					RETRY
				END IF

				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "OE_REGLINE"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #OE_REGLINE.CH%
			CLOSE #OE_REGLINE.SEQ%

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

	OE_TRAN_POST = EXIT_STATUS

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
	RESUME HelpError

	END FUNCTION
