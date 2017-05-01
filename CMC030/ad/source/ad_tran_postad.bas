1	%TITLE "AD System Posting Function"
	%SBTTL "AD_TRAN_POSTAD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_TRAN_POSTAD(LONG OPT, &
		LONG		SUBOPT, &
		STRING		BATCH_NUMBER, &
		STRING		TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		AD_CALCULATION_CDD AD_CALCULATION, &
		AD_REGUNIT_CDD	AD_REGUNIT_POST, &
		STRING		GLPERIOD)

	!
	! COPYRIGHT (C) 1989 BY
	!
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
	!	AD>POST
	!	POST>AD
	!	.b
	!	.lm +5
	!	AD_TRAN_POSTAD performs two functions, assigned
	!	by two very different POST programs:
	!	.b
	!	1) Will make sure that AD Calculation ledger
	!	information is correct, so that the ledger
	!	can be posted to the GL System.  (AD_POST_LEDGER)
	!	.b
	!	2) Will post AD Units Journal information
	!	into the AD Unit Register file.
	!	(AD_POST_UNITS)
	!
	! Index:
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	OPT		is the variable which decides what this
	!		function will be doing each time it is run.
	!		(i.e. OPT = OPT_CONFIRM is a signal to confirm
	!		the post process with the user)
	!	SUBOPT
	!	BATCH_NUMBER
	!	TITLE()		is the array holding the titles and headings
	!		which will be printed on the final transmittal.
	!	UTL_REPORTX	is a data structure used by all functions
	!		and subprograms involved in printing reports and
	!		posting transmittals.
	!	AD_CALCULATION	is a data structure used by the AD Post
	!		program to hold a single record from the Asset
	!		Calculation ledger.
	!	AD_REGUNIT
	!	GLPERIOD	contains the Year and Period (in YYYYPP
	!		form) of the GL Year/Period Ledger.
	!
	! Outputs:
	!
	!	Returned Value	is CMC$_NORMAL when whatever this function
	!		is doing has been done correctly, CMC$_WARNING when
	!		there are easily (?) solved problems, or some other
	!		value if REALLY bizarre things are going on.
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_TRAN_POSTAD
	!	$ LIB FUNC_LIB:CMCFUN/REP AD_TRAN_POSTAD
	!	$ DELETE AD_TRAN_POSTAD.OBJ;*
	!
	! Author:
	!
	!	06/16/89 - Aaron Redd
	!
	! Modification History:
	!
	!	06/29/89 - Aaron Redd
	!			Rewrote and reformatted this function so that
	!		not only could it be used by both AD_POSTs (LEDGER and
	!		UNITS), but that it would more closely follow the
	!		(now-)standardized form of all TRAN_POST functions.
	!
	!	09/20/91 - Frank F. Starman
	!		Return END_DATE when checking posting period.
	!
	!	03/12/92 - Kevin Handy
	!		Removed duplicate error trapping (check)
	!
	!	04/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/07/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add additional error traps
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include special CMC code file
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions and MAPs/DECLAREs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.HB"
	MAP	(AD_REGUNIT)	AD_REGUNIT_CDD		AD_REGUNIT

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROL.HB"
	MAP	(AD_CONTROL)	AD_CONTROL_CDD		AD_CONTROL

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP	(AD_CONTROLOBJ)	AD_CONTROLOBJ_CDD	AD_CONTROLOBJ

	COM (AD_TRAN_POSTAD.CH) &
		AD_CONTROLOBJ.CH%, &
		AD_CONTROL.CH%, &
		AD_REGUNIT.CH%, &
		AD_REGUNIT.SEQ%

	COM (AD_TRAN_POSTAD.COM) &
		STRING	LAST_OBJECT = 1%, &
		STRING	START_DATE = 8%, &
		STRING	END_DATE = 8%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH

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
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

	!
	! Set the record counter to zero
	!
	RECORDS% = 0%

	%PAGE

	SELECT OPT

	CASE OPT_RESTART

		!
		! Remove the posted records from the AD REGUNIT file
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "AD_REGUNIT.HIS", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Open AD_REGUNIT History file
		!
1000		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.CRE"
		USE
			!
			! Can't open a file only for ourselves
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "AD_REGUNIT"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Do any AD_REGUNIT records have this batch number?
		!
1010		WHEN ERROR IN
			FIND #AD_REGUNIT.CH%, KEY #1% EQ BATCH_NUMBER
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1030 IF ERR = 155%
			FILENAME$ = "AD_REGUNIT"
			CONTINUE HelpError
		END WHEN

1020		!
		! Get and delete all records with this batch number
		!
		WHEN ERROR IN
			GET #AD_REGUNIT.CH%
			GOTO 1030 IF AD_REGUNIT::BATCH <> BATCH_NUMBER
			DELETE #AD_REGUNIT.CH%
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 1030 IF ERR = 11%
			FILENAME$ = "AD_REGUNIT"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%
		GOTO 1020

		!
		! Close the cleaned-up AD_REGUNIT History
		!
1030		CLOSE #AD_REGUNIT.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, "", "", "", "")

		!
		! Tell the user just how many records we deleted
		!
		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_CHECK

2000		!
		! Open AD object control file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.UPD"
		USE
			!
			! Can't open a file only for ourselves
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "AD_CONTROLOBJ"
			CONTINUE HelpError
		END WHEN

		SELECT (SUBOPT AND 65535%)

		CASE SUBOPT_LEDGER

2100			!
			! Open AD Control file
			!
			%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROL.UPD"
			GET #AD_CONTROL.CH%, RECORD 1%, REGARDLESS

			TEST_PERIOD$ = AD_CONTROL::LASTPER

			GET #AD_CONTROLOBJ.CH%, &
				KEY #0% EQ AD_CONTROL::DEP_OBJECT

			V% = READ_PERIOD("FIND", AD_CONTROLOBJ::ERA, &
				TEST_PERIOD$, PERIOD_DESCR$, STAT$, &
				START_DATE, END_DATE, 1%)

			IF (TEST_PERIOD$ <> AD_CONTROLOBJ::LASTDEP)
			THEN
				TEXT$ = SPACE$(18%) + &
					"The updated period must be " + &
					TEST_PERIOD$
				CALL OUTP_LINE("", UTL_REPORTX, TITLE(), &
					TEXT$, 0%)
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF

			GLPERIOD = TEST_PERIOD$

			!
			! Make sure that the period has not been closed
			!
			IF (GLPERIOD <= AD_CONTROL::LASTPER)
			THEN
				TEXT$ = SPACE$(18%) + "GL period " + GLPERIOD + &
					" has been closed"
				CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
			!
			!Return also end date of the period
			!
			GLPERIOD = GLPERIOD + END_DATE

		CASE SUBOPT_REGISTER

2200			WHEN ERROR IN
				%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.OPN"
				GET #AD_REGUNIT.CH%, KEY #1% EQ BATCH_NUMBER
			USE
				!
				! Can't open a file only for ourselves
				!
				IF ERR = 138%
				THEN
					SLEEP 5%
					RETRY
				END IF

				!
				! Locked block
				!
				IF ERR = 154%
				THEN
					SLEEP 1%
					RETRY
				END IF

				CONTINUE 2210 IF (ERR = 155%) OR (ERR = 5%)
				FILENAME$ = "AD_REGUNIT"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = CMC$_WARNING
			GOTO ExitFunction

2210			CALL ASSG_CHANNEL(AD_REGUNIT.SEQ%, STAT%)
			WHEN ERROR IN
				OPEN "AD_REGUNIT.SEQ" AS FILE AD_REGUNIT.SEQ%, &
					ORGANIZATION SEQUENTIAL FIXED, &
					MAP AD_REGUNIT, &
					TEMPORARY, &
					ALLOW NONE, &
					ACCESS MODIFY
			USE
				FILENAME$ = "AD_REGUNIT.SEQ"
				CONTINUE HelpError
			END WHEN

			LAST_OBJECT = " "

		END SELECT

	CASE OPT_ADDREC

		SELECT (SUBOPT AND 65535%)

		CASE SUBOPT_LEDGER

			!
			! Check AD Calculation DEP_OBJECT
			!
			WHEN ERROR IN
				GET #AD_CONTROL.CH%, RECORD 1%, REGARDLESS
			USE
				FILENAME$ = "AD_CONTROL"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = CMC$_WARNING IF &
				AD_CALCULATION::DEP_OBJECT <> &
				AD_CONTROL::DEP_OBJECT

		CASE SUBOPT_REGISTER

3210			IF (LAST_OBJECT <> AD_REGUNIT_POST::DEP_OBJECT)
			THEN
				!
				! Get the control object record matching RegUnit
				!
				GET #AD_CONTROLOBJ.CH%, &
					KEY #0% EQ AD_REGUNIT_POST::DEP_OBJECT

				!
				! Check the status flag of the object record
				!
				IF (AD_CONTROLOBJ::STATUS_FLAG <> "0")
				THEN
					TEXT$ = "The Object Status Flag is " + &
						AD_CONTROLOBJ::STATUS_FLAG
					CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
					EXIT_STATUS = CMC$_WARNING
				END IF

				!
				! Check dates if necessary
				!
				IF (SUBOPT AND SUBOPT_CHECK)
				THEN
					!
					! Read the period information
					!
					V% = READ_PERIOD("READ", AD_CONTROLOBJ::ERA, &
						GLPERIOD, "", "", START_DATE, END_DATE, 0%)

					EXIT_STATUS = CMC$_DATEOUT IF &
						((AD_REGUNIT_POST::ACTION_DATE < START_DATE) OR &
						(AD_REGUNIT_POST::ACTION_DATE > END_DATE))
				END IF
			END IF

3220			AD_REGUNIT = AD_REGUNIT_POST
			PUT #AD_REGUNIT.SEQ%

			LAST_OBJECT = AD_REGUNIT_POST::DEP_OBJECT

		END SELECT

	CASE OPT_CONFIRM
		!
		! Not much to confirm right now
		!

	CASE OPT_POSTFILE

		SELECT (SUBOPT AND 65535%)

		CASE SUBOPT_LEDGER
			!
			! Find and update AD Control file record
			!
			GET #AD_CONTROL.CH%, RECORD 1%, REGARDLESS

			AD_CONTROL::LASTPER = GLPERIOD
			UPDATE #AD_CONTROL.CH%

			CLOSE #AD_CONTROL.CH%
			CALL ASSG_FREECHANNEL(AD_CONTROL.CH%)

		CASE SUBOPT_REGISTER

			!
			! Get ready
			!
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"AD_REGUNIT.HIS", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Open the AD Unit Register file
			!
4200			WHEN ERROR IN
				%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.CRE"
			USE
				!
				! Can't open a file only for ourselves
				!
				IF ERR = 138%
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "AD_REGUNIT"
				CONTINUE HelpError
			END WHEN

			EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", "")

			GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

			!
			! Start at the beginning of the temporary file
			!
4210			RESET #AD_REGUNIT.SEQ%

4220			!
			! Get a record from the temp file and put it
			! in AD_REGUNIT
			!
			WHEN ERROR IN
				GET #AD_REGUNIT.SEQ%
				PUT #AD_REGUNIT.CH%
			USE
				CONTINUE 4230 IF ERR = 11%
				FILENAME$ = "AD_REGUNIT.SEQ"
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 4220

			!
			! Done; Close the files (killing the temporary)
			!
4230			CLOSE #AD_REGUNIT.CH%
			CLOSE #AD_REGUNIT.SEQ%

			EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "", "", "")

			!
			! Tell the user how many records we posted
			!
			TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
					" Posted Records"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		END SELECT

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:
	AD_TRAN_POSTAD = EXIT_STATUS
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
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	!******************************************************************
	! End of function AD_TRAN_POSTAD
	!******************************************************************
	END FUNCTION
