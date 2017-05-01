1	%TITLE "Resynchronize Subledger"
	%SBTTL "SB_TRAN_RESYNC"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SB_TRAN_RESYNC(LONG OPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		STRING SYSTEM)

	!
	! COPYRIGHT (C) 1987, 1988, 1989 BY
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	! ABSTRACT:HELP
	!	.p
	!	This function is used to resynchronize the subledger.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_TRAN_RESYNC
	!	$ LIB FUNC_LIB:CMCFUN/REP SB_TRAN_RESYNC
	!	$ DELETE SB_TRAN_RESYNC.OBJ;*
	!
	! AUTHOR:
	!
	!	05/12/89 - B. Craig Larsen
	!
	! MODIFICATION HISTORY:
	!
	!	05/24/91 - Kevin Handy
	!		Modified to close GL_PERIOD file as soon as possible.
	!
	!	08/26/92 - Frank F. Starman
	!		Replace GL_CHARTEX with SB_ACCOUNT.
	!
	!	09/03/92 - Dan Perkins
	!		Trap ERR 11 at line 440.
	!
	!	12/08/92 - Frank F. Starman
	!		Check for undefined subbaccount while reading GL.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/27/93 - Kevin Handy
	!		Added messages to output so that user doesn't
	!		get "Aborted"/crash as the only indication of a
	!		problem in the post.
	!
	!	10/04/93 - Kevin Handy
	!		Modified so that undefined subaccounts will not
	!		cause "interrupt", since it just wants to try to
	!		delete the record and continue anyway, losing all
	!		but the last undefined message.
	!
	!	02/27/95 - Kevin Handy
	!		Disabled unsolicited input stuff in an attempt
	!		to stop DWI crashing.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/08/95 - Kevin Handy
	!		Lose commented out code, unecessary externals.
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/04/97 - Kevin Handy
	!		Lose commented out code, merge several '"xx"+"xx"',
	!		fix several "if xxx+yyy=xx1+yy1", reformat closer
	!		to 80 columns.
	!
	!	08/26/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	06/18/98 - Kevin Handy
	!		Ignore 'zero' records from the GL, so I don't
	!		have so many complaints from LL.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	02/21/2000 - Kevin Handy
	!		Modified to speed up scan through SB_ACCOUNT by
	!		jumping out of loop when the system doesn't match
	!		instead of looping to the end of the file.
	!
	!	03/06/2000 - Kevin Handy
	!		Add code to do a fast skip over accounts that are
	!		not WIP accounts. Hopefully this will really speed
	!		up the resync.
	!		Load SB_ACCOUNT into SUBACCOUNT_LIST$() so we don't
	!		have to loop through that file so many many times.
	!		Use WHEN ERROR IN for error trapping.
	!		(Seems to have doubled it's speed)
	!
	!	03/02/2004 - Kevin Handy
	!		Increase dimensions on number of GL period files
	!		and on the number of subaccounts to fix a MARCO
	!		problem.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE			SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	COM (SB_TRAN_RESYNC_COM) RECORDS%, &
		SB_CONTROL.CH%, &
		SB_BALANCE.CH%, &
		GL_YYYY_PP.CH%, &
		SB_ACCOUNT.CH%, &
		YYYYPP$ = 6%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL LONG	FUNCTION OUTP_UNDEFCODES

	DECLARE  LONG	EXIT_STATUS

	!
	! Dimension statements
	!
	DIM GL_YYYY_PP_FILE$(200%)
	DIM SUBACCOUNT_LIST$(400%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

	SELECT OPT

	CASE OPT_OPENFILE

200		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.MOD"
		USE
			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		WHEN ERROR IN
			GET #SB_CONTROL.CH%, KEY #0% EQ SYSTEM
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		YYYYPP$ = SB_CONTROL::PERIOD

		!
		! Check control flag
		!
		IF SB_CONTROL::CONTROLFLAG <> "0" AND &
			SB_CONTROL::CONTROLFLAG <> "3"
		THEN
			TEXT$ = SPACE$(18%) + &
				"The status flag in the control file is " + &
				SB_CONTROL::CONTROLFLAG + "."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

220		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.PST"
		USE
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "SB_BALANCE.HIS", "", YYYYPP$, "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

230		RECORDS% = 0%
		SB_CONTROL::CONTROLFLAG = "3"
		SB_CONTROL::BATCH	= BATCH_NUMBER
		SB_CONTROL::CDATE	= DATE_TODAY
		SB_CONTROL::CTIME	= TIME_NOW

		UPDATE #SB_CONTROL.CH%

240		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
		USE
			FILENAME$ = "SB_ACCOUNT"
			CONTINUE HelpError
		END WHEN

250		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

	CASE OPT_CHECK

300		WHEN ERROR IN
			FIND #SB_BALANCE.CH%, KEY #1% EQ YYYYPP$ + SYSTEM
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE ExitFunction IF ERR = 155%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

 NextRec:
310		WHEN ERROR IN
			GET #SB_BALANCE.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE ExitFunction IF ERR = 11%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		GOTO ExitFunction IF SB_BALANCE::PERIOD <> YYYYPP$ OR &
			SB_BALANCE::SYSTEM <> SYSTEM

		!
		! Test subaccount number
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT(SB_CONTROL::SUBJECT, &
			SB_BALANCE::SUBACCOUNT, SB_SUBACCOUNT_EXAM)

		SELECT EXIT_STATUS

		CASE CMC$_NORMAL
			! Code found

		CASE CMC$_UNDEFINED

			DELETE #SB_BALANCE.CH% IF SB_BALANCE::BEG_AMOUNT = 0.0
			EXIT_STATUS = CMC$_NORMAL

		CASE ELSE
			TEXT$ = SPACE$(18%) + "Subaccount lookup faialure " + &
				SB_BALANCE::SUBACCOUNT + " - Reason " + &
				NUM1$(EXIT_STATUS)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO ExitFunction

		END SELECT

		GOTO NextRec

	CASE OPT_UPDATE

400		!
		! Let's load up the list of subaccounts so that we don't
		! have to loop through the file billions of times
		!
		SUBACCOUNT_LIST% = 0%

		WHEN ERROR IN
			FIND #SB_ACCOUNT.CH%, KEY #0% EQ SYSTEM, REGARDLESS

 SubLoad:
			GET #SB_ACCOUNT.CH%, REGARDLESS

			IF SYSTEM = SB_ACCOUNT::SYSTEM
			THEN
				SUBACCOUNT_LIST% = SUBACCOUNT_LIST% + 1%
				SUBACCOUNT_LIST$(SUBACCOUNT_LIST%) = &
					SB_ACCOUNT::ACCOUNT
				GOTO SubLoad
			END IF

		USE
			CONTINUE 405
		END WHEN

405		!
		! Big long pass to zero out all of the running balances
		!
		WHEN ERROR IN
			FIND #SB_BALANCE.CH%, KEY #1% EQ YYYYPP$ + SYSTEM
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 412 IF ERR = 155%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

 NextRec2:
410		WHEN ERROR IN
			GET #SB_BALANCE.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 412 IF ERR = 11%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		GOTO 412 IF SB_BALANCE::SYSTEM <> SYSTEM

		IF SB_BALANCE::PERIOD >= YYYYPP$
		THEN
			SB_BALANCE::AMOUNT	= 0.0
			SB_BALANCE::UNITS	= 0.0
			SB_BALANCE::HOURS	= 0.0

			UPDATE #SB_BALANCE.CH%
		END IF

		!
		! Try for next record
		!
		GOTO NextRec2

412		!
		! Now a pass to loop through all of the GL periods that
		! are open in the ledger
		!
		! Get information needed to open GL Period files (GL_YYYY_PP)
		!
		CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

		CALL FIND_FILE(GL_YYYY_PP.DEV$ + "GL_*.LED", &
			GL_YYYY_PP_FILE$(), 16%, "", "")

		GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

		IF GL_YYYY_PP_FILE% > 0%
		THEN
			GL_YYYY_PP_FILE$(LOOP%) = &
				MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 7%) &
				FOR LOOP% = 1% TO GL_YYYY_PP_FILE%

			LOOP% = 0%
		ELSE
			GOTO ExitFunction
		END IF

		!
		! Process the next GL period
		!
 NextFile:
414		LOOP% = LOOP% + 1%
		GOTO ExitFunction IF LOOP% > GL_YYYY_PP_FILE%

		YYYY_PP$ = GL_YYYY_PP_FILE$(LOOP%)
		GLPERIOD$ = LEFT(YYYY_PP$, 4%) + RIGHT(YYYY_PP$, 6%)
		GOTO NextFile IF GLPERIOD$ < YYYYPP$ OR LEN(YYYY_PP$) <> 7%

		CLOSE #GL_YYYY_PP.CH%

		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"GL_" + GLPERIOD$ + ".LED", "", "", &
			GLPERIOD$)

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

420		WHEN ERROR IN
			RESET #GL_YYYY_PP.CH%
		USE
			CONTINUE 414 IF ERR = 155% OR ERR = 11%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

		!
		! Start by assuming success
		!
		TEST.ACCT% = -1%

		!
		! Process one GL period record
		!
 NextRec3:
430		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%, REGARDLESS
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 414 IF ERR = 11%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

		IF LAST.ACCT$ = GL_YYYY_PP::ACCT AND TEST.ACCT% = 0%
		THEN
			!
			! Since we've seen this account already, and we know
			! that is isn't defined as a WIP account, let's
			! try to skip to the next account.
			!
			! This should probibly be handled somehow else, but
			! the flags were already checked here, so I jus
			! modified this test somewhat.
			!
			WHEN ERROR IN
				FIND #GL_YYYY_PP.CH%, &
					KEY #0% GT LAST.ACCT$, &
					REGARDLESS
			USE
				IF ERR = 154%
				THEN
					SLEEP 5%
					RETRY
				END IF

				CONTINUE 414
			END WHEN

			GOTO NextRec3
		END IF

		!
		! Ignore the record if it wouldn't affect anything
		!
		GOTO NextRec3 &
			IF (GL_YYYY_PP::AMOUNT = 0.0) AND &
			(GL_YYYY_PP::UNITS = 0.0) AND &
			(GL_YYYY_PP::HOURS = 0.0)

		!
		! Assume we will not use this account
		!
		LAST.ACCT$ = GL_YYYY_PP::ACCT
		TEST.ACCT% = 0%

		!
		! Look through the list of valid subaccoiunts, and
		! decide if this one is useful
		!
		FOR LOOP1% = 1% TO SUBACCOUNT_LIST%
			IF COMP_STRING(GL_YYYY_PP::ACCT, &
				SUBACCOUNT_LIST$(LOOP1%))
			THEN
				GOSUB ValidSubaccount
				GOTO 430
			END IF
		NEXT LOOP1%

		!
		! Try for next record
		!
		GOTO 430

 ValidSubaccount:
		!
		! We will use this account after all
		!
		TEST.ACCT% = -1%

		!
		! Test subaccount number
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT(SB_CONTROL::SUBJECT, &
			GL_YYYY_PP::SUBACC, SB_SUBACCOUNT_EXAM)

		SELECT EXIT_STATUS

		CASE CMC$_NORMAL

			! Code found

		CASE CMC$_UNDEFINED

			TOTAL = TOTAL + GL_YYYY_PP::AMOUNT

			!
			! Decide weither to print check number
			! or invoice number
			!
			IF GL_YYYY_PP::CKNO = "" AND &
				TRM$(GL_YYYY_PP::REFNO) <> ""
			THEN
				TEMP$ = "-" + &
					LEFT(GL_YYYY_PP::REFNO, 7%)
			ELSE
				TEMP$ = GL_YYYY_PP::CKNO + "  "
			END IF

			TEXT$ = GLPERIOD$ + "  " + &
				GL_YYYY_PP::ACCT + " " + &
				GL_YYYY_PP::SUBACC + " " + &
				GL_YYYY_PP::XREFNO + " " + &
				TEMP$ + " " + &
				GL_YYYY_PP::BTHNUM + " " + &
				PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%) + " " + &
				LEFT(GL_YYYY_PP::DESCR, 26%) + " " + &
				FORMAT$(GL_YYYY_PP::AMOUNT, "##,###,###.## ") + &
				FORMAT$(TOTAL, "###,###,###.##")

			!
			! Keep undefined codes
			!
			EXIT_STATUS = OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_RECORDX, TEXT$)

			GOTO NextRec3

		CASE ELSE
			TEXT$ = SPACE$(18%) + &
				"Subaccount lookup faialure " + &
				SB_BALANCE::SUBACCOUNT + &
				" - Reason " + NUM1$(EXIT_STATUS)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), &
				TEXT$, 0%)
			GOTO ExitFunction

		END SELECT

		RECORDS% = RECORDS% + 1%

450		!
		! Try for existing record
		!
		WHEN ERROR IN
			GET #SB_BALANCE.CH%, KEY #0% EQ SYSTEM + &
				GL_YYYY_PP::SUBACC + GL_YYYY_PP::OPERATION + &
				GL_YYYY_PP::ACCT + YYYYPP$
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 460 IF ERR = 155%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		SB_BALANCE::AMOUNT = SB_BALANCE::AMOUNT + &
			GL_YYYY_PP::AMOUNT
		SB_BALANCE::UNITS  = SB_BALANCE::UNITS + &
			GL_YYYY_PP::UNITS
		SB_BALANCE::HOURS  = SB_BALANCE::HOURS + &
			GL_YYYY_PP::HOURS

		UPDATE #SB_BALANCE.CH%

		GOTO 490

460		!
		! No current record, so create one
		!
		SB_BALANCE::SYSTEM	= SYSTEM
		SB_BALANCE::SUBACCOUNT	= GL_YYYY_PP::SUBACC
		SB_BALANCE::OPERATION	= GL_YYYY_PP::OPERATION
		SB_BALANCE::ACCOUNT	= GL_YYYY_PP::ACCT
		SB_BALANCE::PERIOD	= YYYYPP$

		SB_BALANCE::AMOUNT	= GL_YYYY_PP::AMOUNT
		SB_BALANCE::UNITS	= GL_YYYY_PP::UNITS
		SB_BALANCE::HOURS	= GL_YYYY_PP::HOURS

		SB_BALANCE::BEG_AMOUNT	= 0.0
		SB_BALANCE::BEG_UNITS	= 0.0
		SB_BALANCE::BEG_HOURS	= 0.0

		PUT #SB_BALANCE.CH%

490		RETURN

	CASE OPT_CLOSEFILE

500		GET #SB_CONTROL.CH%, KEY #0% EQ SYSTEM

		SB_CONTROL::CONTROLFLAG	= "0"

		UPDATE #SB_CONTROL.CH%

		CLOSE #SB_CONTROL.CH%

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Records updated"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_REPORT
		!
		! Print undefined code if any
		!
		TEXT$ = "Period  Account            SubAccount " + &
			"Xref       Ck/Ref   Batch    Date     " + &
			"Description                       Amount " + &
			" Running-Total"

		EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), &
			UTL_REPORTX, TEXT$)

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:

	SB_TRAN_RESYNC = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR

	TEXT$ = "%Untrapped error " + NUM1$(ERR) + &
		" - " + ERT$(ERR) + " at " + NUM1$(ERL) + " in " + &
		ERN$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
