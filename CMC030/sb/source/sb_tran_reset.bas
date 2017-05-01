1	%TITLE "Reset Subledger"
	%SBTTL "SB_TRAN_RESET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SB_TRAN_RESET(LONG OPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX, &
		STRING SYSTEM)

	!
	! COPYRIGHT (C) 1987 BY
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	!
	! ABSTRACT:HELP
	!	.p
	!	This function is used to reset subledger for
	!	account number
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_TRAN_RESET
	!	$ LIB FUNC_LIB:CMCFUN/REP SB_TRAN_RESET
	!	$ DELETE SB_TRAN_RESET.OBJ;*
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
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/14/94 - Kevin Handy
	!		Fixed bug going back over year end, where it would
	!		add 1 to the year instead of subtracting 1.
	!
	!	01/14/94 - Kevin Handy
	!		Modified to use format$(x-1,...)  instead of
	!		right(num(x+9999),2) business, to make it clearer
	!		what is happening.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Lose extra parameter on ASSG_FREECHANNEL.
	!
	!	11/10/95 - Kevin Handy
	!		Keep track of deleted/updated records seperately.
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/26/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD	SB_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD	SB_BALANCE

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	COM (SB_TRAN_RESET_COM) UPD_RECORDS%, &
		DEL_RECORDS%, &
		SB_CONTROL.CH%, &
		SB_BALANCE.CH%, &
		CUR_PERIOD$ = 6%, &
		PREVIOUS_PERIOD$ = 6%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION OUTP_UNDEFCODES

	DECLARE LONG	EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

	SELECT OPT

	!
	! Remove batch number
	!
	CASE OPT_RESTART
		!
		! Not used
		!

	CASE OPT_OPENFILE

200		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.MOD"
			GET #SB_CONTROL.CH%, KEY #0% EQ SYSTEM
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		PREVIOUS_PERIOD% = VAL%(RIGHT(SB_CONTROL::PERIOD, 5%)) - 1%
		CUR_PERIOD% = VAL%(RIGHT(SB_CONTROL::PERIOD, 5%))
		YEAR$ = LEFT(SB_CONTROL::PERIOD, 4%)

		!
		! Check control flag
		!
		IF SB_CONTROL::CONTROLFLAG <> "0" AND &
			SB_CONTROL::CONTROLFLAG <> "2"
		THEN
			TEXT$ = SPACE$(18%) + &
				"The status flag in the control file is " + &
				SB_CONTROL::CONTROLFLAG + "."
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

210		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.UPD"
			GET #GL_PERIOD.CH%, RECORD 1%
			CLOSE #GL_PERIOD.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "GL_PERIOD"
			CONTINUE HelpError
		END WHEN

		CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

		CUR_PERIOD$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

		IF PREVIOUS_PERIOD% < 1%
		THEN
			PREVIOUS_PERIOD% = GL_PERIOD::FPFY
			YEAR$ = FORMAT$(VAL%(YEAR$) - 1%, "<0>###")
		END IF

		PREVIOUS_PERIOD$ = YEAR$ + FORMAT$(PREVIOUS_PERIOD%, "<0>#")

		!
		! Open up batch control file and get a batch number
		!
		IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "SB_BALANCE", SYSTEM, &
			CUR_PERIOD$, PREVIOUS_PERIOD$) <> CMC$_NORMAL
		THEN
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"SB_BALANCE.HIS", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

220		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.PST"
		USE
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

230		UPD_RECORDS% = 0%
		DEL_RECORDS% = 0%
		SB_CONTROL::CONTROLFLAG = "2"
		SB_CONTROL::BATCH	= BATCH_NUMBER
		SB_CONTROL::CDATE	= DATE_TODAY
		SB_CONTROL::CTIME	= TIME_NOW

		WHEN ERROR IN
			UPDATE #SB_CONTROL.CH%
		USE
			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

	CASE OPT_UPDATE

400		WHEN ERROR IN
			FIND #SB_BALANCE.CH%, KEY #1% EQ CUR_PERIOD$ + SYSTEM
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE ExitFunction IF ERR = 155%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

 NextRec:
410		WHEN ERROR IN
			GET #SB_BALANCE.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE ExitFunction IF ERR = 11%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		GOTO ExitFunction IF SB_BALANCE::PERIOD <> CUR_PERIOD$ OR &
			SB_BALANCE::SYSTEM <> SYSTEM

		IF ABS(SB_BALANCE::AMOUNT + &
			SB_BALANCE::UNITS + SB_BALANCE::HOURS) = 0.0
		THEN
			DELETE #SB_BALANCE.CH%
			DEL_RECORDS% = DEL_RECORDS% + 1%
		ELSE
			SB_BALANCE::BEG_AMOUNT	= 0.0
			SB_BALANCE::BEG_UNITS	= 0.0
			SB_BALANCE::BEG_HOURS	= 0.0

			UPDATE #SB_BALANCE.CH%

			UPD_RECORDS% = UPD_RECORDS% + 1%
		END IF

		GOTO NextRec

	CASE OPT_CONFIRM

		EXIT_STATUS = OUTP_UNDEFCODES(OPT_CONFIRM, &
			TITLE(), UTL_REPORTX, "")

	CASE OPT_CLOSEFILE

500		WHEN ERROR IN
			GET #SB_CONTROL.CH%, KEY #0% EQ SYSTEM

			SB_CONTROL::PERIOD	= PREVIOUS_PERIOD$
			SB_CONTROL::CONTROLFLAG	= "0"
			UPDATE #SB_CONTROL.CH%
			CLOSE #SB_CONTROL.CH%
		USE
			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		TEXT$ = SPACE$(9%) + FORMAT$(UPD_RECORDS%, "########") + &
			" Records updated in " + SB_CONTROL::PERIOD + " Ledger"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%) &
			IF UPD_RECORDS%

		TEXT$ = SPACE$(9%) + FORMAT$(DEL_RECORDS%, "########") + &
			" Records deleted in " + SB_CONTROL::PERIOD + " Ledger"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%) &
			IF DEL_RECORDS%

	CASE OPT_REPORT
		!
		! Print undefined code if any
		!
		TEXT$ = " Subacct#    Operation AccountNumber "

		EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), &
			UTL_REPORTX, TEXT$)

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:

	SB_TRAN_RESET = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	EXIT_STATUS = CMC$_UNTERROR
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
