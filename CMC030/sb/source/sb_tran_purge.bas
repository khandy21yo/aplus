1	%TITLE "Purge SubAccount/Balance/Budget Files"
	%SBTTL "SB_TRAN_PURGE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SB_TRAN_PURGE(LONG OPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD	UTL_REPORTX)

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
	!	This function is used to purge the SB_SUBACCOUNT, SB_BALANCE
	!	and the SB_BUDGET files.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_TRAN_PURGE
	!	$ LIB FUNC_LIB:CMCFUN/REP SB_TRAN_PURGE
	!	$ DELETE SB_TRAN_PURGE.OBJ;*
	!
	! AUTHOR:
	!
	!	05/26/89 - B. Craig Larsen
	!
	! MODIFICATION HISTORY:
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/05/2000 - Kevin Handy
	!		Use A"x"B
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

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD	SB_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD	SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_BUDGET.HB"
	MAP (SB_BUDGET)	SB_BUDGET_CDD	SB_BUDGET

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	COM (SB_TRAN_PURGE_COM) RECORDS%, &
		SB_CONTROL.CH%, &
		SB_SUBACCOUNT.CH%, &
		SB_BALANCE.CH%, &
		SB_BUDGET.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED
	EXTERNAL LONG	FUNCTION	OUTP_FILE

	DECLARE LONG	EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

	!
	! Set up input from user
	!
	SUBJECT$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! ABSTRACT:SUBJECT
	!
	!--

	LIMITDATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))
	!++
	! ABSTRACT:LIMITDATE
	!
	!--

	SELECT	SUBJECT$
	CASE "J"
		DEF_SYSTEM$ = "JC"

	CASE ELSE
		DEF_SYSTEM$ = ""

	END SELECT

	SELECT OPT

	CASE OPT_OPENFILE

		SELECT SUBJECT$
		CASE "J"

		CASE ELSE
			TEXT$ = SPACE$(18%) + "Unknown Subject '" + SUBJECT$ + &
				"' - Will attempt to purge " + &
				"SB_SUBACCOUNT file only."
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

			GOTO 250

		END SELECT

200		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.MOD"

			GET #SB_CONTROL.CH%, KEY #0% EQ DEF_SYSTEM$
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		!
		! Check control flag
		!
		IF SB_CONTROL::CONTROLFLAG <> "0" AND &
			SB_CONTROL::CONTROLFLAG <> "4"
		THEN
			TEXT$ = SPACE$(18%) + &
				"The status flag in the control " + &
				"file is " + SB_CONTROL::CONTROLFLAG + "."
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitFunction
		END IF

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"SB_SUBACCOUNT.MAS", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

220		RECORDS% = 0%
		SB_CONTROL::CONTROLFLAG = "4"
		SB_CONTROL::BATCH	= BATCH_NUMBER
		SB_CONTROL::CDATE	= DATE_TODAY
		SB_CONTROL::CTIME	= TIME_NOW

		WHEN ERROR IN
			UPDATE #SB_CONTROL.CH%
		USE
			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

230		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.PST"
		USE
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

240		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BUDGET.PST"
		USE
			FILENAME$ = "SB_BUDGET"
			CONTINUE HelpError
		END WHEN

250		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.PST"
		USE
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN

260		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

	CASE OPT_DELETE

400		WHEN ERROR IN
			FIND #SB_SUBACCOUNT.CH%, KEY #0% EQ SUBJECT$
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE ExitFunction IF ERR = 155%
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN

 NextRec:
410		WHEN ERROR IN
			GET #SB_SUBACCOUNT.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE ExitFunction IF ERR = 11%
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		GOTO ExitFunction IF SB_SUBACCOUNT::SUBJECT <> SUBJECT$

		GOTO NextRec IF SB_SUBACCOUNT::EDATE >= LIMITDATE$ OR &
			SB_SUBACCOUNT::SSTATUS <> "C"

		SELECT	SUBJECT$
		CASE "J"

		CASE ELSE
			GOTO 460

		END SELECT

 NextRec1:
420		WHEN ERROR IN
			FIND #SB_BUDGET.CH%, KEY #0% EQ DEF_SYSTEM$ + &
				SB_SUBACCOUNT::SUBACCOUNT
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 440 IF ERR = 155%
			FILENAME$ = "SB_BUDGET"
			CONTINUE HelpError
		END WHEN

430		WHEN ERROR IN
			GET #SB_BUDGET.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 440 IF ERR = 11%
			FILENAME$ = "SB_BUDGET"
			CONTINUE HelpError
		END WHEN

		GOTO NextRec2 IF SB_BUDGET::SYSTEM <> DEF_SYSTEM$ OR &
			SB_BUDGET::SUBACCOUNT <> SB_SUBACCOUNT::SUBACCOUNT

		DELETE #SB_BUDGET.CH%

		!
		! Try for next record
		!
		GOTO NextRec1

 NextRec2:
440		WHEN ERROR IN
			FIND #SB_BALANCE.CH%, KEY #0% EQ DEF_SYSTEM$ + &
				SB_SUBACCOUNT::SUBACCOUNT
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 460 IF ERR = 155%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

450		WHEN ERROR IN
			GET #SB_BALANCE.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 460 IF ERR = 11%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		GOTO 460 IF SB_BALANCE::SYSTEM <> DEF_SYSTEM$ OR &
			SB_BALANCE::SUBACCOUNT <> SB_SUBACCOUNT::SUBACCOUNT

		DELETE #SB_BALANCE.CH%

		!
		! Try for next record
		!
		GOTO NextRec2

460		WHEN ERROR IN
			DELETE #SB_SUBACCOUNT.CH%
		USE
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN

		RECORDS% = RECORDS% + 1%

		!
		! Output a line to the file
		!
		TEXT$ = SB_SUBACCOUNT::SUBJECT + "  " + &
			SB_SUBACCOUNT::SUBACCOUNT + " " + &
			LEFT(SB_SUBACCOUNT::DESCR, 30%) + " " + &
			SB_SUBACCOUNT::TTYPE + " " + &
			SB_SUBACCOUNT::CLASS + " " + &
			PRNT_DATE(SB_SUBACCOUNT::BDATE, 8%) + " " + &
			SB_SUBACCOUNT::SSTATUS + " " + &
			PRNT_DATE(SB_SUBACCOUNT::EDATE, 8%)

		!
		! Keep undefined codes
		!
		EXIT_STATUS = OUTP_FILE(OPT_ADDREC, TITLE(), &
			UTL_RECORDX, TEXT$, ADD_TITLE$())
		GOTO ExitFunction IF EXIT_STATUS <> CMC$_NORMAL

		!
		! Try for next record
		!
		GOTO NextRec

	CASE OPT_CLOSEFILE

		SELECT	SUBJECT$
		CASE "J"

		CASE ELSE
			GOTO 510

		END SELECT

500		WHEN ERROR IN
			GET #SB_CONTROL.CH%, KEY #0% EQ DEF_SYSTEM$

			SB_CONTROL::CONTROLFLAG	= "0"
			UPDATE #SB_CONTROL.CH%
			CLOSE #SB_CONTROL.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN
510		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Records were deleted from SB_SUBACCOUNT file."
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_REPORT
		!
		! Print deleted records if any
		!
		ADD_TITLE$(1%) = "RECORDS DELETED FROM SUBACCOUNT FILE"
		X% = (132% - LEN(ADD_TITLE$(1%))) / 2%
		ADD_TITLE$(1%) = STRING$(X%, A"="B) + ADD_TITLE$(1%) + &
			STRING$(132% - (X% + LEN(ADD_TITLE$(1%))), A"="B)
		ADD_TITLE$(2%) = &
			"Sb Subaccount Description                    " + &
			"Ty Clas OnsetDate  S TermDate"

		EXIT_STATUS = OUTP_FILE(OPT_SUMMARY, TITLE(), &
			UTL_REPORTX, TEXT$, ADD_TITLE$())

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:

	SB_TRAN_PURGE = EXIT_STATUS
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
