1	%TITLE "Close Subledger"
	%SBTTL "SB_TRAN_CLOSE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	!
	! ABSTRACT:HELP
	!	.p
	!	Special program to hack up the JC Balances by sucking
	!	in the detail from a specific GL file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_SPEC_WEIRDCLOSE
	!	$ LINK/EXE=JC_EXE: JC_SPEC_WEIRDCLOSE,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_SPEC_WEIRDCLOSE.OBJ;*
	!
	! AUTHOR:
	!
	!	02/18/2000 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
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
	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	%PAGE

	SYSTEM$ = "JC"

200	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.MOD"

	GET #SB_CONTROL.CH%, KEY #0% EQ SYSTEM$, REGARDLESS

210	YYYYPP$ = "199911"
	YYYY_PP$ = "1999_12"

220	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.MOD"

240	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"

250	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"

260	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"

420	RESET #GL_YYYY_PP.CH%

 NextRec3:
430	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 1000
	END WHEN

	print "*";
	print GL_YYYY_PP::ACCT if ccpos(0%) >= 50%

	!
	! In this case I don't have any blank subaccounts that I want
	! to worry about, so a quick skip out is OK
	!
	IF GL_YYYY_PP::SUBACC = ""
	THEN
		GOTO 430
	END IF

	IF LAST.ACCT$ = GL_YYYY_PP::ACCT AND TEST.ACCT% = 0%
	THEN
		WHEN ERROR IN
			FIND #GL_YYYY_PP.CH%, KEY #0% GT LAST.ACCT$
		USE
			CONTINUE 1000
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

	LAST.ACCT$ = GL_YYYY_PP::ACCT
	TEST.ACCT% = 0%

440	FIND #SB_ACCOUNT.CH%, KEY #0% EQ SYSTEM$, REGARDLESS

 NextAcct:
	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE 430
	END WHEN

450	GOTO 430 &
		IF SYSTEM$ <> SB_ACCOUNT::SYSTEM

	IF COMP_STRING(GL_YYYY_PP::ACCT, SB_ACCOUNT::ACCOUNT) = 0%
	THEN
		Goto NextAcct
	END IF

	TEST.ACCT% = -1%

	!
	! Test subaccount number
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, &
			KEY #0% EQ SB_CONTROL::SUBJECT + GL_YYYY_PP::SUBACC, &
			REGARDLESS
	USE
		CONTINUE 600
	END WHEN

	WHEN ERROR IN
		GET #SB_BALANCE.CH%, KEY #0% EQ SYSTEM$ + &
			GL_YYYY_PP::SUBACC + GL_YYYY_PP::OPERATION + &
			GL_YYYY_PP::ACCT + YYYYPP$
	USE
		CONTINUE 600
	END WHEN

	SB_BALANCE::AMOUNT = SB_BALANCE::AMOUNT + &
		GL_YYYY_PP::AMOUNT
	SB_BALANCE::UNITS  = SB_BALANCE::UNITS	+ &
		GL_YYYY_PP::UNITS
	SB_BALANCE::HOURS  = SB_BALANCE::HOURS	+ &
		GL_YYYY_PP::HOURS

	UPDATE #SB_BALANCE.CH%

	print "!";
	print SB_BALANCE::SUBACCOUNT if ccpos(0%) >= 50%

500	WHEN ERROR IN
		GET #SB_BALANCE.CH%
	USE
		CONTINUE 600
	END WHEN

	IF SYSTEM$ = SB_BALANCE::SYSTEM AND &
		SB_BALANCE::SUBACCOUNT = GL_YYYY_PP::SUBACC AND &
		SB_BALANCE::OPERATION = GL_YYYY_PP::OPERATION AND &
		SB_BALANCE::ACCOUNT = GL_YYYY_PP::ACCT
	THEN
		SB_BALANCE::BEG_AMOUNT = SB_BALANCE::BEG_AMOUNT + &
			GL_YYYY_PP::AMOUNT
		SB_BALANCE::BEG_UNITS  = SB_BALANCE::BEG_UNITS	+ &
			GL_YYYY_PP::UNITS
		SB_BALANCE::BEG_HOURS  = SB_BALANCE::BEG_HOURS	+ &
			GL_YYYY_PP::HOURS

		UPDATE #SB_BALANCE.CH%

		print ".";
		print SB_BALANCE::SUBACCOUNT if ccpos(0%) >= 50%

		GOTO 500
	END IF

 !	GOTO 450

600	!
	! Try for next record
	!
	GOTO NextAcct

1000	!
	! Done
	!

32767	END
