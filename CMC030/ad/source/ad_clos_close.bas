1	%TITLE "Asset Ledger Closing Program"
	%SBTTL "AD_CLOS_CLOSE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center, Idaho Falls, Idaho.
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Close Ledger For Period\* updates the Assets Period
	!	Ledger to the Asset Balances File and History File.
	!	.b
	!	The procedure will not allow the same Object to be updated for the
	!	same period more than once.
	!	.lm -5
	!
	! Index:
	!	.x Close Asset Ledger
	!	.x Asset>Ledger Close
	!	.x Asset>Period Ledger
	!	.x Asset>Balances File
	!	.x History File
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_CLOS_CLOSE/LINE
	!	$ LINK/EXE=AD_EXE: AD_CLOS_CLOSE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_CLOS_CLOSE.OBJ;*
	!
	! Author:
	!
	!	12/27/87 - Frank F.  Starman
	!
	! Modification history:
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	03/14/92 - Kevin handy
	!		Clean up variables (checkvar)
	!
	!	03/18/92 - Dan Perkins
	!		Change variable PERIOD.CUR$ to PERIOD_CUR$.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/09/95 - Kevin Handy
	!		Added display of periods for each dep
	!		group, with a status message.
	!
	!	04/04/95 - Kevin Handy
	!		(V3.6)
	!		Updated for changes made in V3.6
	!
	!	05/19/97 - Kevin Handy
	!		Use integer on key #.
	!
	!	07/30/97 - Kevin Handy
	!		Change READ_PERIOD parameter XAGE from REAL to INT.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/05/99 - Kevin Handy
	!		Use 'WHEN ERROR'
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Map statements
	!
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.HB"
	MAP (AD_BALANCE)		AD_BALANCE_CDD	AD_BALANCE

	%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.HB"
	MAP (AD_CALCULATION)	AD_CALCULATION_CDD	AD_CALCULATION

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP (AD_CONTROLOBJ)	AD_CONTROLOBJ_CDD	AD_CONTROLOBJ

	%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.HB"
	MAP (AD_HISTORY)	AD_HISTORY_CDD	AD_HISTORY

	%PAGE

	CALL READ_INITIALIZE

	!
	! Get device designations
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.UPD"
	USE
		FILENAME$ = "AD_CONTROLOBJ"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Ledger file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.UPD"
	USE
		FILENAME$ = "AD_CALCULATION"
		CONTINUE HelpError
	END WHEN

320	!
	! Open History file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.CRE"
	USE
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.CRE"
	USE
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

500	!
	! Paint the background, and confirm close
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_SCREEN_DATA%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SCREEN_DATA%, &
		"Asset Ledger Close for " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Asset # ", 6%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Object  ", 7%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Ob  Closed  Update  Status", 9%, 5%)

600	!
	! Display last period closed for each asset type
	!
	RESET #AD_CONTROLOBJ.CH%
	DISPLAY_LINE% = 10%

610	WHEN ERROR IN
		GET #AD_CONTROLOBJ.CH%, REGARDLESS
	USE
		CONTINUE 700
	END WHEN

	!
	! Look for any problems
	!
	TEXT1$ = ""

	IF AD_CONTROLOBJ::STATUS_FLAG = "2"
	THEN
		TEXT1$ = "Reset in process  "
	END IF

	IF AD_CONTROLOBJ::STATUS_FLAG = "3"
	THEN
		TEXT1$ = "Depreciation in process  "
	END IF

	IF AD_CONTROLOBJ::STATUS_FLAG = "3"
	THEN
		TEXT1$ = "Need to recalculate depreciation  "
	END IF

	PERIOD_CUR$ = AD_CONTROLOBJ::LASTPER
	V% = READ_PERIOD("FIND", AD_CONTROLOBJ::ERA, PERIOD_CUR$, &
		PERIOD_DESCR$, STAT$, "", "", 1%)

	IF PERIOD_CUR$ <> AD_CONTROLOBJ::LASTDEP
	THEN
		TEXT1$ = TEXT1$ + "Must update period "
	END IF

	TEXT$ = AD_CONTROLOBJ::DEP_OBJECT + "   " + &
		AD_CONTROLOBJ::LASTPER + "  " + &
		AD_CONTROLOBJ::LASTDEP + "  " + &
		TEXT1$
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		TEXT$, DISPLAY_LINE%, 5%)
	DISPLAY_LINE% = DISPLAY_LINE% + 1%

	GOTO 610


700	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!	^*Confirm\*
	!	.b
	!	.lm +5
	!	^*Confirm\* asks for user confirmation of the period that is to be updated
	!	to the Balances File and the History File.
	!	.lm -5
	!
	! Index:
	!	.x Confirm>Close Ledger for Period
	!	.x Close Ledger for Period>Confirm
	!
	!--

	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm updating - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	%PAGE

1000	!******************************************************************
	! Close Ledger
	!******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Updating", 1%)

	!
	! Find First record in the Asset ledger, going through by
	! object type.
	!
	TEST_OBJECT$ = ""

	RESET #AD_CALCULATION.CH%, KEY #1%

1020	!
	! Main loop starts here
	!
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AD_CALCULATION.CH%, REGARDLESS
	USE
		CONTINUE 1400 IF ERR = 11%
		FILENAME$ = "AD_CALCULATION"
		CONTINUE HelpError
	END WHEN

1030	IF TEST_OBJECT$ <> AD_CALCULATION::DEP_OBJECT
	THEN
		IF TEST_OBJECT$ <> ""
		THEN
			GET #AD_CONTROLOBJ.CH%, KEY #0% EQ TEST_OBJECT$

			AD_CONTROLOBJ::LASTPER		= AD_CONTROLOBJ::LASTDEP
			AD_CONTROLOBJ::STATUS_FLAG	= "0"

			UPDATE #AD_CONTROLOBJ.CH%
		END IF

		!
		! Set close flag in the object control file
		!
		GET #AD_CONTROLOBJ.CH%, KEY #0% EQ AD_CALCULATION::DEP_OBJECT

		IF AD_CONTROLOBJ::STATUS_FLAG = "2"
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Reset in process for object " + &
				AD_CALCULATION::DEP_OBJECT, 0%)
			GOTO ExitProgram
		END IF

		IF AD_CONTROLOBJ::STATUS_FLAG = "3"
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Depreciation in process for object " + &
				AD_CALCULATION::DEP_OBJECT, 0%)
			GOTO ExitProgram
		END IF

		IF AD_CONTROLOBJ::STATUS_FLAG = "3"
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Need to recalculate depreciation for object " + &
				AD_CALCULATION::DEP_OBJECT, 0%)
			GOTO ExitProgram
		END IF

		PERIOD_CUR$ = AD_CONTROLOBJ::LASTPER
		V% = READ_PERIOD("FIND", AD_CONTROLOBJ::ERA, PERIOD_CUR$, &
			PERIOD_DESCR$, STAT$, "", "", 1%)

		IF PERIOD_CUR$ <> AD_CONTROLOBJ::LASTDEP
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Must update period " + &
				PERIOD_CUR$ + " for object " + &
				AD_CALCULATION::DEP_OBJECT, 0%)
			GOTO ExitProgram
		END IF

		AD_CONTROLOBJ::STATUS_FLAG = "1"
		UPDATE #AD_CONTROLOBJ.CH%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "UPDATING " + &
			PERIOD_CUR$ + " " + PERIOD_DESCR$, 4%, 5%)

		!
		! Remove record with the close period from the History file
		!
		GOSUB 1800
	END IF

1040	!
	! Update balances
	!
	GOSUB 1700
	GOSUB 1600

	TEST_OBJECT$ = AD_CALCULATION::DEP_OBJECT
	GOTO 1020

1400	!
	! Update control file
	!
	IF TEST_OBJECT$<>""
	THEN
		GET #AD_CONTROLOBJ.CH%, KEY #0% EQ TEST_OBJECT$

		AD_CONTROLOBJ::LASTPER		= AD_CONTROLOBJ::LASTDEP
		AD_CONTROLOBJ::STATUS_FLAG	= "0"

		UPDATE #AD_CONTROLOBJ.CH%
	END IF

1500	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Updating completed", 0%)

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

1600	!******************************************************************
	! Add into the History file
	!******************************************************************

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		AD_CALCULATION::ASSET_NUM, &
		6%, 14%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		AD_CALCULATION::DEP_OBJECT, &
		7%, 14%)

	IF AD_CALCULATION::AMOUNT_CUR <> 0.0 OR AD_CALCULATION::UNIT_CUR <> 0.0
	THEN
		AD_HISTORY::ASSET_NUM	= AD_CALCULATION::ASSET_NUM
		AD_HISTORY::DEP_OBJECT	= AD_CALCULATION::DEP_OBJECT
		AD_HISTORY::DEP_STATUS	= PRIOR_STATUS$
		AD_HISTORY::AMOUNT_HIS	= AD_CALCULATION::AMOUNT_CUR
		AD_HISTORY::UNIT_HIS	= AD_CALCULATION::UNIT_CUR
		AD_HISTORY::PERIOD	= AD_CONTROLOBJ::LASTDEP

		PUT #AD_HISTORY.CH%
	END IF

	RETURN

1700	!******************************************************************
	! Update or add into the Balance file
	!******************************************************************

	WHEN ERROR IN
		GET #AD_BALANCE.CH%, KEY #0% EQ AD_CALCULATION::ASSET_NUM + &
			AD_CALCULATION::DEP_OBJECT
	USE
		CONTINUE 1710 IF ERR = 155%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

	PRIOR_STATUS$ = AD_BALANCE::DEP_STATUS

	AD_BALANCE::DEP_STATUS	= AD_CALCULATION::DEP_STATUS
	AD_BALANCE::AMOUNT_CTD	= AD_BALANCE::AMOUNT_CTD + &
		AD_CALCULATION::AMOUNT_CUR
	AD_BALANCE::UNIT_CTD	= AD_BALANCE::UNIT_CTD + &
		AD_CALCULATION::UNIT_CUR

1705	IF AD_BALANCE::LASTPER <> AD_CONTROLOBJ::LASTDEP
	THEN
		AD_BALANCE::LASTPER = AD_CONTROLOBJ::LASTDEP
		WHEN ERROR IN
			UPDATE #AD_BALANCE.CH%
		USE
			FILENAME$ = "AD_BALANCE"
			CONTINUE HelpError
		END WHEN

	END IF

	RETURN

1710	PRIOR_STATUS$ = "A"
	!
	! Add a new record into the Store file
	!
	AD_BALANCE::ASSET_NUM	= AD_CALCULATION::ASSET_NUM
	AD_BALANCE::DEP_OBJECT	= AD_CALCULATION::DEP_OBJECT
	AD_BALANCE::DEP_STATUS	= AD_CALCULATION::DEP_STATUS
	AD_BALANCE::AMOUNT_CTD	= AD_CALCULATION::AMOUNT_CUR
	AD_BALANCE::UNIT_CTD	= AD_CALCULATION::UNIT_CUR
	AD_BALANCE::LASTPER	= AD_CONTROLOBJ::LASTDEP

	WHEN ERROR IN
		PUT #AD_BALANCE.CH%
	USE
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

	RETURN

1800	!
	! Remove record with the close period from the History file
	!
	WHEN ERROR IN
		GET #AD_HISTORY.CH%, &
			KEY #1% EQ PERIOD_CUR$ + AD_CALCULATION::DEP_OBJECT

		DELETE #AD_HISTORY.CH%
	USE
		CONTINUE HistoryEnd IF ERR = 155%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

	GOTO 1800

 HistoryEnd:
	RETURN

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

32767	END
