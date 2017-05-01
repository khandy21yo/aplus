1	%TITLE "Asset Ledger Reset"
	%SBTTL "AD_CLOS_RESET"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1986 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!	The ^*Reset Ledger For Period\* removes all last
	!	updated period records from the Asset Balances File and
	!	the History File. After resetting the particular objects, the
	!	last period updated in the Object Control File will be reset
	!	to the immediate prior period.
	!	.lm -5
	!
	! Index:
	!	.x Reset>Asset Ledger
	!	.x Asset>Ledger Reset
	!	.x History File
	!	.x Asset>Balances File
	!	.x Period
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_CLOS_RESET/LINE
	!	$ LINK/EXE=AD_EXE: AD_CLOS_RESET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_CLOS_RESET.OBJ;*
	!
	! Author:
	!
	!	12/28/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	03/14/92 - Kevin Handy
	!		Clean up variables (checkvar)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/10/95 - Kevin Handy
	!		Added display of periods for each dep
	!		group, with a status message.
	!
	!	04/04/95 - Kevin Handy
	!		(V3.6)
	!		Modifications to bring up to new version.
	!		Define SMG_RESET as a long.
	!
	!	04/12/95 - Kevin Handy
	!		Changed SCOPE.EXIT% to SCOPE::SCOPE_EXIT
	!
	!	09/26/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/19/97 - Kevin Handy
	!		Use integer in #key's
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter to READ_PERIOD to integer.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/15/99 - Kevin Handy
	!		Use WHEN ERROR
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
	MAP (AD_BALANCE)	AD_BALANCE_CDD	AD_BALANCE

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP (AD_CONTROLOBJ)	AD_CONTROLOBJ_CDD	AD_CONTROLOBJ

	%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.HB"
	MAP (AD_HISTORY)	AD_HISTORY_CDD	AD_HISTORY

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 1%
	DECLARE INTEGER SMG_RESET

	!
	! Declare constants
	!
	DECLARE LONG XLONG, YLONG

	%PAGE

	CALL READ_INITIALIZE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.UPD"
	USE
		FILENAME$ = "AD_CONTROLOBJ"
		CONTINUE HelpError
	END WHEN

310	!
	! Open History file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.UPD"
	USE
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.UPD"
	USE
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

	OBJECT_ITEM$ = "*" + SPACE$(19%)

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_RESET, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_RESET, &
		"Asset Ledger Reset for " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_RESET, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	GOSUB Repaint

1100	!
	! Enter options
	!
	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Change Blank Go Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO 1000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	SELECT OPT$

	CASE "C"
 Changer:
		!*****************************************************
		! Change information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
			"", "Item to change", &
			0.0, 4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 1000

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO 1100 IF LOOP% = 0%
		GOTO Changer IF LOOP% < 1% OR LOOP% > MAX_ITEM

 Changer1:	FLAG% = 0%
		GOSUB DataEntry

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 1000

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			LOOP% = LOOP% - 1% IF LOOP% > 1%
			GOTO Changer1

		!
		! SMG$K_TRM_DOWN
		!
		CASE SMG$K_TRM_DOWN
			LOOP% = LOOP% + 1% IF LOOP% < MAX_ITEM
			GOTO Changer1

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO Changer

	CASE "B"
 BlankR:	!*****************************************************
		! Blank information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
			"", "Item to Blank", 0.0, &
			4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 1000

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO 1100 IF LOOP% = 0%
		GOTO Blankr IF LOOP% < 1% OR LOOP% > MAX_ITEM

		SELECT LOOP%

		CASE 1%
			LSET OBJECT_ITEM$ = "*" + SPACE$(19%)

		END SELECT

		FLAG% = 1%
		GOSUB DataEntry

		GOTO Blankr

	!
	! Go
	!
	CASE "G"
		GOSUB ResetPeriod
		GOTO ExitProgram

	!
	! Help
	!
	! This option calls out a help message describing the
	! program.
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", "PROG", SCOPE::PRG_PROGRAM, &
			"", "HELP")
		GOTO 1000

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 1100

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 Repaint:
	!******************************************************************
	! Repaint the screen
	!******************************************************************

	DATA	5,20, "(01) Object", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_RESET, ATEXT$, XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry FOR LOOP% = 1% TO MAX_ITEM

	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESET, &
		"Ob  Closed  Update  Status", 9%, 5%)

1600	!
	! Display last period closed for each asset type
	!
	RESET #AD_CONTROLOBJ.CH%
	DISPLAY_LINE% = 10%

1610	WHEN ERROR IN
		GET #AD_CONTROLOBJ.CH%, REGARDLESS
	USE
		CONTINUE 1690
	END WHEN

	!
	! Look for any problems
	!
	TEXT1$ = ""

	IF AD_CONTROLOBJ::STATUS_FLAG = "1"
	THEN
		TEXT1$ = "Updating in process"
	END IF

	!IF AD_CONTROLOBJ::STATUS_FLAG = '3'
	!THEN
	!	TEXT1$ = "Depreciation in process"
	!END IF

	PERIOD_CUR$ = AD_CONTROLOBJ::LASTPER
	V% = READ_PERIOD("FIND", AD_CONTROLOBJ::ERA, PERIOD_CUR$, &
		PERIOD_DESCR$, STAT$, "", "", 1%)

	TEXT$ = AD_CONTROLOBJ::DEP_OBJECT + "   " + &
		AD_CONTROLOBJ::LASTPER + "  " + &
		AD_CONTROLOBJ::LASTDEP + "  " + &
		TEXT1$
	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESET, &
		TEXT$, DISPLAY_LINE%, 5%)
	DISPLAY_LINE% = DISPLAY_LINE% + 1%

	GOTO 1610

1690	!
	RETURN

	%PAGE

 DataEntry:
	!******************************************************************
	! Enter/Diaplay items
	!******************************************************************

	TEMP$ = TRM$(SCOPE::PRG_ITEM)

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(LOOP%, "<0>##")

	SELECT LOOP%

	CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Object\*
	!	.b
	!	.lm +5
	!	The ^*Object\* field contains an object which is to be reset.
	!	.b
	!	A blank field causes all objects to be reset.
	!	.lm -5
	!
	! Index:
	!	.x Object>Reset Ledger
	!	.x Reset Ledger>Object
	!
	!--
		OBJECT_ITEM$ = ENTR_3STRING(SCOPE, SMG_RESET, &
			"5;45", "Object", &
			OBJECT_ITEM$, FLAG%, "'E", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 ResetPeriod:
2000	!******************************************************************
	! Reset Ledger
	!******************************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_RESET, &
		"", "Confirm reset - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESET, "Asset # ", 9%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESET, "Object  ", 10%, 5%)

	CALL ENTR_3MESSAGE(SCOPE, "Updating", 1%)

	TEST_OBJECT$ = ""

	RESET #AD_HISTORY.CH%, KEY #2%


 GetNextRec:
2020	!
	! Main loop starts here
	!
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AD_HISTORY.CH%
	USE
		CONTINUE 2400 IF ERR = 11%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

 CheckObject:
	GOTO GetNextRec &
		IF COMP_STRING(AD_HISTORY::DEP_OBJECT, OBJECT_ITEM$) = 0%
	GOTO GetNextRec IF AD_HISTORY::DEP_OBJECT = TEST_OBJECT$

2030	IF TEST_OBJECT$ <> ""
	THEN
		GOSUB 2800

		GET #AD_CONTROLOBJ.CH%, KEY #0% EQ TEST_OBJECT$
		AD_CONTROLOBJ::LASTPER		= PERIOD_CUR$
		AD_CONTROLOBJ::STATUS_FLAG	= "0"

		UPDATE #AD_CONTROLOBJ.CH%
	END IF

	!
	! Set close flag in the object control file
	!
	GET #AD_CONTROLOBJ.CH%, KEY #0% EQ AD_HISTORY::DEP_OBJECT

	IF AD_CONTROLOBJ::STATUS_FLAG = "1"
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Updating in process for object " + &
			AD_HISTORY::DEP_OBJECT, 0%)
		GOTO EndReset
	END IF

	PERIOD_CUR$ = AD_CONTROLOBJ::LASTPER
	V% = READ_PERIOD("FIND", AD_CONTROLOBJ::ERA, PERIOD_CUR$, &
		PERIOD_DESCR$, STAT$, "", "", -1%)

	AD_CONTROLOBJ::STATUS_FLAG = "2"
	UPDATE #AD_CONTROLOBJ.CH%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESET, &
		"RESET TO THE " + PERIOD_CUR$ + " -  " + PERIOD_DESCR$, &
		7%, 5%)

	TEST_OBJECT$ = AD_HISTORY::DEP_OBJECT

2035	WHEN ERROR IN
		FIND #AD_HISTORY.CH%, &
			KEY #2% EQ AD_HISTORY::DEP_OBJECT + &
			AD_CONTROLOBJ::LASTPER,	&
			REGARDLESS
	USE
		CONTINUE 2020 IF ERR = 155%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

2040	WHEN ERROR IN
		GET #AD_HISTORY.CH%
	USE
		CONTINUE 2400 IF ERR = 11%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

	!
	! Reset balances
	!
	GOTO CheckObject IF TEST_OBJECT$ <> AD_HISTORY::DEP_OBJECT

	GOSUB 2700

	GOTO 2040

2400	!
	! Update control file
	!
	IF TEST_OBJECT$ <> ""
	THEN
		GOSUB 2800
		GET #AD_CONTROLOBJ.CH%, KEY #0% EQ TEST_OBJECT$

		AD_CONTROLOBJ::LASTPER		= PERIOD_CUR$
		AD_CONTROLOBJ::STATUS_FLAG	= "0"

		UPDATE #AD_CONTROLOBJ.CH%
	END IF

2500	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Reset completed", 0%)

 EndReset:
	RETURN

	%PAGE

2700	!******************************************************************
	! Update into the Balance file
	!******************************************************************

	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESET, AD_HISTORY::ASSET_NUM, &
		9%, 14%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESET, AD_HISTORY::DEP_OBJECT, &
		10%, 14%)

	WHEN ERROR IN
		GET #AD_BALANCE.CH%, &
			KEY #0% EQ AD_HISTORY::ASSET_NUM + &
			AD_HISTORY::DEP_OBJECT
	USE
		CONTINUE 2710 IF ERR = 155%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

	AD_BALANCE::DEP_STATUS	= AD_HISTORY::DEP_STATUS
	AD_BALANCE::AMOUNT_CTD	= AD_BALANCE::AMOUNT_CTD - &
		AD_HISTORY::AMOUNT_HIS
	AD_BALANCE::AMOUNT_CTD	= 0 IF AD_BALANCE::AMOUNT_CTD < 0.0

	AD_BALANCE::UNIT_CTD	= AD_BALANCE::UNIT_CTD - &
		AD_HISTORY::UNIT_HIS
	AD_BALANCE::UNIT_CTD	= 0 IF AD_BALANCE::UNIT_CTD < 0.0

2705	IF AD_BALANCE::LASTPER <> PERIOD_CUR$
	THEN
		AD_BALANCE::LASTPER = PERIOD_CUR$
		UPDATE #AD_BALANCE.CH%
	END IF

2710	RETURN

2800	!
	! Remove record with the close period from the History file
	!
	WHEN ERROR IN
		GET #AD_HISTORY.CH%, &
			KEY #2% EQ TEST_OBJECT$ + AD_CONTROLOBJ::LASTPER
		DELETE #AD_HISTORY.CH%
	USE
		CONTINUE HistoryEnd IF ERR = 155%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

	GOTO 2800

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
