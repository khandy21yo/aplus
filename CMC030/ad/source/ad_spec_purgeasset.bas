1	%TITLE "Purge Retired Assets"
	%SBTTL "AD_SPEC_PURGEASSET"
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
	!	Using the ^*Purge Assets\* program will generally be a
	!	"From Time/To Time" procedure. It helps to keep in the
	!	Asset Masterfile, Retired Asset File, Asset Balances File,
	!	Assets History File, and Units Register File only necessary
	!	assets. There is an option to purge only an asset with a
	!	certain active flag and to the selected date.
	!	.lm -5
	!
	! Index:
	!	.x Purge Retired Assets
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_SPEC_PURGEASSET/LINE
	!	$ LINK/EXE=AD_EXE: AD_SPEC_PURGEASSET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_SPEC_PURGEASSET.OBJ;*
	!
	! Author:
	!
	!	02/01/88 - Frank Starman
	!
	! Modification history:
	!
	!	10/13/88 - Frank Starman
	!		Purge only retired asset
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	02/26/91 - Craig Tanner
	!		AD_ASSET conversion to AD_35ASSET.
	!
	!	03/19/92 - Dan Perkins
	!		Commented out OPTION$ which doesn't seem to be
	!		used for anything.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.HB"
	MAP (AD_BALANCE)	AD_BALANCE_CDD	AD_BALANCE

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD	AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.HB"
	MAP (AD_DEPRECIATION)	AD_DEPRECIATION_CDD	AD_DEPRECIATION

	%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.HB"
	MAP (AD_HISTORY)	AD_HISTORY_CDD	AD_HISTORY

	%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.HB"
	MAP (AD_REGUNIT)	AD_REGUNIT_CDD	AD_REGUNIT

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 1%

	!
	! Declare constants
	!
	DECLARE LONG XLONG, YLONG, SMG_PURGE

	%PAGE

	ON ERROR GOTO 19000

	CALL READ_INITIALIZE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.UPD"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.UPD"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.UPD"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.UPD"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

370	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.UPD"
	USE
		CONTINUE 500 IF ERR = 5%
		FILENAME$ = "AD_REGUNIT"
		CONTINUE HelpError
	END WHEN

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************
	DATE_ITEM$ = DATE_TODAY

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_PURGE, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_PURGE, &
		"Purge Retired Asset for " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_PURGE, &
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
	!OPTION$ = SEG$(OPTLIST$, OPT%, INSTR(OPT%, OPTLIST$ + " ", " ") - 1%)

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

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
			"Item to change", 0.0, 4%, "##", "")

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

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
			"Item to Blank", 0.0, 4%, "##", "")

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
			LSET DATE_ITEM$ = DATE_TODAY

		END SELECT

		FLAG% = 1%
		GOSUB DataEntry

		GOTO Blankr

	CASE "G"
		GOSUB Purge
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

	DATA	6,20, "(01) To Date", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE, ATEXT$, XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry FOR LOOP% = 1% TO MAX_ITEM

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
	!	^*(01) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field causes the purging to end with
	!	the selected date.
	!	.b
	!	A blank setting causes the purging to end with the last date in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Date>Purge
	!	.x Purge>To Date
	!
	!--
		DATE_ITEM$ = ENTR_3DATE(SCOPE, SMG_PURGE, "6;45", "To Date", &
			DATE_ITEM$, FLAG%, "'E", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 Purge:
2000	!******************************************************************
	! Purge
	!******************************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_PURGE, &
		"", "Confirm purge - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE, "Asset # ", 9%, 5%)

	CALL ENTR_3MESSAGE(SCOPE, "Purging ...", 1%)

	RESET #AD_35ASSET.CH%

 GetNextRec:
2020	!
	! Main loop starts here
	!
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AD_35ASSET.CH%
	USE
		CONTINUE 2100 IF ERR = 11%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec IF EDIT$(AD_35ASSET::RET_DATE, -1%) = ""
	GOTO GetNextRec IF AD_35ASSET::RET_DATE > DATE_ITEM$

	SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE, AD_35ASSET::ASSET_NUM, 9%, 15%)

2030	!
	! Units File
	!
	WHEN ERROR IN
		GET #AD_REGUNIT.CH%, KEY #0% EQ AD_35ASSET::ASSET_NUM
		DELETE #AD_REGUNIT.CH%
	USE
		CONTINUE 2040 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_REGUNIT"
		CONTINUE HelpError
	END WHEN

	GOTO 2030

2040	!
	! History File
	!
	WHEN ERROR IN
		GET #AD_HISTORY.CH%, KEY #0% EQ AD_35ASSET::ASSET_NUM
		DELETE #AD_HISTORY.CH%
	USE
		CONTINUE 2050 IF ERR = 11% OR ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

	GOTO 2040

2050	!
	! Asset depreciation
	!
	WHEN ERROR IN
		GET #AD_DEPRECIATION.CH%, KEY #0% EQ AD_35ASSET::ASSET_NUM
		DELETE #AD_DEPRECIATION.CH%
	USE
		CONTINUE 2060 IF ERR = 11% OR ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

	GOTO 2050

2060	!
	! Asset depreciation
	!
	WHEN ERROR IN
		GET #AD_BALANCE.CH%, KEY #0% EQ AD_35ASSET::ASSET_NUM
		DELETE #AD_BALANCE.CH%
	USE
		CONTINUE 2070 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 2060

2070	!
	! Asset depreciation
	!
	WHEN ERROR IN
		DELETE #AD_35ASSET.CH%
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec

2100	SLEEP 1%
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
