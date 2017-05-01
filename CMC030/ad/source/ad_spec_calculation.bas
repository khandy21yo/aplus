1	%TITLE "Calculation Depreciation"
	%SBTTL "AD_SPEC_CALCULATION"
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
	!	The ^*Calculation Depreciation\* option
	!	calculates depreciation for a particular period.
	!	.b
	!	The Calculation process causeS a new Asset Ledger file to be
	!	created and updates the last period calculated field in the
	!	Object Controlling File for each calculated Object. During the
	!	calculation, the Status Flag is switched to a "3" and will reset
	!	back to zero when the calculation is complete. If different Objects
	!	have different accounting eras, a calculation routine must be
	!	accomplished for each Object separately. For Object's with the
	!	same accounting era, the depreciation calculation is accomplished
	!	simultaneously. If an Asset Depreciation Method, Convention, or
	!	Period is undefined, no depreciation will be calculated and the
	!	activity status will be X.
	!	.lm -5
	!
	! Index:
	!	.x Calculation Depreciation
	!	.x Period
	!	.x Asset Ledger
	!	.x Period
	!	.x Status Flag
	!	.x Accounting Era
	!	.x Accounting Era
	!	.x Depreciation Method
	!	.x Convention
	!	.x Period
	!	.x Activity Status
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_SPEC_CALCULATION/LINE
	!	$ LINK/EXE=AD_EXE: AD_SPEC_CALCULATION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_SPEC_CALCULATION.OBJ;*
	!
	! Author:
	!
	!	12/16/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	03/14/91 - Frank F. Starman
	!		Fixed problem 'Division by zero on line 2265'
	!
	!	03/19/92 - Dan Perkins
	!		Commented out OPTION$ which doesn't seem to be
	!		used for anything.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/10/94 - Kevin Handy
	!		Broke line 2210 into several lines so that I could
	!		figure out what file/statement was actually generating
	!		an error at KINGB.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Fix SMG_CALCUL to SMG_CALCUL%.
	!		Changed OINVPER to "OINVPER".
	!
	!	08/21/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	10/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer.
	!
	!	02/10/98 - Kevin Handy
	!		Reformat source code
	!
	!	02/10/98 - Kevin Handy
	!		Allow AMOUNT_CUR to go negative so that it they
	!		change the method it will adjust it to current.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/13/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	10/27/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	05/24/2001 - Kevin Handy
	!		If something has a retirement date on it, don't
	!		calculate it any more.
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map file
	!
	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.HB"
	MAP (AD_DEPRECIATION)	AD_DEPRECIATION_CDD	AD_DEPRECIATION

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD	AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS)	AD_DEPCLASS_CDD	AD_DEPCLASS

	%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.HB"
	MAP (AD_METHOD)	AD_METHOD_CDD	AD_METHOD

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP (AD_CONTROLOBJ)	AD_CONTROLOBJ_CDD	AD_CONTROLOBJ

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLE.HB"
	MAP (AD_TABLE)	AD_TABLE_CDD	AD_TABLE

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLEONE.HB"
	MAP (AD_TABLEONE)	AD_TABLEONE_CDD	AD_TABLEONE

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLETWO.HB"
	MAP (AD_TABLETWO)	AD_TABLETWO_CDD	AD_TABLETWO

	%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.HB"
	MAP (AD_REGUNIT)	AD_REGUNIT_CDD	AD_REGUNIT

	%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.HB"
	MAP (AD_BALANCE)	AD_BALANCE_CDD	AD_BALANCE

	%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.HB"
	MAP (AD_CALCULATION)	AD_CALCULATION_CDD	AD_CALCULATION

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILING.HB"
	MAP (AD_CEILING)	AD_CEILING_CDD	AD_CEILING

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGONE.HB"
	MAP (AD_CEILINGONE)	AD_CEILINGONE_CDD	AD_CEILINGONE

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGTWO.HB"
	MAP (AD_CEILINGTWO)	AD_CEILINGTWO_CDD	AD_CEILINGTWO

	!
	! Exter
	!
	EXTERNAL REAL FUNCTION AD_FUNC_CONVENTION
	EXTERNAL REAL FUNCTION AD_FUNC_METHOD

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 2%

	!
	! Declare constants
	!
	DECLARE LONG XLONG, YLONG

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("AD_CALCULATION", AD_CALCULATION.DEV$, STAT%)

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.OPN"
	USE
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
	USE
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.UPD"
	USE
		FILENAME$ = "AD_CONTROLOBJ"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.OPN"
	USE
		FILENAME$ = "AD_METHOD"
		CONTINUE HelpError
	END WHEN

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.OPN"
	USE
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_TABLEONE.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "AD_TABLEONE"
		CONTINUE HelpError
	END WHEN

370	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_TABLETWO.OPN"
	USE
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "AD_TABLETWO"
		CONTINUE HelpError
	END WHEN

380	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.OPN"
	USE
		CONTINUE 390 IF ERR = 5%
		FILENAME$ = "AD_REGUNIT"
		CONTINUE HelpError
	END WHEN

390	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.OPN"
	USE
		CONTINUE 410 IF ERR = 5%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

410	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_TABLE.OPN"
	USE
		CONTINUE 420 IF ERR = 5%
		FILENAME$ = "AD_TABLE"
		CONTINUE HelpError
	END WHEN

420	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILING.OPN"
	USE
		CONTINUE 430 IF ERR = 5%
		FILENAME$ = "AD_CEILING"
		CONTINUE HelpError
	END WHEN

430	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGONE.OPN"
	USE
		CONTINUE 500 IF ERR = 5%
		FILENAME$ = "AD_CEILINGONE"
		CONTINUE HelpError
	END WHEN

440	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGTWO.OPN"
	USE
		CONTINUE 500 IF ERR = 5%
		FILENAME$ = "AD_CEILINGTWO"
		CONTINUE HelpError
	END WHEN

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

	PERIOD_ITEM$ = "      "
	OBJECT_ITEM$ = "*" + SPACE$(19%)

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_CALCUL%, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_CALCUL%, &
		"Asset depreciation")

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_CALCUL%, &
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
			CALL ENTR_3MESSAGE(SCOPE, "Sorry, Unable to blank", 0%)

		CASE 2%
			LSET OBJECT_ITEM$ = "*" + SPACE$(19%)

		END SELECT

		FLAG% = 1%
		GOSUB DataEntry

		GOTO Blankr

	CASE "G"
		GOSUB Calcul
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

	DATA	6,20, "(01) Period ", &
		7,20, "(02) Object", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_CALCUL%, ATEXT$, XLONG, YLONG)
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
	!	^*(01) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field enters the period for which
	!	depreciation is to be calculated.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Period
	!	.x Period>Asset Depreciation
	!	.x Asset Depreciation>Period
	!
	!--


		PERIOD_ITEM$ = ENTR_PERIOD(SMG_CALCUL%, "6;45", &
			"Period ", &
			PERIOD_ITEM$, FLAG%, "", DEFLT$)

	CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Object\*
	!	.b
	!	.lm +5
	!	The ^*Object\* field enters all objects
	!	for which depreciation is to be calculated.
	!	.b
	!	The procedure for entry may include "wildcarding".
	!	.lm -5
	!
	! Index:
	!	.x Object>Asset Depreciation
	!	.x Asset Depreciation>Object
	!
	!--

		OBJECT_ITEM$ = ENTR_3STRING(SCOPE, SMG_CALCUL%, &
			"7;45", "Object", &
			OBJECT_ITEM$, FLAG%, "'E", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 Calcul:
2000	!
	! Calculate depreciation
	!

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_CALCUL%, &
		"", "Confirm depreciation - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	!
	! Exit if no confirmation
	!
	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

 !	WHEN ERROR IN
 !		KILL AD_CALCULATION.DEV$ + "AD_CALCULATION.LED" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE EndKill
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AD_CALCULATION.DEV$ + &
		"AD_CALCULATION.LED;*")

 ! EndKill:
	!
	! Create new depreciation ledger
	!
	%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.CRE"

	SMG_STATUS% = SMG$PUT_CHARS(SMG_CALCUL%, "Asset #    ", 12%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_CALCUL%, "Object     ", 13%, 5%)

	!
	! Display "Please wait..."
	!
	CALL ENTR_3MESSAGE(SCOPE, "Calculation Depreciation", 1% + 16%)

	!
	! Reset depreciation file by object key
	!
	RESET #AD_DEPRECIATION.CH%, KEY #1%

 GetNextRec:
2210	!
	! Main loop starts here
	! Read depraciation file
	!
	WHEN ERROR IN
		GET #AD_DEPRECIATION.CH%, REGARDLESS
	USE
		CONTINUE EndDep IF ERR = 11%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

	!
	! Check Object
	!
	GOTO GetNextRec &
		IF COMP_STRING(AD_DEPRECIATION::DEP_OBJECT, OBJECT_ITEM$) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_CALCUL%, AD_DEPRECIATION::ASSET_NUM, &
		12%, 22%)

	!
	! Do not calculate depreciation if invalid period for object
	! Add record in asset ledger and go to the next one
	!
	GOTO AddRecord IF INVALID_OBJ$ = AD_DEPRECIATION::DEP_OBJECT

	!
	! Initialize variables
	!
	AMOUNT_CUR = 0.0
	UNIT_CUR = 0.0
	DEP_STATUS$ = "A"
	PERIOD_CUR$ = PERIOD_ITEM$
	DISP_DATE$ = ""

	!
	! Check controlling record if a new object
	!
2211	GOTO 2215 UNLESS AD_DEPRECIATION::DEP_OBJECT <> DEP_OBJECT$

	IF DEP_OBJECT$ <> ""
	THEN
		!
		! Update control flag to zero
		!
		WHEN ERROR IN
			GET #AD_CONTROLOBJ.CH%, KEY #0% EQ DEP_OBJECT$
		USE
			FILENAME$ = "AD_CONTROLOBJ"
			CONTINUE HelpError
		END WHEN

		IF AD_CONTROLOBJ::STATUS_FLAG <> "4"
		THEN
			AD_CONTROLOBJ::STATUS_FLAG = "0"
			WHEN ERROR IN
				UPDATE #AD_CONTROLOBJ.CH%
			USE
				FILENAME$ = "AD_CONTROLOBJ"
				CONTINUE HelpError
			END WHEN
		END IF
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_CALCUL%, AD_DEPRECIATION::DEP_OBJECT, &
		13%, 22%)

2212	WHEN ERROR IN
		GET #AD_CONTROLOBJ.CH%, KEY #0% EQ AD_DEPRECIATION::DEP_OBJECT
	USE
		FILENAME$ = "AD_CONTROLOBJ"
		CONTINUE HelpError
	END WHEN

	IF READ_PERIOD("FIND", AD_CONTROLOBJ::ERA, PERIOD_CUR$, &
		"", STAT$, FROM_DATE$, TO_DATE$, 0%)
	THEN
		CALL HELP_34MESSAGE(SCOPE, "Invalid Period for Object " + &
			AD_DEPRECIATION::DEP_OBJECT + "(" + PERIOD_CUR$ + ")", &
			"W", SCOPE::PRG_PROGRAM, "", "OINVPER")
	!++
	! Warning:OINVPER
	!--
		INVALID_OBJ$ =  AD_DEPRECIATION::DEP_OBJECT

		AD_CONTROLOBJ::LASTDEP		= "??????"
		AD_CONTROLOBJ::STATUS_FLAG	= "4"

		WHEN ERROR IN
			UPDATE #AD_CONTROLOBJ.CH%
		USE
			FILENAME$ = "AD_CONTROLOBJ"
			CONTINUE HelpError
		END WHEN

		DEP_STATUS$ = "X"
		GOTO AddRecord
	END IF

	!
	! Set flag to 3 (calculation in running)
	!
2213	AD_CONTROLOBJ::LASTDEP		= PERIOD_CUR$
	AD_CONTROLOBJ::STATUS_FLAG	= "3"

	WHEN ERROR IN
		UPDATE #AD_CONTROLOBJ.CH%
	USE
		FILENAME$ = "AD_CONTROLOBJ"
		CONTINUE HelpError
	END WHEN

	V% = READ_PERIOD("READ", AD_CONTROLOBJ::ERA, &
		LEFT(PERIOD_CUR$, 4%), "", "", FIRST_DATE$, "", 0%)
	FIRST_MONTH% = VAL(MID(FIRST_DATE$, 5%, 2%))
	FIRST_MONTH% = FIRST_MONTH% + 1% &
		IF VAL(MID(FIRST_DATE$, 7%, 2%)) > 15%

2215	!
	! Read asset description file
	!
	WHEN ERROR IN
		GET #AD_35ASSET.CH%, &
			KEY #0% EQ AD_DEPRECIATION::ASSET_NUM, &
			REGARDLESS
	USE
		IF ERR = 155%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Undefined Asset#  " + &
				AD_DEPRECIATION::ASSET_NUM, 0%)
			CONTINUE 2400
		END IF
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

	SERV_DATE$ = AD_35ASSET::SERVDATE

	!
	! Check if service date is older then depreciation "to date"
	!
	GOTO GetNextRec IF TO_DATE$ < SERV_DATE$
	DEPREC_DATE$ = TO_DATE$

	IF TRM$(AD_35ASSET::RET_DATE) <> ""
	THEN
 !		IF AD_35ASSET::RET_DATE <= DEPREC_DATE$
 !		THEN
 !			DEPREC_DATE$ = AD_35ASSET::RET_DATE
 !			DISP_DATE$ = AD_35ASSET::RET_DATE
 !		END IF

		GOTO GetNextRec

	END IF

2220	!
	! Read depreciation class table
	!
	WHEN ERROR IN
		GET #AD_DEPCLASS.CH%, &
			KEY #0% EQ AD_DEPRECIATION::DEPCLASS, &
			REGARDLESS
	USE
		CONTINUE NoDepClass IF ERR = 155%
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

	REC_PER = VAL(AD_DEPCLASS::YEARS)

	BASIS = AD_35ASSET::COST
	SALVAGE = AD_35ASSET::SALVAGE
	BONUS = AD_35ASSET::BONUS
	ITC = AD_35ASSET::ITCREDUCE
	SERV_MONTH% = VAL(MID(AD_35ASSET::SERVDATE, 5%, 2%))

	!
	! Adjust basis if necessary
	!
	BASIS = BASIS - SALVAGE IF AD_DEPCLASS::SALVFACTOR = "Y"
	BASIS = BASIS - BONUS   IF AD_DEPCLASS::BONUSFACTOR = "Y"
	BASIS = BASIS - ITC IF AD_DEPCLASS::ITCFACTOR = "Y"

2240	AD_BALANCE::AMOUNT_CTD = 0.0
	IF PERIOD_CUR$ > AD_CONTROLOBJ::LASTPER AND &
		TRM$(AD_CONTROLOBJ::LASTPER) <> ""
	THEN
		!
		! Take prior depreciation amount
		!
		WHEN ERROR IN
			GET #AD_BALANCE.CH%, &
				KEY #0% EQ AD_DEPRECIATION::ASSET_NUM + &
				AD_DEPRECIATION::DEP_OBJECT, &
				REGARDLESS
		USE
			CONTINUE 2250 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AD_BALANCE"
			CONTINUE HelpError
		END WHEN
		GOTO GetNextRec IF AD_BALANCE::DEP_STATUS <> "A"
	END IF

2250	WHEN ERROR IN
		GET #AD_METHOD.CH%, &
			KEY #0% EQ AD_DEPCLASS::DEPMETHOD, &
			REGARDLESS
	USE
		IF ERR = 155%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Undefined method " + &
				AD_DEPCLASS::DEPMETHOD + " (" + &
				AD_DEPRECIATION::ASSET_NUM + " " + &
				AD_DEPRECIATION::DEP_OBJECT + ")", 0%)
			CONTINUE 2400
		END IF
		FILENAME$ = "AD_METHOD"
		CONTINUE HelpError
	END WHEN

	!
	! First year month
	!
	INIT_MONTH% = FIRST_MONTH% - SERV_MONTH%
	INIT_MONTH% = 12% + INIT_MONTH% IF INIT_MONTH% <= 0%

	DEP_MONTH% = VAL(MID(DEPREC_DATE$, 5%, 2%))
	DEP_MONTH% = DEP_MONTH% - 1% IF VAL(MID(DEPREC_DATE$, 7%, 2%)) <= 15%
	DEP_MONTH% = DEP_MONTH% - FIRST_MONTH% + 1%
	DEP_MONTH% = DEP_MONTH% + 12% IF DEP_MONTH% <= 0%

	RATE_ENDYEAR, RATE_PRIOR = 0.0

	SELECT AD_METHOD::CALCULATION

	!
	! Method based on Unit-of-Production depreciation method
	!
	CASE "12"
		TOTAL_UNITS = AD_35ASSET::UNITS
		PERIOD_UNIT$ = AD_CONTROLOBJ::LASTPER
		UNIT_CUR = 0.0

		WHILE PERIOD_UNIT$ < PERIOD_CUR$
			!
			! Figure out next period
			!
			V% = READ_PERIOD("NEXT", AD_CONTROLOBJ::ERA, &
				PERIOD_UNIT$, "", "", "", DEPREC_DATE$, 1%)

2260			!
			! Read units for this period
			!
			WHEN ERROR IN
				FIND #AD_REGUNIT.CH%, &
					KEY #0% EQ AD_DEPRECIATION::ASSET_NUM + &
					AD_DEPRECIATION::DEP_OBJECT + &
					PERIOD_UNIT$, &
					REGARDLESS
			USE
				CONTINUE 2265 IF ERR = 155% OR ERR = 9%
				FILENAME$ = "AD_REGUNIT"
				CONTINUE HelpError
			END WHEN

 NextUnits:
			WHEN ERROR IN
				GET #AD_REGUNIT.CH%, REGARDLESS
			USE
				CONTINUE 2265 IF ERR = 11% OR ERR = 9%
				FILENAME$ = "AD_REGUNIT"
				CONTINUE HelpError
			END WHEN

			GOTO 2265 &
				IF AD_REGUNIT::ASSET_NUM + &
				AD_REGUNIT::DEP_OBJECT <> &
				AD_DEPRECIATION::ASSET_NUM + &
				AD_DEPRECIATION::DEP_OBJECT

			UNIT_CUR = UNIT_CUR + AD_REGUNIT::QUANTITY
			GOTO NextUnits

2265		!
		! Next Period
		!
		NEXT

		RATE_ENDYEAR, RATE_PRIOR = UNIT_CUR / TOTAL_UNITS &
			IF TOTAL_UNITS <> 0.0

	CASE ELSE
2270		!
		! What depreciation year
		!
		DEP_YEAR% = 1%
		DEP_MON% = DATE_MONCODE(DEPREC_DATE$)
		DEP_MON% = DEP_MON% - 1% &
			IF VAL(MID(DEPREC_DATE$, 7%, 2%)) <= 15%
		TEST_CODE% = DATE_MONCODE(AD_35ASSET::SERVDATE) + 12.0
		WHILE TEST_CODE% <= DEP_MON%
		!AND DEP_YEAR% < REC_PER + 1.0
			DEP_YEAR% = DEP_YEAR% + 1%
			TEST_CODE% = TEST_CODE% + 12%
		NEXT

		INDEX% = 13% - INIT_MONTH%
		DEP_YEAR% = DEP_YEAR% + 1% &
			IF DEP_MONTH% < INDEX%

		IF AD_METHOD::CALCULATION = "01"
		THEN
			!
			! Method based on a optional table
			!
			TABLE% = 0%
			GOSUB 18100
			IF TABLE%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Undefined Optional Table " + &
					AD_DEPCLASS::OPTTABLE + " (" + &
					AD_DEPRECIATION::ASSET_NUM + " " + &
					AD_DEPRECIATION::DEP_OBJECT + ")", 0%)
				GOTO 2400
			END IF
		ELSE
			!
			! Service date is first or second half of the month ?
			!
			SERV_HALF% = 1%
			SERV_HALF% = 2% &
				IF VAL(MID(SERV_DATE$, 7%, 2%)) > 15%

			!
			! Go for convention coefficient
			!
			CONV = AD_FUNC_CONVENTION(AD_DEPCLASS::FYCONV, &
				2% * INIT_MONTH% - SERV_HALF%)

			IF DEP_YEAR% > 1%
			THEN
				RATE_PRIOR = AD_FUNC_METHOD( &
					AD_METHOD::CALCULATION, &
					AD_DEPCLASS::YEARS, &
					DEP_YEAR% * 1.0 - 1.0, CONV)
			END IF

			RATE_ENDYEAR = AD_FUNC_METHOD( &
				AD_METHOD::CALCULATION, &
				AD_DEPCLASS::YEARS, DEP_YEAR% * 1.0, CONV)

			SELECT AD_METHOD::CALCULATION
			!
			! Check only for declining method
			!
			CASE "03", "04", "05", "06"
				DEP_STATUS$ = "F" &
					IF DEP_STATUS$ <> "R" AND &
					DEP_MON% - DATE_MONCODE( &
					AD_35ASSET::SERVDATE) >= 12% * REC_PER
			END SELECT
		END IF

		END SELECT

2280	DCONV = 1.0
	IF DISP_DATE$ = DEPREC_DATE$
	THEN
		DEP_STATUS$ = "R"
		IF DEP_YEAR% >= REC_PER + 1.0 &
			OR (DEP_YEAR% = REC_PER AND CONV = 1.0)
		THEN
			DEP_MONTH% = 12%
			GOTO DepAmount
		END IF
		!
		! Disposal date is first or second half of the month ?
		!
		DISP_HALF% = 1%
		DISP_HALF% = 2% &
			IF VAL(MID(DISP_DATE$, 7%, 2%)) > 15%

		!
		! Go for retired convention coefficient
		!
		DCONV = AD_FUNC_CONVENTION(AD_DEPCLASS::DYCONV, &
			2% * DEP_MONTH% - DISP_HALF%)
		DEP_MONTH% = 12%
	END IF

 DepAmount:
2290	!
	! Current depreciation amount
	!
	IF DEP_YEAR% = 1%
	THEN
 !		IF INIT_MONTH% = 0%
 !		THEN
 !			AMOUNT_CUR = FUNC_ROUND(RATE_PRIOR * BASIS, 2%)
 !		ELSE
			AMOUNT_CUR = FUNC_ROUND(RATE_PRIOR * BASIS, 2%) + &
				FUNC_ROUND( &
				(DEP_MONTH% + INIT_MONTH% - 12.0) * &
				(RATE_ENDYEAR - RATE_PRIOR) * &
				BASIS / INIT_MONTH% * DCONV, 2%)
 !		END IF
	ELSE
		IF DEP_YEAR% > REC_PER AND TRM$(AD_DEPCLASS::CEILTABLE) = ""
		THEN
			DEP_MONTH% = 12.0 - INIT_MONTH% &
				IF DEP_MONTH% > 12.0 - INIT_MONTH%
			FRACT = 0.0
			FRACT = DEP_MONTH% / (12.0 - INIT_MONTH%) &
				IF INIT_MONTH% <> 12.0
			AMOUNT_CUR = FUNC_ROUND(RATE_PRIOR * BASIS, 2%) + &
				FUNC_ROUND(FRACT * (RATE_ENDYEAR - RATE_PRIOR) * &
				BASIS * DCONV, 2%)
		ELSE
			AMOUNT_CUR = FUNC_ROUND(RATE_PRIOR * BASIS, 2%) + &
				FUNC_ROUND(DEP_MONTH% / 12.0 * &
				(RATE_ENDYEAR - RATE_PRIOR) * &
				BASIS * DCONV, 2%)
		END IF
	END IF

	AMOUNT_CUR = AMOUNT_CUR - AD_BALANCE::AMOUNT_CTD

	!
	! Check unlogical cases
	!
 !	AMOUNT_CUR = 0. IF AMOUNT_CUR < 0.
	IF FUNC_ROUND(AMOUNT_CUR + AD_BALANCE::AMOUNT_CTD, 2%) > BASIS
	THEN
		AMOUNT_CUR = BASIS - AD_BALANCE::AMOUNT_CTD
	END IF


	IF AD_DEPCLASS::CEILTABLE <> ""
	THEN
		!
		! Read ceiling table
		!
		GOSUB 18200
		IF TABLE%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Undefined Ceiling" + &
			AD_DEPCLASS::CEILTABLE + " (" + &
			AD_DEPRECIATION::ASSET_NUM + " " + &
			AD_DEPRECIATION::DEP_OBJECT + ")", 0%)
			GOTO 2400
		END IF

		AMOUNT_CUR = CEILING &
			IF CEILING <> 0. AND CEILING<AMOUNT_CUR
	END IF

	DEP_STATUS$ = "F" IF DEP_STATUS$ <> "R" AND &
		FUNC_ROUND(AMOUNT_CUR + AD_BALANCE::AMOUNT_CTD - &
		BASIS, 2%) >= 0.0

 AddRecord:
2300	DEP_OBJECT$ = AD_DEPRECIATION::DEP_OBJECT

	AD_CALCULATION::ASSET_NUM	= AD_DEPRECIATION::ASSET_NUM
	AD_CALCULATION::DEP_OBJECT	= AD_DEPRECIATION::DEP_OBJECT
	AD_CALCULATION::DEP_STATUS	= DEP_STATUS$
	AD_CALCULATION::AMOUNT_CUR	= AMOUNT_CUR
	AD_CALCULATION::UNIT_CUR	= UNIT_CUR

	PUT #AD_CALCULATION.CH%

	GOTO GetNextRec

 EndDep:
	IF DEP_OBJECT$ <> ""
	THEN
		!
		! Update control flag to zero
		!
		GET #AD_CONTROLOBJ.CH%, KEY #0% EQ DEP_OBJECT$

		IF AD_CONTROLOBJ::STATUS_FLAG <> "4"
		THEN
			AD_CONTROLOBJ::STATUS_FLAG = "0"
			UPDATE #AD_CONTROLOBJ.CH%
		END IF
	END IF

	RETURN

2400	!
	! Update control flag '4' because unable to depreciate an asset
	!
	GET #AD_CONTROLOBJ.CH%, KEY #0% EQ AD_DEPRECIATION::DEP_OBJECT

	AD_CONTROLOBJ::STATUS_FLAG = "4"
	UPDATE #AD_CONTROLOBJ.CH%

	DEP_STATUS$ = "X"
	GOTO AddRecord

	%PAGE

18100	!
	! Read optional table
	!
	OPTTABLE$ = STRING$(LEN(AD_TABLE::OPTTABLE), A"?"B)
	YEAR$ = STRING$(LEN(AD_TABLE::YEARS), A"?"B)
	EFFDATE$ = STRING$(LEN(AD_TABLE::EFFDATE), A"?"B)
	DIMEN$ = STRING$(LEN(AD_TABLE::DIMEN), A"?"B)

	WHEN ERROR IN
		FIND #AD_TABLE.CH%, &
			KEY #0% EQ AD_DEPCLASS::OPTTABLE + &
			AD_DEPCLASS::YEARS, &
			REGARDLESS
	USE
		IF ERR = 155% OR ERR = 9%
		THEN
			TABLE% = -1%
			CONTINUE BackFromTable
		END IF
		FILENAME$ = "AD_TABLE"
		CONTINUE HelpError
	END WHEN

 NextOptTable:
	WHEN ERROR IN
		GET #AD_TABLE.CH%, REGARDLESS
	USE
		CONTINUE 18120 IF ERR = 11%
		IF ERR = 155% OR ERR = 9%
		THEN
			TABLE% = -1%
			CONTINUE BackFromTable
		END IF
		FILENAME$ = "AD_TABLE"
		CONTINUE HelpError
	END WHEN

	GOTO 18120 IF AD_DEPCLASS::OPTTABLE + AD_DEPCLASS::YEARS <> &
		AD_TABLE::OPTTABLE + AD_TABLE::YEARS

	GOTO 18120 IF AD_TABLE::EFFDATE > AD_35ASSET::SERVDATE

	OPTTABLE$ = AD_TABLE::OPTTABLE
	YEAR$ = AD_TABLE::YEARS
	EFFDATE$ = AD_TABLE::EFFDATE
	DIMEN$ = AD_TABLE::DIMEN

	GOTO NextOptTable

18120	SELECT DIMEN$

	!
	! Get percentage from one dim table
	!
	CASE "1"
		WHEN ERROR IN
			GET #AD_TABLEONE.CH%, &
				KEY #0% EQ OPTTABLE$ + YEAR$ + EFFDATE$ + "01", &
				REGARDLESS
		USE
			IF ERR = 155% OR ERR = 9%
			THEN
				TABLE% = -1%
				CONTINUE BackFromTable
			END IF
			FILENAME$ = "AD_TABLEONE"
			CONTINUE HelpError
		END WHEN

		RATE_YEAR = 0.0001 * AD_TABLEONE::PERCENTAGE
		RATE_ENDYEAR = RATE_YEAR

18130		FOR I% = 2% TO DEP_YEAR%
			WHEN ERROR IN
				GET #AD_TABLEONE.CH%, &
					KEY #0% EQ OPTTABLE$ + YEAR$ + EFFDATE$ + &
					FORMAT$(I%, "<0>#"), REGARDLESS
			USE
				CONTINUE 18140 IF ERR = 155%
				FILENAME$ = "AD_TABLEONE"
				CONTINUE HelpError
			END WHEN

			RATE_YEAR = 0.0001 * AD_TABLEONE::PERCENTAGE

18140			RATE_ENDYEAR = RATE_ENDYEAR + RATE_YEAR
		NEXT I%

		!
		! Get percentage from two dim table
		!
	CASE "2"

18150		WHEN ERROR IN
			GET #AD_TABLETWO.CH%, &
				KEY #0% EQ OPTTABLE$ + YEAR$ + EFFDATE$ + "01", &
				REGARDLESS
		USE
			IF ERR = 155% OR ERR = 9%
			THEN
				TABLE% = -1%
				CONTINUE BackFromTable
			END IF
			FILENAME$ = "AD_TABLETWO"
			CONTINUE HelpError
		END WHEN

		RATE_YEAR = 0.0001 * AD_TABLETWO::PERCENTAGE(INDEX% - 1%)
		RATE_ENDYEAR = RATE_YEAR

18160		FOR I% = 2% TO DEP_YEAR%
			WHEN ERROR IN
				GET #AD_TABLETWO.CH%, &
					KEY #0% EQ OPTTABLE$ + YEAR$ + EFFDATE$ + &
					FORMAT$(I%, "<0>#"), REGARDLESS
			USE
				CONTINUE 18170 IF ERR = 155%
				FILENAME$ = "AD_TABLETWO"
				CONTINUE HelpError
			END WHEN

			RATE_YEAR = 0.0001 * &
				AD_TABLETWO::PERCENTAGE(INDEX% - 1%)

18170			RATE_ENDYEAR = RATE_ENDYEAR + RATE_YEAR
		NEXT I%

	CASE ELSE
		TABLE% = -1%
	END SELECT

	RATE_PRIOR = RATE_ENDYEAR - RATE_YEAR

 BackFromTable:
	RETURN

18200	!
	! Read ceiling table
	!
	CEILTABLE$ = STRING$(LEN(AD_CEILING::CEILTABLE), A"?"B)
	EFFDATE$ = STRING$(LEN(AD_CEILING::EFFDATE), A"?"B)
	DIMEN$ = STRING$(LEN(AD_CEILING::DIMEN), A"?"B)

	WHEN ERROR IN
		FIND #AD_CEILING.CH%, &
			KEY #0% EQ AD_DEPCLASS::CEILTABLE, &
			REGARDLESS
	USE
		IF ERR = 155% OR ERR = 9%
		THEN
			TABLE% = -1%
			CONTINUE BackFromCeil
		END IF
		FILENAME$ = "AD_CEILING"
		CONTINUE HelpError
	END WHEN

 NextCeilTable:
	WHEN ERROR IN
		GET #AD_CEILING.CH%, REGARDLESS
	USE
		IF ERR = 155% OR ERR = 9%
		THEN
			TABLE% = -1%
			CONTINUE BackFromCeil
		END IF
		CONTINUE 18220 IF ERR = 11%
		FILENAME$ = "AD_CEILING"
		CONTINUE HelpError
	END WHEN


	GOTO 18220 IF AD_DEPCLASS::CEILTABLE <> AD_CEILING::CEILTABLE

	GOTO 18220 IF AD_CEILING::EFFDATE > AD_35ASSET::SERVDATE

	CEILTABLE$ = AD_CEILING::CEILTABLE
	EFFDATE$ = AD_CEILING::EFFDATE
	DIMEN$ = AD_CEILING::DIMEN

	GOTO NextCeilTable

18220	SELECT DIMEN$

	!
	! Get ceiling from one dim table
	!
	CASE "1"

		WHEN ERROR IN
			GET #AD_CEILINGONE.CH%, &
				KEY #0% EQ CEILTABLE$ + EFFDATE$ + "01", &
				REGARDLESS
		USE
			IF ERR = 155% OR ERR = 9%
			THEN
				TABLE% = -1%
				CONTINUE BackFromCeil
			END IF
			FILENAME$ = "AD_CEILINGONE"
			CONTINUE HelpError
		END WHEN

		CEILING = AD_CEILINGONE::CEILING

18230		FOR I% = 2% TO DEP_YEAR%
			WHEN ERROR IN
				GET #AD_CEILINGONE.CH%, &
					KEY #0% EQ CEILTABLE$ + EFFDATE$ + &
					FORMAT$(I%, "<0>#"), &
					REGARDLESS
			USE
				CONTINUE 18240 IF ERR = 155%
				FILENAME$ = "AD_CEILINGONE"
				CONTINUE HelpError
			END WHEN

18240			CEILING = AD_CEILINGONE::CEILING
		NEXT I%

		!
		! Get ceiling from two dim table
		!
	CASE "2"

18250		WHEN ERROR IN
			GET #AD_CEILINGTWO.CH%, &
				KEY #0% EQ CEILTABLE$ + YEAR$ + EFFDATE$ + "01", &
				REGARDLESS
		USE
			IF ERR = 155% OR ERR = 9%
			THEN
				TABLE% = -1%
				CONTINUE BackFromCeil
			END IF
			FILENAME$ = "AD_CEILINGTWO"
			CONTINUE HelpError
		END WHEN

		CEILING = AD_CEILINGTWO::CEILING(INDEX% - 1%)

18260		FOR I% = 2% TO DEP_YEAR%
			WHEN ERROR IN
				GET #AD_CEILINGTWO.CH%, &
					KEY #0% EQ CEILTABLE$ + YEAR$ + EFFDATE$ + &
					FORMAT$(I%, "<0>#"), REGARDLESS
			USE
				CONTINUE 18270 IF ERR = 155%
				FILENAME$ = "AD_CEILINGTWO"
				CONTINUE HelpError
			END WHEN

18270			CEILING = AD_CEILINGTWO::CEILING(INDEX% - 1%)
		NEXT I%

	CASE ELSE
		TABLE% = -1%
	END SELECT

 BackFromCeil:
	RETURN

 NoDepClass:
	CALL HELP_34MESSAGE(SCOPE, "Undefined Dep Class " + &
		AD_DEPRECIATION::DEPCLASS + " (" + &
		TRM$(AD_DEPRECIATION::ASSET_NUM) + "," + &
		AD_DEPRECIATION::DEP_OBJECT + ")", "W", ERN$, "", "UDFDEPCLASS")
	!++
	! Warning:UDFDEPCLASS
	!	^*Undefined Depreciation Class\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	Depreciation class for an asset is blank or doesn't
	!	exist in the depreciation class table.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Correct the depreciation class in the Asset Depreciation
	!	file and calculate depreciation again.
	!	.lm +5
	!
	! Index:
	!	.x Depreciation Class
	!
	!--

	GOTO 2400

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR) + "[" + &
		TRM$(AD_DEPRECIATION::ASSET_NUM) + "]" + NUM1$(INIT_MONTH%), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
