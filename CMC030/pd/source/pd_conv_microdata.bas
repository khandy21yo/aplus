1	%TITLE "Convert MicroData Product Files"
	%SBTTL "PD_CONV_MICRODATA"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_CONV_MICRODATA/LINE
	!	$ LINK/EXECUTABLE=PD_EXE: PD_CONV_MICRODATA,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_CONV_MICRODATA.OBJ;*
	!
	! Author:
	!	07/08/91 - Val James Allen
	!
	! Modification history:
	!
	!	08/23/91 - Dan Perkins
	!		Changed MASK$ IF-THEN to case statements
	!		Realigned to better match record
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/04/93 - Kevin Handy
	!		Changed "=<" to "<=".
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

10	ON ERROR GOTO 19000

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)	PC_PRICE_CDD	PC_PRICE

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)	PC_COST_CDD	PC_COST

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)	IC_BINMAP_CDD	IC_BINMAP

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY( 20%, 80%, DISPLAY_ID%,,, )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_ID%)
	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( DISPLAY_ID%, PASTE_ID%, &
		1%, 1% )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%
	SMG_STATUS% = SMG$SET_CURSOR_MODE(PASTE_ID%, SMG$M_CURSOR_OFF)

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, "Convert King B files", &
		2%, 15%,SMG$M_BOLD)
	SMG_STATUS% = SMG$DRAW_LINE(DISPLAY_ID%, 4%, 1%, 4%, 80%, &
		SMG$M_BOLD)

 ConfirmConv:	! Ask user if they realy would like to convert all files

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Convert files :", 9%, 20%)
	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
		"9;36", "Confirm Converting", "N", 16%, "'", "N"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO ConfirmConv	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, &
		SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		GOTO ConfirmConv

	END SELECT

	SMG_STATUS% = SMG$DELETE_CHARS(DISPLAY_ID%, 30%, 9%, 20%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	GOTO ExitProgram IF CONF$ <> "Y"

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "File   :", 18%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Record :", 19%, 4%)

	! Tell user the file we are converting is Product
	!
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "PRODUCT ",18%, 15%)

	!
	! Open the needed files
	!
1405	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.CRE"

1406	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.CRE"

1407	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"

1408	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"

1409	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.CRE"

1410	CALL ASSG_CHANNEL(CASE_TEXT.CH%,STAT%)
	CASE_TEXT.DEV$ = ""
	CASE_TEXT.NAME$ = CASE_TEXT.DEV$+"PROD.ASC"

	OPEN CASE_TEXT.NAME$ FOR INPUT AS FILE CASE_TEXT.CH%, &
		ORGANIZATION SEQUENTIAL

	GOSUB InitializeFiles


 GetNextCaseRec:
1420	! Start the actual conversion
	LINPUT #CASE_TEXT.CH%, LINE$


1430	!
	! Build conversion record - basic field
	!
	X% = POS(LINE$,">", 1%)

	GOTO 1420 IF X% <= 1%

	MASK$ = LEFT$(LINE$, X%)

	LINE$ = RIGHT$(LINE$, X% + 1%)

	SELECT MASK$

	CASE "<END>"
		GOSUB BuildFiles
		GOSUB InitializeFiles

	CASE "<REAL.DESC>"
		LOCATION$ = MID$(LINE$, 5%, 3%)
		LINE$ = RIGHT$(LINE$, 9%)
		Y% = POS(LINE$, "!", 1%)
		IF Y% = 0%
		THEN
			TYPEFLAG$ = "M"
			PRODUCT$ = LINE$
		ELSE
			TYPEFLAG$ = "O"
			PRODUCT$ = LEFT$(LINE$, Y% - 1%)
		END IF

	CASE "<TYPE>"
		PD_PRODUCT::PROD_TYPE = LINE$

	CASE "<PRICE.GROUP>"
		PD_PRODUCT::CATEGORY = LEFT$(LINE$, 4%)

	CASE "<PRICE>"
		PC_PRICE::PRICECOST = (VAL(LINE$)/1000)

	CASE "<PRICE-2>"
		PD_PRODUCT::DESCRIPTION = LINE$

	CASE "<UN>"
		PD_PRODUCT::UOM = LEFT$(LINE$, 2%)

	CASE "<REP-COST>"
		PC_COST::COST = (VAL(LINE$)/100)

	CASE "<MAX>"
		IC_BINMAP::MAXLEVEL = VAL(LINE$)
		MAXLEVEL = VAL(LINE$)

	CASE "<ON-HAND>"
		IC_35BALANCE::BBALANCE = VAL(LINE$)
		ONHAND = VAL(LINE$)

	CASE "<MIN>"
		IC_BINMAP::SAFETY = VAL(LINE$)
		MINLEVEL = VAL(LINE$)

	END SELECT

	GOTO 1420

 InitializeFiles:

		TYPEFLAG$ = ""
		PRODUCT$ = ""
		LOCATION$ = ""
		ONHAND = 0.0
		MINLEVEL = 0.0
		MAXLEVEL = 0.0
		!
		! Set basic fields for product file
		!
		PD_PRODUCT::PRODUCT_NUM		= ""
		PD_PRODUCT::DESCRIPTION		= ""
		PD_PRODUCT::PROD_TYPE		= ""
		PD_PRODUCT::CATEGORY		= ""
		PD_PRODUCT::UOM			= ""
		PD_PRODUCT::PACK		= ""
		PD_PRODUCT::LABEL		= ""
		PD_PRODUCT::METHOD		= "STD"
		PD_PRODUCT::BDATE		= "19910101"
		PD_PRODUCT::SSTATUS		= "A"
		PD_PRODUCT::EDATE		= ""
		PD_PRODUCT::SECONDARY_CODE	= ""

		IC_35BALANCE::PRODUCT		= ""
		IC_35BALANCE::LOCATION		= ""
		IC_35BALANCE::TRANSTYPE		= "CC"
		IC_35BALANCE::BBALANCE		= 0.0
		IC_35BALANCE::PBALANCE		= 0.0
		IC_35BALANCE::RBALANCE		= 0.0

		IC_BINMAP::PRODUCT		= ""
		IC_BINMAP::LOCATION		= ""
		IC_BINMAP::BIN(0%)		= ""
		IC_BINMAP::BIN(1%)		= ""
		IC_BINMAP::BIN(2%)		= ""
		IC_BINMAP::BIN(3%)		= ""
		IC_BINMAP::SAFETY		= 0.0
		IC_BINMAP::MAXLEVEL		= 0.0
		IC_BINMAP::ABC			= ""
		IC_BINMAP::CYCLEMAP		= ""

		PC_COST::PRODUCT		= ""
		PC_COST::LOCATION		= ""
		PC_COST::EFFDATE		= "19910101"
		PC_COST::COST			= 0.0

		PC_PRICE::PRODUCT_NUM		= ""
		PC_PRICE::LOCATION		= ""
		PC_PRICE::PCTYPE		= ""
		PC_PRICE::XDATE			= "19910101"
		PC_PRICE::XTIME			= ""
		PC_PRICE::PRICECOST		= 0.0

	RETURN


 BuildFiles:

1450		RETURN IF PRODUCT$ = "" OR LOCATION$ = ""
		GOTO 1460 IF TYPEFLAG$ = "O"
		PD_PRODUCT::PRODUCT_NUM = PRODUCT$
		SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "M " + &
			PD_PRODUCT::PRODUCT_NUM + &
			"                                                    ",19%,15%)
		PUT #PD_PRODUCT.CH%
	RETURN

1460		SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "O " + PRODUCT$ + &
			" " + LOCATION$ + &
			"                                             ", &
			19%,15%)

		IC_35BALANCE::PRODUCT = PRODUCT$
		IC_35BALANCE::LOCATION = LOCATION$
		IC_BINMAP::PRODUCT = PRODUCT$
		IC_BINMAP::LOCATION = LOCATION$
		PC_COST::PRODUCT = PRODUCT$
		PC_COST::LOCATION = LOCATION$
		PC_PRICE::PRODUCT_NUM = PRODUCT$
		PC_PRICE::LOCATION = LOCATION$

		GOTO 1470 IF IC_35BALANCE::BBALANCE = 0.0

		FIND #IC_35BALANCE.CH%, KEY#0% EQ IC_35BALANCE::PRODUCT + &
				IC_35BALANCE::LOCATION + &
				IC_35BALANCE::TRANSTYPE, &
				REGARDLESS

		GET #IC_35BALANCE.CH%

		IC_35BALANCE::BBALANCE = IC_35BALANCE::BBALANCE + ONHAND

		UPDATE #IC_35BALANCE.CH%

		GOTO 1470

1465		PUT #IC_35BALANCE.CH%

1470		GOTO 1480 IF IC_BINMAP::SAFETY = 0.0 AND &
			IC_BINMAP::MAXLEVEL = 0.0

		FIND #IC_BINMAP.CH%, KEY#0% EQ IC_BINMAP::PRODUCT + &
			IC_BINMAP::LOCATION, REGARDLESS

		GET #IC_BINMAP.CH%

		IC_BINMAP::SAFETY = IC_BINMAP::SAFETY + MINLEVEL
		IC_BINMAP::MAXLEVEL = IC_BINMAP::MAXLEVEL + MAXLEVEL

		UPDATE #IC_BINMAP.CH%

		GOTO 1480

1475		PUT #IC_BINMAP.CH%

1480		PUT #PC_COST.CH% IF PC_COST::COST <> 0.0

1490		PUT #PC_PRICE.CH% IF PC_PRICE::PRICECOST <> 0.0

1499	RETURN


1500	GOSUB BuildFiles


	! Put file PRODUCT in a list of completed converions

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "PRODUCT", 9%, 14%)

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	IF CONF$ = "Y"
	THEN
		CALL ENTR_3MESSAGE(SCOPE,"Conversion Process Complete", 0%)
	ELSE
		CALL ENTR_3MESSAGE(SCOPE,"Aborting Conversion Process", 0%)
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "",1%)
	SMG_STATUS% = SMG$SET_CURSOR_MODE(PASTE_ID%, SMG$M_CURSOR_ON)
	SMG_STATUS% = SMG$DELETE_PASTEBOARD(PASTE_ID%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(DISPLAY_ID%)

	GOTO 32767

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	CONF$ = "N"
	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************


	FILENAME$ = ""
	SELECT ERL

	CASE 1405%
		FILENAME$ = "PD_PRODUCT"

	CASE 1420%
		RESUME 1500 IF ERR = 11%
		FILENAME$ = "PRODUCT_INPUT"

	CASE 1450%
		IF ERR = 134%
		THEN
		SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "DUPLICATE KEY", &
			19%,40%)
		RESUME 1499
		END IF

		FILENAME$ = "PD_PRODUCT"

	CASE 1460%
		RESUME 1465 IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"

	CASE 1470%
		RESUME 1475 IF ERR = 155%
		FILENAME$ = "IC_BINMAP"

	CASE 1480%
		FILENAME$ = "PC_COST"

	CASE 1490%
		FILENAME$ = "PC_PRICE"

	END SELECT

	RESUME HelpError

32767	END
