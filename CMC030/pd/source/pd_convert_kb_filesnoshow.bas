1	%TITLE "Convert King B files"
	%SBTTL "PD_CONVERT_KB_FILESNOSHOW"
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
	!	$ BAS PD_SOURCE:PD_CONVERT_KB_FILESNOSHOW/LINE
	!	$ LINK/EXECUTABLE=PD_EXE: PD_CONVERT_KB_FILESNOSHOW, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_CONVERT_KB_FILESNOSHOW.OBJ;*
	!
	! Author:
	!	07/08/91 - Val James Allen
	!
	! Modification history:
	!
	!	10/04/93 - Kevin Handy
	!		Changed "=>" to ">=".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/27/96 - Kevin Handy
	!		Clean up source code.
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

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

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

	IF MASK$ = "<END>"
	THEN
		GOSUB BuildFiles
		GOSUB InitializeFiles
		GOTO 1420
	END IF

	IF MASK$ = "<REAL.DESC>"
	THEN
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

		GOTO 1420
	END IF


	IF MASK$ = "<PRICE-2>"
	THEN
		PD_PRODUCT::DESCRIPTION = LINE$
		GOTO 1420
	END IF

	IF MASK$ = "<TYPE>"
	THEN
		PD_PRODUCT::PROD_TYPE = LINE$
		GOTO 1420
	END IF

	IF MASK$ = "<PRICE.GROUP>"
	THEN
		PD_PRODUCT::CATEGORY = LEFT$(LINE$, 4%)
	END IF

	IF MASK$ = "<UN>"
	THEN
		PD_PRODUCT::UOM = LEFT$(LINE$, 2%)
		GOTO 1420
	END IF

	IF MASK$ = "<ON-HAND>"
	THEN
		IC_35BALANCE::BBALANCE = VAL(LINE$)
		ONHAND = VAL(LINE$)
		GOTO 1420
	END IF

	IF MASK$ = "<MIN>"
	THEN
		IC_BINMAP::SAFETY = VAL(LINE$)
		MINLEVEL = VAL(LINE$)
		GOTO 1420
	END IF

	IF MASK$ = "<MAX>"
	THEN
		IC_BINMAP::MAXLEVEL = VAL(LINE$)
		MAXLEVEL = VAL(LINE$)
		GOTO 1420
	END IF

	IF MASK$ = "<REP-COST>"
	THEN
		PC_COST::COST = (VAL(LINE$)/100)
		GOTO 1420
	END IF

	IF MASK$ = "<PRICE>"
	THEN
		PC_PRICE::PRICECOST = (VAL(LINE$)/100)
		GOTO 1420
	END IF

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
1450	RETURN IF PRODUCT$ = "" OR LOCATION$ = ""

	GOTO 1460 IF TYPEFLAG$ = "O"

	PD_PRODUCT::PRODUCT_NUM = PRODUCT$

	PUT #PD_PRODUCT.CH%

	RETURN

1460	IC_35BALANCE::PRODUCT = PRODUCT$
	IC_35BALANCE::LOCATION = LOCATION$
	IC_BINMAP::PRODUCT = PRODUCT$
	IC_BINMAP::LOCATION = LOCATION$
	PC_COST::PRODUCT = PRODUCT$
	PC_COST::LOCATION = LOCATION$
	PC_PRICE::PRODUCT_NUM = PRODUCT$
	PC_PRICE::LOCATION = LOCATION$

	GOTO 1470 IF IC_35BALANCE::BBALANCE = 0.0

	FIND #IC_35BALANCE.CH%, KEY#0% EQ IC_35BALANCE::PRODUCT + &
		IC_35BALANCE::LOCATION + IC_35BALANCE::TRANSTYPE, &
		REGARDLESS

	GET #IC_35BALANCE.CH%

	IC_35BALANCE::BBALANCE = IC_35BALANCE::BBALANCE + ONHAND

	UPDATE #IC_35BALANCE.CH%

	GOTO 1470

1465	PUT #IC_35BALANCE.CH%

1470	GOTO 1480 IF IC_BINMAP::SAFETY = 0.0 AND IC_BINMAP::MAXLEVEL = 0.0

	FIND #IC_BINMAP.CH%, &
		KEY#0% EQ IC_BINMAP::PRODUCT + IC_BINMAP::LOCATION, REGARDLESS

	GET #IC_BINMAP.CH%

	IC_BINMAP::SAFETY = IC_BINMAP::SAFETY + MINLEVEL

	IC_BINMAP::MAXLEVEL = IC_BINMAP::MAXLEVEL + MAXLEVEL

	UPDATE #IC_BINMAP.CH%

	GOTO 1480

1475	PUT #IC_BINMAP.CH%

1480	IF PC_COST::COST <> 0.0
	THEN
		PUT #PC_COST.CH%
	END IF
1490	IF PC_PRICE::PRICECOST <> 0.0
	THEN
		PUT #PC_PRICE.CH%
	END IF

1499	RETURN

1500	GOSUB BuildFiles

	! Put file PRODUCT in a list of completed converions


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
		IF ERR = 134%
		THEN
			RESUME 1490
		END IF
		FILENAME$ = "PC_COST"

	CASE 1490%
		IF ERR = 134%
		THEN
			RESUME 1499
		END IF
		FILENAME$ = "PC_PRICE"

	END SELECT

	RESUME HelpError

32767	END
