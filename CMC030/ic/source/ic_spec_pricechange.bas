1	%TITLE "Inventory Price Change"
	%SBTTL "IC_SPEC_PRICECHANGE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Inventory Price Change\* option
	!	makes rapid changes in product cost/prices.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_PRICECHANGE/LINE
	!	$ LINK/EXECUTABLE=IC_EXE: IC_SPEC_PRICECHANGE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_PRICECHANGE.OBJ;*
	!
	! Author:
	!
	!	04/24/92 - Dan Perkins & Frank Starman
	!
	! Modification history:
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	05/27/92 - Dan Perkins
	!		Moved FACTOR variable out of loop so it
	!		would remain constant instead of changing with
	!		every record.
	!
	!	05/28/92 - Frank F. Starman
	!		Fix some problems causing by incorrect date
	!		in PC_READ_... functions.
	!
	!	06/15/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/14/96 - Kevin Handy
	!		Modified so that edining in seconf column of
	!		prices is not one off the display.
	!
	!	05/04/96 - Kevin Handy
	!		Modified so that pressing the up arrow to go
	!		back to "Percentage Change" field will display
	!		the original number, not the modified number.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)		PC_COST_CDD		PC_COST

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)		PC_PRICE_CDD		PC_PRICE

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP (PC_PRCTYPE)	PC_PRCTYPE_CDD		PC_PRCTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)	UTL_PROFILE_CDD		UTL_PROFILE

	MAP (DP_OUTP_XUNSOL)				RRR_FLAG%

	!
	! Declare variables
	!
	DECLARE LONG   SMG_PRICECHANGE
	DECLARE LONG   SMG_WIN2
	DECLARE LONG   SMG_WIN3
	DECLARE STRING PRICETYPE(10%)
	DECLARE STRING PRICEDESC(10%)
	DECLARE REAL   PRICE(10%)

	!
	! External functions
	!
	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL REAL	FUNCTION PC_READ_PRICE
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION UTL_EXAM_LOCATION
	EXTERNAL LONG	FUNCTION OUTP_UNSOLICITED

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open Product File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Location File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Cost File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"
	USE
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Price File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.CRE"
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Price File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

350	!
	! Open Profile to get Default Location
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
		CALL ASSG_FREECHANNEL(UTL_PROFILE.CH%)
	USE
		CONTINUE MakeWindow IF ERR = 5% OR ERR = 155%
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

 MakeWindow:
	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		13%, &
		70%, &
		SMG_PRICECHANGE, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_PRICECHANGE, &
		"Inv Product Price Change for " + TRM$(SCOPE::PRG_COMPANY))

	SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, &
		"Sort By (C,D,P,T,S)", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, "From Item", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, "To Item", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, "Wildcard", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, "Location", 6%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, "Base Date", 7%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, "Eff Date", 8%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_PRICECHANGE, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

	SORTBY$    = "P"
	FROM_ITEM$ = SPACE$(10%)
	TO_ITEM$   = SPACE$(10%)
	WLDCRD$    = SPACE$(20%)

	LOCATION$  = UTL_PROFILE::DEFLOCATION

	V% = UTL_EXAM_LOCATION(LOCATION$, UTL_LOCATION_EXAM)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, &
		LOCATION$, 6%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, &
		UTL_LOCATION_EXAM::LOCNAME, 6%, 35%, , SMG$M_BOLD)

	EFF_DATE$, BASE_DATE$ = DATE_TODAY

	OPT$ = "C"
	SCOPE::PRG_IDENT    = "H"
	SCOPE::PRG_PROGRAM = "IC_SPEC_PRICECHANGE"
	GOTO SelectOption

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	!
	! Enter options
	!
	SCOPE::PRG_IDENT   = "H"
	SCOPE::PRG_ITEM    = "HELP"
	SCOPE::PRG_PROGRAM = "IC_SPEC_PRICECHANGE"

	OPTLIST$ = "Change Auto Manual Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control C
	!
	CASE 3%
		GOTO 1000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

 SelectOption:
	SELECT OPT$

	!
	! Call the help message
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", &
			SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, &
			"", "HELP")
		GOTO 1000

	CASE "C"

 Sortby:
		SCOPE::PRG_ITEM   = "FLD01SORT"
	!++
	! Abstract:FLD01SORT
	!	^*Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field selects
	!	the order in which the report is to be printed.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*S\* - Product Secondary Code
	!	.te
	!	^*T\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		SORTBY$ = ENTR_3STRING(SCOPE, SMG_PRICECHANGE, "2;25", &
			"Sort By", SORTBY$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Sortby

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

		SELECT SORTBY$

		CASE "C"
			SORT_KEY% = 2%

		CASE "D"
			SORT_KEY% = 3%

		CASE "P"
			SORT_KEY% = 0%

		CASE "S"
			SORT_KEY% = 4%

		CASE "T"
			SORT_KEY% = 1%

		END SELECT

 FromItem:
		SCOPE::PRG_ITEM   = "FLD02FROM"
	!++
	! Abstract:FLD02FROM
	!	^*From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	program to begin with a selected item number.
	!	.b
	!	A blank field will cause the program to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
		FROM_ITEM$ = ENTR_3STRING(SCOPE, SMG_PRICECHANGE, "3;25", &
			"From Item", FROM_ITEM$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Sortby

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 ToItem:
		SCOPE::PRG_ITEM   = "FLD03TO"
	!++
	! Abstract:FLD03TO
	!	^*To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes
	!	the program to end with a selected item number.
	!	.b
	!	A blank setting will cause the program to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
		TO_ITEM$ = ENTR_3STRING(SCOPE, SMG_PRICECHANGE, "4;25", &
			"To Item", TO_ITEM$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO FromItem

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 Wildcard:
		SCOPE::PRG_ITEM   = "FLD04WILDCARD"
	!++
	! Abstract:FLD04WILDCARD
	!	^*Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a "wildcard" value.
	!	.lm -5
	!
	! Index:
	!
	!--
		WLDCRD$ = ENTR_3STRING(SCOPE, SMG_PRICECHANGE, "5;25", &
			"Wildcard", WLDCRD$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO ToItem

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT


1100		SCOPE::PRG_ITEM   = "FLD05LOCATION"
	!++
	! Abstract:FLD05LOCATION
	!	^*Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field selects a designated
	!	product location for which the prices or cost will be changed.
	!	.b
	!	The ^*Location\* will default to the company profile location
	!	if no location is entered by the user.
	!	.b
	!	^*NOTE:\* The company profile location is the ^*default\*
	!	location, which means prices and costs will be changed for
	!	^*all locations\*, if no other locations are defined.  Use
	!	caution when entering a location for this reason.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		LOCATION$ = ENTR_3STRING(SCOPE, SMG_PRICECHANGE, "6;25", &
			"Location", LOCATION$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT
		!
		! List Choices
		!
		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM   = "FLD05LOCATION"

			IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
			THEN
				LOCATION$ = UTL_LOCATION::LOCATION
			END IF

			GOTO 1100

		CASE SMG$K_TRM_UP
			GOTO Wildcard

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

		V% = UTL_EXAM_LOCATION(LOCATION$, UTL_LOCATION_EXAM)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, &
			LOCATION$, 6%, 25%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_PRICECHANGE, &
			UTL_LOCATION_EXAM::LOCNAME, 6%, 35%, , SMG$M_BOLD)

 Basedate:

		SCOPE::PRG_ITEM   = "FLD06BASEDATE"
	!++
	! Abstract:FLD06BASEDATE
	!	^*Base Date\*
	!	.b
	!	.lm +5
	!	The ^*Base Date\* field is the date on which changes
	!	will be calculated.  This date will default to today's
	!	date but can be changed to any date the user wishes.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		BASE_DATE$ = ENTR_3DATE(SCOPE, SMG_PRICECHANGE, "7;25", &
			"Base Date", BASE_DATE$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO 1100

		CASE SMG$K_TRM_DOWN
			GOTO Effdate

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 Effdate:
		SCOPE::PRG_ITEM   = "FLD07EFFDATE"
	!++
	! Abstract:FLD07EFFDATE
	!	^*Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field enters the date
	!	that the price or cost changes become effective.  The date
	!	will default to today's date but can be changed to any
	!	date the user wishes.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		EFF_DATE$ = ENTR_3DATE(SCOPE, SMG_PRICECHANGE, "8;25", &
			"Eff Date", EFF_DATE$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Basedate

		CASE SMG$K_TRM_DOWN
			GOTO Effdate

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT


	CASE "A"
		GOSUB Win3
		GOTO 1000

	CASE "M"
		GOSUB Win2
		GOTO 1000

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 1000

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 Win2:
2000	!*******************************************************************
	! Set Up Second Window
	!*******************************************************************

	WHEN ERROR IN
		RESET #PC_PRCTYPE.CH%
	USE
		CONTINUE CreateWin2 IF ERR = 9% OR ERR = 11%
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

	LOOP% = 0%

 GetPricetype:
	WHEN ERROR IN
		GET #PC_PRCTYPE.CH%, REGARDLESS
	USE
		CONTINUE CreateWin2 IF ERR = 9% OR ERR = 11%
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

	GOTO CreateWin2 IF LOOP% = 10%

	LOOP% = LOOP% + 1%
	PRICETYPE(LOOP%) = PC_PRCTYPE::CODE
	PRICEDESC(LOOP%) = LEFT(PC_PRCTYPE::DESCRIPTION, 18%)

	GOTO GetPriceType

 CreateWin2:
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		13%, &
		70%, &
		SMG_WIN2, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN2, "Manual Price Change")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Product #", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Description", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Prod Type", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Prod Cat", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Secondary Code", 6%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Cost", 8%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WIN2, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

3000	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE OutaWin2 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
3020	!
	! Main loop starts here
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE OutaWin2 IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	!GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORTBY$

	CASE "C"
		GOTO OutaWin2 IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO OutaWin2 IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO OutaWin2 IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO OutaWin2 IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO OutaWin2 IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Put the junk on the screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::PRODUCT_NUM, &
		2%, 20%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::DESCRIPTION, &
		3%, 20%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::PROD_TYPE, &
		4%, 20%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::CATEGORY, &
		5%, 20%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::SECONDARY_CODE, &
		6%, 20%, , SMG$M_BOLD)

	FOR LOOP% = 1% TO 5%

		PRICE(LOOP%) = PC_READ_PRICE &
			(PD_PRODUCT::PRODUCT_NUM, LOCATION$, &
			PRICETYPE(LOOP%), BASE_DATE$, "", "", "")

		IF PRICE(LOOP%) <> 0.0
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
				PRICEDESC(LOOP%), &
				LOOP% + 8%, 2%, ,)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
				FORMAT$(PRICE(LOOP%), "#,###,###.###"), &
				LOOP% + 8%, 20%, , SMG$M_BOLD)

		ELSE
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, SPACE$(35%), &
				LOOP% + 8%, 2%, ,)
		END IF

	NEXT LOOP%

	FOR LOOP% = 6% TO 10%

		PRICE(LOOP%) = PC_READ_PRICE &
			(PD_PRODUCT::PRODUCT_NUM, LOCATION$, &
			PRICETYPE(LOOP%), BASE_DATE$, "", "", "")

		IF PRICE(LOOP%) <> 0.0
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
				PRICEDESC(LOOP%), &
				LOOP% + 2%, 40%, ,)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
				FORMAT$(PRICE(LOOP%), "#,###,###.###"), &
				LOOP% + 2%, 58%, , SMG$M_BOLD)
		ELSE
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, SPACE$(35%), &
				LOOP% + 2%, 40%, ,)
		END IF

	NEXT LOOP%

	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, LOCATION$, &
		BASE_DATE$, JUNK$)

3030	SCOPE::PRG_ITEM   = "FLD10COST"
	!++
	! Abstract:FLD10COST
	!	^*Cost\*
	!
	!--
	COST = ENTR_3NUMBER(SCOPE,  SMG_WIN2, "08;20", "Cost", COST, FLAG%, &
		"#,###,###.###", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Skip key
	!
	CASE SMG$K_TRM_NEXT_SCREEN
		GOTO GetNextRec

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO OutaWin2

	END SELECT

	FOR LOOP% = 1% TO 10%

 EnterPrice:
		IF PRICE(LOOP%) <> 0.0
		THEN
			LP%  = LOOP% - 5%
			COL% = 58%

			IF LP% <= 0%
			THEN
				LP%  = LOOP%
				COL% = 20%
			ELSE
				LP% = LP% - 1%
			END IF

			PRICE(LOOP%) = ENTR_3NUMBER(SCOPE,  SMG_WIN2, &
				NUM1$(LP% + 8%) + ";" + NUM1$(COL%), &
				"Price ", PRICE(LOOP%), FLAG%, &
				"#,###,###.###", DEFLT$)

			SELECT SCOPE::SCOPE_EXIT
			!
			! Skip key
			!
			CASE SMG$K_TRM_NEXT_SCREEN
				GOTO 3050

			!
			! Exit key
			!
			CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
				GOTO OutaWin2

			END SELECT

		END IF

		SELECT SCOPE::SCOPE_EXIT
		!
		! Up arrow
		!
		CASE SMG$K_TRM_UP
			LOOP% = LOOP% - 1%
			GOTO EnterPrice IF LOOP% > 0%
			GOTO 3030
		END SELECT

	NEXT LOOP%

3050	!
	! See if we already have a record
	!
	GOTO 3060 IF PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, LOCATION$, &
		EFF_DATE$, COST_DATE$) = COST

	IF EFF_DATE$ <> COST_DATE$
	THEN
		PC_COST::PRODUCT  = PD_PRODUCT::PRODUCT_NUM
		PC_COST::LOCATION = LOCATION$
		PC_COST::EFFDATE  = EFF_DATE$
		PC_COST::COST     = COST

		PUT #PC_COST.CH%
	ELSE
		GET #PC_COST.CH%, KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
			LOCATION$ + EFF_DATE$

		PC_COST::COST     = COST

		UPDATE #PC_COST.CH%
	END IF

3060	FOR LOOP% = 1% TO 10%

		GOTO 3065 IF PC_READ_PRICE (PD_PRODUCT::PRODUCT_NUM, &
			LOCATION$, PRICETYPE(LOOP%), EFF_DATE$, &
			"", COST_DATE$, "") = PRICE(LOOP%)

		IF EFF_DATE$ <> COST_DATE$
		THEN
			PC_PRICE::PRODUCT_NUM = PD_PRODUCT::PRODUCT_NUM
			PC_PRICE::LOCATION    = LOCATION$
			PC_PRICE::PCTYPE      = PRICETYPE(LOOP%)
			PC_PRICE::XDATE       = EFF_DATE$
			PC_PRICE::XTIME       = ""
			PC_PRICE::PRICECOST   = PRICE(LOOP%)

			PUT #PC_PRICE.CH%
		ELSE
			GET #PC_PRICE.CH%, &
				KEY #1% EQ PRICETYPE(LOOP%) + &
				PD_PRODUCT::PRODUCT_NUM + LOCATION$ + &
				COST_DATE$

			PC_PRICE::PRICECOST = PRICE(LOOP%)

			UPDATE #PC_PRICE.CH%

		END IF

3065	NEXT LOOP%

	GOTO GetNextRec

 OutaWin2:
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_WIN2, SCOPE::SMG_PBID)

	RETURN

	%PAGE

 Win3:
	!*******************************************************************
	! Set Up Third Window
	!*******************************************************************

	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		13%, &
		70%, &
		SMG_WIN3, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN3, "Automatic Price Change")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, "From Price Type", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, "Percentage Change", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, "To Price Type", 4%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WIN3, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

	FROMPCTYPE$ = SPACE$(2%)
	TOPCTYPE$   = SPACE$(2%)
	FACTOR      = 1.0

	WIN3OPT$ = "C"
	SCOPE::PRG_IDENT = "H"
	SCOPE::PRG_PROGRAM = "IC_SPEC_PRICECHANGE"
	GOTO SelectWin3Option

4000	!
	! Enter options
	!
	WIN3OPTLIST$ = "Change Go Help eXit"
	WIN3OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", WIN3OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control C
	!
	CASE 3%
		GOTO 4000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO OutaWin3

	END SELECT

 SelectWin3Option:
	SELECT WIN3OPT$

	!
	! Call the help message
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", &
			SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, &
			"", "HELP")
		GOTO 4000

	CASE "C"

 FromPriceType:
		SCOPE::PRG_ITEM   = "FLD11FROMTYPE"
	!++
	! Abstract:FLD11FROMTYPE
	!	^*From Price Type\*
	!	.b
	!	.lm +5
	!	The ^*From Price Type\* field selects
	!	a particular price type or cost on which the change is based.
	!	.b
	!	If this field is blank, any changes will be based on the cost.
	!	.lm -5
	!
	! Index:
	!
	!--
		FROMPCTYPE$ = ENTR_3STRING(SCOPE, SMG_WIN3, "2;25", &
			"From Price Type", FROMPCTYPE$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM = "FLD11FROMTYPE"

			IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "VX") = 1%
			THEN
				FROMPCTYPE$ = PC_PRCTYPE::CODE
			END IF

			GOTO FromPriceType

		CASE SMG$K_TRM_UP
			GOTO FromPriceType

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 4000

		END SELECT

		IF FROMPCTYPE$ <> ""
		THEN
			PC_PRCTYPE::DESCRIPTION = &
				STRING$(LEN(PC_PRCTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, &
				"Q0" + FROMPCTYPE$) <> 1%
		ELSE
			PC_PRCTYPE::DESCRIPTION = "Cost"
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, &
			PC_PRCTYPE::DESCRIPTION, 2%, 35%, , SMG$M_BOLD)

 Factor:
		SCOPE::PRG_ITEM   = "FLD12CHANGE"
	!++
	! Abstract:FLD12CHANGE
	!	^*Percentage Change\*
	!	.b
	!	.lm +5
	!	The ^*Percentage Change\* field is the
	!	multiplier in percentage by which the amount in the To Price
	!	Type field will be changed.  Enter a positive number if you
	!	want to increace the price in the To Price Type field.  Enter
	!	a negative number to decrease the price in the To Price Type
	!	field.
	!	.b
	!	^*Example 1:\* The price from the From Price Type field is 100.00.
	!	You want to increase this price by 10% to 110.00.
	!	You would enter 10.000 in the ^*Percentage Change\* field.
	!	.b
	!	^*Example 2:\* The price from the From Price Type field is 100.00.
	!	You want to decrease this price by 25% to 75.00.
	!	You would enter -25.000 in the ^*Percentage Change\* field.
	!	.lm -5
	!
	! Index:
	!
	!--
		FACTOR = ENTR_3NUMBER(SCOPE, SMG_WIN3, "3;20", &
			"Factor ", (FACTOR * 100.0 - 100.0), FLAG%, &
			"###.##%", DEFLT$)

		FACTOR = 1.0 + FACTOR / 100.00

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO FromPriceType

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 4000

		END SELECT

 ToPriceType:
		SCOPE::PRG_ITEM   = "FLD13TOTYPE"
	!++
	! Abstract:FLD13TOTYPE
	!	^*To Price Type\*
	!	.b
	!	.lm +5
	!	The ^*To Price Type\* field selects
	!	a particular price type or cost on which the change will
	!	be reflected.
	!	.b
	!	If this field is blank, any changes will be reflected to
	!	the product cost.
	!	.lm -5
	!
	! Index:
	!
	!--
		TOPCTYPE$ = ENTR_3STRING(SCOPE, SMG_WIN3, "4;25", &
			"To Price Type", TOPCTYPE$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM = ""

			IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "VX") = 1%
			THEN
				TOPCTYPE$ = PC_PRCTYPE::CODE
			END IF

			GOTO ToPriceType

		CASE SMG$K_TRM_UP
			GOTO Factor

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 4000

		END SELECT

		IF TOPCTYPE$ <> ""
		THEN
			PC_PRCTYPE::DESCRIPTION = &
				STRING$(LEN(PC_PRCTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, &
				"Q0" + TOPCTYPE$) <> 1%
		ELSE
			PC_PRCTYPE::DESCRIPTION = "Cost"
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, &
			PC_PRCTYPE::DESCRIPTION, 4%, 35%, , SMG$M_BOLD)

	CASE "G"
		GOTO 4100

	CASE "X"
		GOTO OutaWin3

	END SELECT

	GOTO 4000

4100	CALL ENTR_3MESSAGE(SCOPE, "Working", 1% + 16%)

	!
	! Put the junk on the screen
	!
	TEXT$ = "Product              Old Amt      New Amt"

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, TEXT$, 6%, 2%)

	TEXT$ = "_________________________________________"

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, TEXT$, 7%, 2%)

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE OutaWin3 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetProdRec:
4120	!
	! Main loop starts here
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE OutaWin3 IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
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
			GOTO OutaWin3
		END IF
	END IF

	!
	! Check current record
	!
	!GOTO GetProdRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORTBY$

	CASE "C"
		GOTO OutaWin3 IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetProdRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO OutaWin3 IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetProdRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO OutaWin3 IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetProdRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO OutaWin3 IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetProdRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO OutaWin3 IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetProdRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

4200	IF FROMPCTYPE$ <> ""
	THEN
		OLDMONEY = PC_READ_PRICE(PD_PRODUCT::PRODUCT_NUM, LOCATION$, &
			FROMPCTYPE$, BASE_DATE$, "", "", "")
	ELSE
		OLDMONEY = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, LOCATION$, &
			BASE_DATE$, JUNK$)
	END IF

	MONEY = FUNC_ROUND(OLDMONEY * FACTOR, 3%)

4300	IF TOPCTYPE$ <> ""
	THEN
		!
		! See if we already have a record
		!
		TESTMONEY = PC_READ_PRICE(PD_PRODUCT::PRODUCT_NUM, LOCATION$, &
			TOPCTYPE$, EFF_DATE$, "", COST_DATE$, "")

		GOTO GetProdRec IF TESTMONEY = MONEY

		IF EFF_DATE$ <> COST_DATE$
		THEN
			PC_PRICE::PRODUCT_NUM = PD_PRODUCT::PRODUCT_NUM
			PC_PRICE::LOCATION    = LOCATION$
			PC_PRICE::PCTYPE      = TOPCTYPE$
			PC_PRICE::XDATE       = EFF_DATE$
			PC_PRICE::XTIME       = ""
			PC_PRICE::PRICECOST   = MONEY

			PUT #PC_PRICE.CH%
		ELSE
			WHEN ERROR IN
				GET #PC_PRICE.CH%, &
					KEY #1% EQ TOPCTYPE$ + &
					PD_PRODUCT::PRODUCT_NUM + LOCATION$ + &
					COST_DATE$
			USE
				CONTINUE GetProdRec IF ERR = 155%
				FILENAME$ = "PC_PRICE"
				CONTINUE HelpError
			END WHEN

			PC_PRICE::PRICECOST = MONEY

			UPDATE #PC_PRICE.CH%
		END IF
	END IF

4400	IF TOPCTYPE$ = ""
	THEN
		!
		! See if we already have a record
		!
		TESTMONEY = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, LOCATION$, &
			EFF_DATE$, COST_DATE$)

		GOTO GetProdRec IF TESTMONEY = MONEY

		IF EFF_DATE$ <> COST_DATE$
		THEN
			PC_COST::PRODUCT  = PD_PRODUCT::PRODUCT_NUM
			PC_COST::LOCATION = LOCATION$
			PC_COST::EFFDATE  = EFF_DATE$
			PC_COST::COST     = MONEY

			PUT #PC_COST.CH%
		ELSE
			WHEN ERROR IN
				GET #PC_COST.CH%, &
					KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
					LOCATION$ + EFF_DATE$
			USE
				CONTINUE GetProdRec IF ERR = 155%
				FILENAME$ = "PC_COST"
				CONTINUE HelpError
			END WHEN

			PC_COST::COST = MONEY

			UPDATE #PC_COST.CH%
		END IF
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, PD_PRODUCT::PRODUCT_NUM, &
		8%, 2%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, &
		FORMAT$(TESTMONEY, "#,###,###.###"), &
		8%, 17%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN3, &
		FORMAT$(MONEY, "#,###,###.###"), &
		8%, 30%, , SMG$M_BOLD)

	GOTO GetProdRec

 OutaWin3:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_WIN3, SCOPE::SMG_PBID)

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

19999	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION PC_MAIN_PRCTYPE
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PC_MAIN_PRCTYPE.ID

		MAINT_GROUP = PC_MAIN_PRCTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLD06EFFDATE
	!	^*Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field enters the date
	!	that the price or cost changes become effective. The date
	!	will default to today's date but can be changed to any
	!	date the user wishes.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD12FACTOR
	!	^*Change Factor\*
	!	.b
	!	.lm +5
	!	The ^*Change Factor\* field is the
	!	multiplier by which the amount in the To Price Type field
	!	will be changed.
	!	.lm -5
	!
	! Index:
	!
	!--
