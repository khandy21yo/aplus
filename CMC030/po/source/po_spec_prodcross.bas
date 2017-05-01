1	%TITLE "Purchase Order Part Cross Change"
	%SBTTL "PO_SPEC_PRODCROSS"
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
	!	$ BAS PO_SOURCE:PO_SPEC_PRODCROSS/LINE
	!	$ LINK/EXECUTABLE=PO_EXE: PO_SPEC_PRODCROSS,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_SPEC_PRODCROSS.OBJ;*
	!
	! Author:
	!
	!	06/17/92 - Dan Perkins
	!		Modified from IC_SPEC_PRICECHANGE.
	!
	! Modification history:
	!
	!	06/30/92 - Frank F. Starman
	!		Change routine to add new records or update
	!		the old ones.
	!
	!	07/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/06/92 - Dan Perkins
	!		Check program documentation.
	!
	!	08/26/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/04/93 - Kevin Handy
	!		Changed "=>" to ">=".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!		Fix end of history marker (--)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS
	DECLARE			PO_PARTCROSS_CDD	PO_PARTCROSS_NEW, &
							PO_PARTCROSS_OLD

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MEASURE.HB"
	MAP (UTL_MEASURE)	UTL_MEASURE_CDD		UTL_MEASURE

	MAP (DP_OUTP_XUNSOL)				RRR_FLAG%

	!
	! Declare variables
	!
	DECLARE LONG    SMG_WIN1
	DECLARE LONG    SMG_WIN2
	DECLARE INTEGER FLG(13%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

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
	! Open Partcross File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.CRE"
	USE
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

 MakeWindow:
	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		16%, &
		70%, &
		SMG_WIN1, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN1, &
		"Vendor Product Change for " + TRM$(SCOPE::PRG_COMPANY))

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Sort By (C,D,P,S,T)", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "From Item", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "To Item", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Wildcard", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Vendor#", 7%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Priority", 8%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Ven Prod# same as Inv Prod#", 9%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Ven Desc same as Inv Desc", 10%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Ven UOM same as Inv UOM", 11%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Vendor Factor", 12%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Our Factor", 13%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Minimum Quantity", 14%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, "Req Days Lead Time", 15%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WIN1, &
		SCOPE::SMG_PBID, &
		2%, &
		6% &
	)

	%PAGE

	!
	! Initialize variables
	!
	SORTBY$      = "P"
	FROM_ITEM$   = SPACE$(10%)
	TO_ITEM$     = SPACE$(10%)
	WLDCRD$      = SPACE$(20%)
	VENNUM$      = SPACE$(10%)
	PRIORITY$    = "1"
	VENFACTOR    = 1.0
	OURFACTOR    = 1.0
	MINQTY       = 0.0
	LEADTIME     = 0.0
	FLG(I%)      = 0% FOR I% = 1% TO 13%

	OPT$ = "C"
	SCOPE::PRG_IDENT    = "H"
	SCOPE::PRG_PROGRAM = "PO_SPEC_PRODCROSS"
	GOTO SelectOption

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	!
	! Enter options
	!
	SCOPE::PRG_IDENT   = "H"
	SCOPE::PRG_ITEM    = "HELP"
	SCOPE::PRG_PROGRAM = "PO_SPEC_PRODCROSS"

	OPTLIST$ = "Change Go Help eXit"
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
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", "HELP")
		GOTO 1000

	CASE "C"
 Sortby:
		SCOPE::PRG_ITEM   = "FLD01"
	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*Sort by	C,D,P,S,T\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field selects
	!	a particular sort key.
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
		SORTBY$ = ENTR_3STRING(SCOPE, SMG_WIN1, "2;25", &
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
		SCOPE::PRG_ITEM   = "FLD02"
	!++
	! Abstract:FLD02
	!	^*From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	report to begin printing with a selected item number.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
		FROM_ITEM$ = ENTR_3STRING(SCOPE, SMG_WIN1, "3;25", &
			"From Item", FROM_ITEM$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Sortby

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 ToItem:
		SCOPE::PRG_ITEM   = "FLD03"
	!++
	! Abstract:FLD03
	!	^*To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes
	!	the report to end printing with a selected item number.
	!	.b
	!	A blank setting will cause the program to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
		TO_ITEM$ = ENTR_3STRING(SCOPE, SMG_WIN1, "4;25", &
			"To Item", TO_ITEM$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO FromItem

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 Wildcard:
		SCOPE::PRG_ITEM   = "FLD04"
	!++
	! Abstract:FLD04
	!	^*Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a "wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		WLDCRD$ = ENTR_3STRING(SCOPE, SMG_WIN1, "5;25", &
			"Wildcard", WLDCRD$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO ToItem

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 VenNum:
		SCOPE::PRG_ITEM   = "FLD05"
	! +++
	! Abstract:FLD05
	!	^* Vendor _#\*
	!	.b
	!	.lm +5
	!	The ^*Vendor _#\* field enters an
	!	assigned number which identifies a specific vendor.
	!	.b
	!	The field will accommodate up to ten (10) alphanumeric
	!	characters.
	!	.b
	!	Valid Vendor _#'s may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
		VENNUM$ = ENTR_3STRING(SCOPE, SMG_WIN1, "7;31", &
			"Vendor #", VENNUM$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		!
		! List Choices
		!
		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM   = "FLD06"

			IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX") = 1%
			THEN
				VENNUM$ = AP_VENDOR::VENNUM
			END IF

			GOTO VenNum

		CASE SMG$K_TRM_UP
			GOTO Wildcard

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

		IF FUNC_TESTENTRY(SMG_WIN1, &
			VENNUM$, &
			AP_VENDOR::VENNAM, &
			"AP", SCOPE::PRG_ITEM, "PROG", &
			"Vendor", AP_MAIN_VENDOR.ID) <> 0%
		THEN
			GOTO VenNum
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN1, &
			AP_VENDOR::VENNAM, 7%, 45%, , SMG$M_BOLD)

 Priority:
		SCOPE::PRG_ITEM   = "FLD06"
	! +++
	! Abstract:FLD06
	!	^* Priority\*
	!	.b
	!	.lm +5
	!	The ^*Priority\* field enterd a priority
	!	number which ranks a specific vendor with regard to a product.
	!	Any alpha numeric character may be entered in this field.
	!	Priorities could be 0-9, A-Z, or any combination the user wishes
	!	to use.
	!	.b
	!	If all records are to have the same priority, this field would
	!	be left blank.
	!	.b
	!	This field will accommodate one (1) alphanumeric character.
	!	.lm -5
	!
	! Index:
	!
	!--
		PRIORITY$ = ENTR_3STRING(SCOPE, SMG_WIN1, "8;31", &
			"Priority", PRIORITY$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO VenNum

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 Product:
		SCOPE::PRG_ITEM   = "FLD07"
	!++
	! Abstract:FLD07
	!	^* Same Vendor Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Same Vendor Product _#\* field requires an entry of "Y"
	!	if the Vendor Product _# is the same as the user product
	!	number or an "N" if the Vendor Product _# is different
	!	from the user product number.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		PRODYESNO$ = ENTR_3YESNO(SCOPE, SMG_WIN1, "9;31", &
			"Same Product #", PRODYESNO$, FLAG%, "'E", "N")

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Priority

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 Description:
		SCOPE::PRG_ITEM   = "FLD08"
	!++
	! Abstract:FLD08
	!	^* Same Product Description\*
	!	.b
	!	.lm +5
	!	The ^*Same Product Description\* requires an entry of "Y"
	!	if the Vendor Product Description is the same
	!	as the user product description or an "N" if the Vendor
	!	Product Description is different from the user product
	!	description.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		DESCYESNO$ = ENTR_3YESNO(SCOPE, SMG_WIN1, "10;31", &
			"Same Description", DESCYESNO$, FLAG%, &
			"'E", "N")

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Product

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 UOM:
		SCOPE::PRG_ITEM   = "FLD09"
	!++
	! Abstract:FLD09
	!	^* Same Unit Of Measure\*
	!	.b
	!	.lm +5
	!	The ^*Same Unit of Measure\* field requires an entry of "Y"
	!	if the Vendor Unit of Measure is the same as
	!	the user unit of measure or an "N" if the Vendor Unit
	!	of Measure is different from the user unit of measure.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		UOMYESNO$ = ENTR_3YESNO(SCOPE, SMG_WIN1, "11;31", &
			"Same UOM", UOMYESNO$, FLAG%, "'E", "N")

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Description

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 VenFactor:
		SCOPE::PRG_ITEM   = "FLD10"
	!++
	! Abstract:FLD10
	!	^* Vendor Factor\*
	!	.b
	!	.lm +5
	!	The ^*Vendor Factor\* field
	!	enters the vendors side of a ratio that defines the conversion
	!	factor for this product. For example, if twelve of the vendors
	!	parts are used to make seven of your parts, enter a twelve (12) here,
	!	and a seven (7) in the next field.
	!	.b
	!	The field will accommodate a number up to 9999.999.
	!	.b
	!	The default for this field is 1.000.
	!	.b
	!	Set this field value to zero (0) if you want to change
	!	the ^*Vendor Factor\* field for every record.
	!	.lm -5
	!
	! Index:
	!
	!--
		VENFACTOR = ENTR_3NUMBER(SCOPE, SMG_WIN1, "12;31", &
			"Vendor Factor", VENFACTOR, FLAG%, &
			"####.###", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO UOM

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 OurFactor:
		SCOPE::PRG_ITEM   = "FLD11"
	!++
	! Abstract:FLD11
	!	^* Our Factor\*
	!	.b
	!	.lm +5
	!	The ^*Our Factor\* field enters the
	!	second half of the ratio used to determine the conversion
	!	factor between the vendors part number and our part number.
	!	.b
	!	The field will accommodate a number up to 9999.999.
	!	.b
	!	The default for this field is 1.000.
	!	.b
	!	Set this field value to zero (0) if you want to change
	!	the ^*Our Factor\* field for every record.
	!	.lm -5
	!
	! Index:
	!
	!--
		OURFACTOR = ENTR_3NUMBER(SCOPE, SMG_WIN1, "13;31", &
			"Our Factor", OURFACTOR, FLAG%, &
			"####.###", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO VenFactor

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 MinQty:
		SCOPE::PRG_ITEM   = "FLD12"
	!++
	! Abstract:FLD12
	!	^* Minimum Order Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Minimum Order Quantity\* field
	!	enters the minimun quantity the vendor requires for
	!	purchase.
	!	.b
	!	The field will accommodate a number up to 9999.
	!	.b
	!	The default for this field is 0.
	!	.b
	!	Set this field value to -1.0 if you want to change
	!	the ^*Minimum Order Quantity\* field for every record.
	!	.lm -5
	!
	! Index:
	!
	!--

		MINQTY = ENTR_3NUMBER(SCOPE, SMG_WIN1, "14;31", &
			"Min Qty", MINQTY, FLAG%, &
			"####", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO OurFactor

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 LeadTime:
		SCOPE::PRG_ITEM   = "FLD13"
	!++
	! Abstract:FLD13
	!	^* Required Days Lead Time\*
	!	.b
	!	.lm +5
	!	The ^*Required Days Lead Time\* field
	!	enters the expected length of time (in days)
	!	that it normally takes to receive this
	!	product from this vendor.
	!	.b
	!	The field will accommodate a number up to 9999.
	!	.b
	!	The default for this field is 0.
	!	.b
	!	Set this field value to -1.0 if you want to change
	!	the ^*Required Days Lead Time\* field for every record.
	!	.lm -5
	!
	! Index:
	!
	!--
		LEADTIME = ENTR_3NUMBER(SCOPE, SMG_WIN1, "15;31", &
			"Lead Time", LEADTIME, FLAG%, &
			"####", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO MinQty

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

	CASE "G"
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
	!*******************************************************************
	! Set Up Second Window
	!*******************************************************************

 CreateWin2:
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		17%, &
		70%, &
		SMG_WIN2, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN2, "Vendor Product Change")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Product #", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Description", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Prod Type", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Prod Cat", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Secondary Code", 6%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Vendor#", 8%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Priority", 9%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Vendor Product#", 10%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Description", 11%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Unit of Measure", 12%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Vendor Factor", 13%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Our Factor", 14%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Minimum Quantity", 15%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Req Days Lead Time", 16%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WIN2, &
		SCOPE::SMG_PBID, &
		2%, &
		6% &
	)

2000	WHEN ERROR IN
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
	SCOPE::SCOPE_EXIT = 0%

2010	!
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
	! Set Record defaults
	!
	PO_PARTCROSS::PRODUCT = PD_PRODUCT::PRODUCT_NUM
	PO_PARTCROSS::VENDOR  = VENNUM$

	IF PRODYESNO$ = "Y"
	THEN
		PO_PARTCROSS::VENPROD = PD_PRODUCT::PRODUCT_NUM
		FLG(7%) = 1%
	ELSE
		PO_PARTCROSS::VENPROD = SPACE$(LEN(PO_PARTCROSS::VENPROD))
		FLG(7%) = 0%
	END IF

	IF DESCYESNO$ = "Y"
	THEN
		PO_PARTCROSS::DESCR = PD_PRODUCT::DESCRIPTION
		FLG(8%) = 1%
	ELSE
		PO_PARTCROSS::DESCR = SPACE$(LEN(PO_PARTCROSS::DESCR))
		FLG(8%) = 0%
	END IF

	IF UOMYESNO$ = "Y"
	THEN
		PO_PARTCROSS::VENUOM = PD_PRODUCT::UOM
		FLG(9%) = 1%
	ELSE
		PO_PARTCROSS::VENUOM = SPACE$(LEN(PO_PARTCROSS::VENUOM))
		FLG(9%) = 0%
	END IF

	IF VENFACTOR > 0.0
	THEN
		PO_PARTCROSS::VENFAC = VENFACTOR
		FLG(10%) = 1%
	ELSE
		PO_PARTCROSS::VENFAC = 1.0
		FLG(10%) = 0%
	END IF

	IF OURFACTOR > 0.0
	THEN
		PO_PARTCROSS::FACTOR = OURFACTOR
		FLG(11%) = 1%
	ELSE
		PO_PARTCROSS::FACTOR = 1.0
		FLG(11%) = 0%
	END IF

	IF MINQTY >= 0.0
	THEN
		PO_PARTCROSS::MINQTY = MINQTY
		FLG(12%) = 1%
	ELSE
		PO_PARTCROSS::MINQTY = 0.0
		FLG(12%) = 0%
	END IF

	IF LEADTIME >= 0.0
	THEN
		PO_PARTCROSS::LEAD = LEADTIME
		FLG(13%) = 1%
	ELSE
		PO_PARTCROSS::LEAD = 0.0
		FLG(13%) = 0%
	END IF

	GOT_REC_FLAG% = 0%

2100	!
	! Get Partcross Record
	!
	WHEN ERROR IN
		GET #PO_PARTCROSS.CH%, &
			KEY #1% EQ VENNUM$ + PD_PRODUCT::PRODUCT_NUM
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE Displ IF ERR = 155%
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

	GOT_REC_FLAG% = -1%

 Displ:
	IF PRIORITY$ <> ""
	THEN
		PO_PARTCROSS::PRIORITY = PRIORITY$
		FLG(6%) = 1%
	ELSE
		!PO_PARTCROSS::PRIORITY = SPACE$(LEN(PO_PARTCROSS::PRIORITY))
		FLG(6%) = 0%
	END IF
	!
	! Put the junk on the screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::PRODUCT_NUM, &
		2%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::DESCRIPTION, &
		3%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::PROD_TYPE, &
		4%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::CATEGORY, &
		5%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::SECONDARY_CODE, &
		6%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PO_PARTCROSS::VENDOR, &
		8%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, AP_VENDOR::VENNAM, &
		8%, 45%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PO_PARTCROSS::PRIORITY, &
		9%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PO_PARTCROSS::VENPROD, &
		10%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PO_PARTCROSS::DESCR, &
		11%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PO_PARTCROSS::VENUOM, &
		12%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
		FORMAT$(PO_PARTCROSS::VENFAC, "####.###"), &
		13%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
		FORMAT$(PO_PARTCROSS::FACTOR, "####.###"), &
		14%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
		FORMAT$(PO_PARTCROSS::MINQTY, "####"), &
		15%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
		FORMAT$(PO_PARTCROSS::LEAD, "####"), &
		16%, 25%, , SMG$M_BOLD)

 PriorityWin2:
	SCOPE::PRG_ITEM   = "FLD20"
	! +++
	! Abstract:FLD20
	!	^* Priority\*
	!	.b
	!	.lm +5
	!	The ^*Priority\* field
	!	enters a priority number which ranks a specific vendor
	!	with regard to a product.  Any alpha numeric character may be
	!	entered in this field.  Priorities could be 0-9, A-Z, or any
	!	combination the user wishes to use.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	The field will accommodate one (1) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
	PO_PARTCROSS::PRIORITY = ENTR_3STRING(SCOPE, SMG_WIN2, "9;25", &
		"Priority", PO_PARTCROSS::PRIORITY, FLG(6%), "'E", DEFLT$)

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

	CASE SMG$K_TRM_UP
		SCOPE::SCOPE_EXIT = 0%

	END SELECT

 ProductWin2:
	SCOPE::PRG_ITEM   = "FLD21"
	!++
	! Abstract:FLD21
	!	^* Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field
	!	enters an assigned number which identifies a specific product.
	!	.b
	!	The field will accommodate up to fourteen (14) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
	PO_PARTCROSS::VENPROD = ENTR_3STRING(SCOPE, SMG_WIN2, "10;25", &
		"Vendor Product", PO_PARTCROSS::VENPROD, FLG(7%), &
		"'E", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_UP
		GOTO PriorityWin2

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

 DescriptionWin2:
	SCOPE::PRG_ITEM   = "FLD22"
	!++
	! Abstract:FLD22
	!	^* Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field
	!	enters a description for a specific product.
	!	.b
	!	The field will accommodate up to forty (40) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
	PO_PARTCROSS::DESCR = ENTR_3STRING(SCOPE, SMG_WIN2, "11;25", &
		"Description", PO_PARTCROSS::DESCR, FLG(8%), &
		"'E", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_UP
		GOTO ProductWin2

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

 UOMWin2:
	SCOPE::PRG_ITEM   = "FLD23"
	!++
	! Abstract:FLD23
	!	^* Unit Of Measure\*
	!	.b
	!	.lm +5
	!	The ^*Unit of Measure\* field
	!	enters a code which identifies an applicable unit of measure which
	!	has been established in the Unit of Measure Table.
	!	.b
	!	The field will accommodate two (2) alphanumeric characters.
	!	.b
	!	Valid Unit of Measure codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
	PO_PARTCROSS::VENUOM = ENTR_3STRING(SCOPE, SMG_WIN2, "12;25", &
		"Vendor UOM", PO_PARTCROSS::VENUOM, FLG(9%), &
		"'E", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT
	!
	! List Choices
	!
	CASE SMG$K_TRM_F14
		SCOPE::PRG_ITEM   = "FLD09"

		IF MAIN_WINDOW(UTL_MAIN_MEASURE.ID, &
			"VX") = 1%
		THEN
			PO_PARTCROSS::VENUOM = UTL_MEASURE::CODE
		END IF

		GOTO UOMWin2

	CASE SMG$K_TRM_UP
		GOTO DescriptionWin2

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

	IF FUNC_TESTENTRY(SMG_WIN2, &
		PO_PARTCROSS::VENUOM, &
		UTL_MEASURE::DESCRIPTION, &
		"UTL", SCOPE::PRG_ITEM, "PROG", &
		"UOM", UTL_MAIN_MEASURE.ID) <> 0%
	THEN
		FLG(9%) = 0%
		GOTO UOMWin2
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, UTL_MEASURE::DESCRIPTION, &
		12%, 45%, , SMG$M_BOLD)

 VenFactorWin2:
	SCOPE::PRG_ITEM   = "FLD24"
	!++
	! Abstract:FLD24
	!	^*Vendor Factor\*
	!	.b
	!	.lm +5
	!	The ^*Vendor Factor\* field
	!	enters the vendors side of a ratio that defines the conversion
	!	factor for this product. For example, if twelve of the vendors
	!	parts are used to make seven of your parts, enter a twelve (12) here,
	!	and a seven (7) in the next field.
	!	.b
	!	The field will accommodate a number up to 9999.999.
	!	.lm -5
	!
	! Index:
	!
	!--
	PO_PARTCROSS::VENFAC = ENTR_3NUMBER(SCOPE, SMG_WIN2, "13;25", &
		"Vendor Factor", PO_PARTCROSS::VENFAC, FLG(10%), &
		"####.###", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_UP
		GOTO UOMWin2

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

	GOTO VenFactorWin2 IF PO_PARTCROSS::VENFAC = 0.0

 OurFactorWin2:
	SCOPE::PRG_ITEM   = "FLD25"
	!++
	! Abstract:FLD25
	!	^* Our Factor\*
	!	.p
	!	The ^*Our Factor\* field
	!	enters the second half of the ratio used to determine the
	!	conversion factor between the vendors part number and our part number.
	!	.p
	!	The field will accommodate a number up to 9999.999.
	!
	! Index:
	!
	!--
	PO_PARTCROSS::FACTOR = ENTR_3NUMBER(SCOPE, SMG_WIN2, "14;25", &
		"Our Factor", PO_PARTCROSS::FACTOR, FLG(11%), &
		"####.###", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_UP
		GOTO VenFactorWin2

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

	GOTO OurFactorWin2 IF PO_PARTCROSS::FACTOR = 0.0

 MinQtyWin2:
	SCOPE::PRG_ITEM   = "FLD26"
	!++
	! Abstract:FLD26
	!	^* Minimun Order Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Minimun Order Quantity\* field
	!	enters the minimun quantity the vendor requires for
	!	purchase.
	!	.b
	!	The field will accommodate a number up to 9999.
	!	.lm -5
	!
	! Index:
	!
	!--
	PO_PARTCROSS::MINQTY = ENTR_3NUMBER(SCOPE, SMG_WIN2, "15;25", &
		"Minimun Quantity", PO_PARTCROSS::MINQTY, FLG(12%), &
		"####", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_UP
		GOTO OurFactorWin2

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

 LeadTimeWin2:
	SCOPE::PRG_ITEM   = "FLD27"
	!++
	! Abstract:FLD27
	!	^* Required Days Lead Time\*
	!	.b
	!	.lm +5
	!	The ^*Required Days Lead Time\* field
	!	enters the expected length of time (in days)
	!	that it normally takes to receive this
	!	product from this vendor.
	!	.b
	!	The field will accommodate a number up to 9999.
	!	.lm -5
	!
	! Index:
	!
	!--
	PO_PARTCROSS::LEAD = ENTR_3NUMBER(SCOPE, SMG_WIN2, "16;25", &
		"Lead Time", 1.0*PO_PARTCROSS::LEAD, FLG(13%), &
		"####", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_UP
		GOTO MinQtyWin2

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

2200	PO_PARTCROSS_NEW = PO_PARTCROSS
	IF GOT_REC_FLAG%
	THEN
		WHEN ERROR IN
			DELETE #PO_PARTCROSS.CH%
		USE
			CONTINUE 2210 IF ERR = 134%
			FILENAME$ = "PO_PARTCROSS"
			CONTINUE HelpError
		END WHEN
	END IF

	WHEN ERROR IN
		PUT #PO_PARTCROSS.CH%
	USE
		CONTINUE 2210 IF ERR = 134%
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec

2210	!
	! remove the old record
	!
	WHEN ERROR IN
		GET #PO_PARTCROSS.CH%, &
			KEY #0% EQ PO_PARTCROSS_NEW::PRODUCT + &
			PO_PARTCROSS_NEW::PRIORITY

		PO_PARTCROSS_OLD = PO_PARTCROSS

		DELETE #PO_PARTCROSS.CH%

		!
		! add a new record and put back the old record
		!
		PO_PARTCROSS = PO_PARTCROSS_NEW

		PUT #PO_PARTCROSS.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN


2220	!
	! try the next available priority
	!
	PO_PARTCROSS_OLD::PRIORITY = &
		CHR$(ASCII(PO_PARTCROSS_OLD::PRIORITY) + 1%)

	PO_PARTCROSS = PO_PARTCROSS_OLD
	PO_PARTCROSS_NEW = PO_PARTCROSS_OLD

	WHEN ERROR IN
		PUT #PO_PARTCROSS.CH%
	USE
		CONTINUE 2210 IF ERR = 134%
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec

 OutaWin2:
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_WIN2, SCOPE::SMG_PBID)

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

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR
	EXTERNAL LONG FUNCTION UTL_MAIN_MEASURE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AP_MAIN_VENDOR.ID

		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_MEASURE.ID

		MAINT_GROUP = UTL_MAIN_MEASURE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
