1	%TITLE "Query the Inventory Product"
	%SBTTL "IC_QURY_QUERY"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! ID:ICQURY
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Query Inventory Product File\* option
	!	makes on-line inquiries into selected products in
	!	respect to balances, prices and transactions.
	!	.lm -5
	!
	! Index:
	!	.x Query
	!
	! Option:
	!
	!	IC_OUTP_QUERYPROD$HELP
	!	IC_OUTP_PRODUCT$HELP
	!	IC_OUTP_ORDER$HELP
	!	IC_OUTP_ALLOCATE$HELP
	!	IC_FUNC_LOSTSALES$HELP
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_QURY_QUERY/LINE
	!	$ LINK/EXECUTABLE=IC_EXE: IC_QURY_QUERY,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_QURY_QUERY.OBJ;*
	!
	! Author:
	!
	!	02/09/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	11/11/92 - Dan Perkins
	!		Added option to print order detail.
	!
	!	11/12/92 - Dan Perkins
	!		Added option to print allocation detail.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Change SMG_QUERY to SMG_QUERY%
	!
	!	06/16/95 - Kevin Handy
	!		Format source closer to 80 columns.
	!
	!	02/12/96 - Kevin Handy
	!		Added lost sales option.
	!
	!	12/11/96 - Kevin Handy
	!		Always force FLAG% to 1024
	!
	!	02/18/97 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter to READ_PERIOD to integer
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	08/30/2000 - Kevin Handy
	!		Move some documentation (comments) from the end of the
	!		program into the option select statements.
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

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_SET

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	COM (CH_PD_PRODUCT_READ) PD_PRODUCT.CH%
	COM (CH_UTL_LOCATION_READ) UTL_LOCATION.CH%
	COM (CH_IC_CONTROL) IC_CONTROL.CH%

	!
	! External functions
	!
	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL LONG	FUNCTION PC_DSPL_PRICE
	EXTERNAL LONG	FUNCTION IC_DSPL_35BALANCE
	EXTERNAL LONG	FUNCTION IC_OUTP_QUERYPROD
	EXTERNAL LONG	FUNCTION IC_OUTP_PRODUCT
	EXTERNAL LONG	FUNCTION IC_OUTP_ORDER
	EXTERNAL LONG	FUNCTION IC_OUTP_ALLOCATE
	EXTERNAL LONG   FUNCTION IC_FUNC_LOSTSALES

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	CALL LIB$GET_SYMBOL("CMC$LOCATION" BY DESC, LOCATION$ BY DESC,,)
	CALL LIB$GET_SYMBOL("CMC$SYSTEM" BY DESC, SYSTEM$ BY DESC,,)
	FLAG% = 1024% &
		IF LOCATION$ = "" AND SYSTEM$ = ""

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
	! Read Period
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"

		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	V% = READ_PERIOD("READ", IC_CONTROL::ERA, IC_CONTROL::PERIOD, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	YYYYPP$ = IC_CONTROL::PERIOD
	CALL ASSG_FREECHANNEL(IC_CONTROL.CH%)

	GOSUB Initialize

	UTL_LOCATION::LOCATION = LOCATION$

	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		13%, &
		70%, &
		SMG_QUERY%, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_QUERY%, &
		SYSTEM$ + " Product Query " + YYYYPP$ + " for " + &
		TRM$(SCOPE::PRG_COMPANY))

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Product #", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Prod Type", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Prod Cat #", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "UOM ", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Prod Status", 6%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Secondary Code", 7%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Cost", 8%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Location #", 10%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Lost Sale", 11%, 2%) &
		IF FLAG% = 1024%

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_QUERY%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

	OPT$ = "F"
	GOTO SelectOption

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	!
	! Enter options
	!
	SCOPE::PRG_ITEM    = "H"
	SCOPE::PRG_IDENT   = "HELP"
	SCOPE::PRG_PROGRAM = "IC_QURY_QUERY"

	OPTLIST$ = "Find Next Trans_print Prod_print Order_det Alloc_det " + &
		"Lostsale Help eXit"
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

  SelectOption:
	SELECT OPT$

	!
	! Call the help message
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", "HELP")
		GOTO 1000

	CASE "F"
1120		PD_PRODUCT::PRODUCT_NUM = &
			ENTR_3STRING(SCOPE, SMG_QUERY%, "2;15", &
			"Product #", PD_PRODUCT::PRODUCT_NUM, &
			0%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT
		!
		! List Choices
		!
		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM = "FLD001"
			PRODUCT$ = PD_PRODUCT::PRODUCT_NUM
			IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") <> 1%
			THEN
				PD_PRODUCT::PRODUCT_NUM = PRODUCT$
			END IF
			GOTO 1120

		CASE SMG$K_TRM_CTRLC
			GOTO 1120

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

1150		WHEN ERROR IN
			FIND #PD_PRODUCT.CH%, &
				KEY #0% GE PD_PRODUCT::PRODUCT_NUM, &
				REGARDLESS
			GET #PD_PRODUCT.CH%, REGARDLESS
		USE
			IF ERR = 155%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Product not found", 0%)
				CONTINUE 1120
			END IF

			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN

		GOSUB Repaint
		GOTO 2000 IF LOCATION$ <> ""

1200		UTL_LOCATION::LOCATION = &
			ENTR_3STRING(SCOPE, SMG_QUERY%, "10;15", &
			"Location #", UTL_LOCATION::LOCATION, &
			0%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT
		!
		! List Choices
		!
		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM = "FLD002"
			LOCATION$ = UTL_LOCATION::LOCATION
			IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") <> 1%
			THEN
				UTL_LOCATION::LOCATION = LOCATION$
			END IF
			GOTO 1200

		CASE SMG$K_TRM_CTRLC
			GOTO 1200

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

2000		WHEN ERROR IN
			FIND #UTL_LOCATION.CH%, &
				KEY #0% GE UTL_LOCATION::LOCATION, REGARDLESS
			GET #UTL_LOCATION.CH%, REGARDLESS
		USE
			IF ERR = 155%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Location not found", 0%)
				CONTINUE 1200
			END IF

			FILENAME$ = "UTL_LOCATION"
			CONTINUE HelpError
		END WHEN

2010		LOCATION$ = ""

	CASE "N"
3000		WHEN ERROR IN
			GET #PD_PRODUCT.CH%, REGARDLESS
		USE
			IF ERR = 11%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "End of file", 0%)
				CONTINUE 1000
			END IF

			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN

	CASE "O"
	!++
	! Abstract:ORDER_DET
	!	^*Order Totals\*
	!	.p
	!	This option prints out the total on-order from the purchase order system.
	!
	! Index:
	!
	!--
		V% = IC_OUTP_ORDER(PD_PRODUCT, UTL_LOCATION, SYSTEM$)

		GOTO 1000

	CASE "A"
	!++
	! Abstract:ALLOC_DET
	!	^*Alloc__Det\*
	!	.b
	!	Print out all allocations currently open for a product.
	!	This report scans the Point of sale, Order Entry, and
	!	Material Order systems for allocations.
	!
	! Index:
	!
	!--
		V% = IC_OUTP_ALLOCATE(PD_PRODUCT, UTL_LOCATION)

		GOTO 1000

	CASE "T"
	!++
	! Abstract:TRANS_PRINT
	!	^*Trans__Print\*
	!	.b
	!	Print a transaction history listing.
	!
	! Index:
	!
	!--
		V% = IC_OUTP_QUERYPROD(PD_PRODUCT, UTL_LOCATION, YYYYPP$)

		GOTO 1000

	CASE "P"
	!++
	! Abstract:PROD_PRINT
	!	^*Prod__Print\*
	!	.b
	!	Print out a product history listing, including prices
	!	and costs.
	!
	! Index:
	!
	!--
		V% = IC_OUTP_PRODUCT(PD_PRODUCT, UTL_LOCATION, YYYYPP$)

		GOTO 1000

	CASE "L"
	!++
	! Abstract:LOSTSALE
	!	^*Lost Sales\*
	!	.b
	!	This option is used to enter lost sales that occur while
	!	in the query screen, which can occur while talking to
	!	a customer on the telephone.
	!	.b
	!	The lost sale will be immediately posted to the ^*current\*
	!	inventory period, using todays date and time one it.
	!	The batch number assigned to the transaction is "LOSTSA".
	!	.b
	!	To cancel a lost sales entry made in error, enter a
	!	negative lost sale to reverse it back out.
	!
	! Index:
	!
	!--
		V% = IC_FUNC_LOSTSALES(SCOPE, PD_PRODUCT, UTL_LOCATION, YYYYPP$)

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOSUB Repaint

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(TEMP_DISPLAY%, SCOPE::SMG_PBID)
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, TEMP_DISPLAY%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(TEMP_DISPLAY%, &
		SCOPE::SMG_PBID, 25%, 1%)

	UTL_LOCATION_SET = UTL_LOCATION

	V% = IC_DSPL_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION_SET::LOCATION, &
		AVAILABLE, "14;2", 64% + 512% + FLAG%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		FORMAT$(AVAILABLE, "######"), 11%, 18%,, SMG$M_BOLD) &
		IF FLAG% = 1024%

	V% = PC_DSPL_PRICE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION_SET::LOCATION, &
		DATE_TODAY, "", &
		PRICE, "4;45", 64% + 512%)

	UTL_LOCATION = UTL_LOCATION_SET

	GOTO 1000

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(TEMP_DISPLAY%, SCOPE::SMG_PBID)

	CALL LIB$GET_SYMBOL("CMC$SPAWN" BY DESC, USER_COMMANDS$ BY DESC,,)
	CALL SUBR_3EXITPROGRAM(SCOPE, USER_COMMANDS$, "")

 Repaint:
	!**************************************************************
	! Repaint product information
	!**************************************************************

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PD_PRODUCT::PRODUCT_NUM, 2%, 15%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PD_PRODUCT::DESCRIPTION, 2%, 30%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PD_PRODUCT::PROD_TYPE, 3%, 30%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PD_PRODUCT::CATEGORY, 4%, 30%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PD_PRODUCT::UOM, 5%, 30%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PD_PRODUCT::SSTATUS, 6%, 30%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PD_PRODUCT::SECONDARY_CODE, 7%, 30%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		FORMAT$(PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, &
		DATE_TODAY, ""), "#,###,###.##"), &
		8%, 15%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		UTL_LOCATION::LOCATION, 10%, 15%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		UTL_LOCATION::LOCNAME, 10%, 20%,, SMG$M_BOLD)

	RETURN

	%Page

 Initialize:
	!*******************************************************************
	! Set Initialize values
	!*******************************************************************

	PD_PRODUCT::PRODUCT_NUM = &
		STRING$(LEN(PD_PRODUCT::PRODUCT_NUM), A"?"B)
	PD_PRODUCT::DESCRIPTION = &
		STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)
	PD_PRODUCT::PROD_TYPE = &
		STRING$(LEN(PD_PRODUCT::PROD_TYPE), A"?"B)
	PD_PRODUCT::CATEGORY = &
		STRING$(LEN(PD_PRODUCT::CATEGORY), A"?"B)
	PD_PRODUCT::UOM = &
		STRING$(LEN(PD_PRODUCT::UOM), A"?"B)
	PD_PRODUCT::LABEL = &
		STRING$(LEN(PD_PRODUCT::LABEL), A"?"B)
	PD_PRODUCT::BDATE = &
		STRING$(LEN(PD_PRODUCT::BDATE), A"?"B)
	PD_PRODUCT::SSTATUS = &
		STRING$(LEN(PD_PRODUCT::SSTATUS), A"?"B)
	PD_PRODUCT::EDATE = &
		STRING$(LEN(PD_PRODUCT::EDATE), A"?"B)
	PD_PRODUCT::SECONDARY_CODE = &
		STRING$(LEN(PD_PRODUCT::SECONDARY_CODE), A"?"B)

	UTL_LOCATION::LOCATION = &
		STRING$(LEN(UTL_LOCATION::LOCATION), A"?"B)
	UTL_LOCATION::LOCNAME = &
		STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B)

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
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:COST
	!	^*Cost\*
	!	.b
	!	.lm +5
	!	The ^*Cost\* function views the
	!	Product Standard Cost screen for all products entered in the Master file.
	!	.lm -5
	!
	! Index:
	!	.x Cost>Function
	!
	!--
	!+-+-+
	!++
	! Abstract:PRICE
	!	^*Price\*
	!	.b
	!	.lm +5
	!	The ^*Price\* function views the
	!	Product Price Maintenance screen for all products entered in the Master File.
	!	.lm -5
	!
	! Index:
	!	.x Price>Function
	!
	!--
	!+-+-+
	!++
	! Abstract:PRODUCT_SCAN
	!	^*Product__scan\*
	!	.b
	!	.lm +5
	!	The ^*Product__scan\* option views
	!	the Product Balance Screen which displays the balance of all products.
	!	.lm -5
	!
	! Index:
	!	.x Product__scan\*>Function
	!
	!--
