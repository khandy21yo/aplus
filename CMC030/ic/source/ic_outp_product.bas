1	%TITLE "Product Scan Report"
	%SBTTL "IC_OUTP_PRODUCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_OUTP_PRODUCT(PD_PRODUCT_CDD PD_PRODUCT, &
		UTL_LOCATION_CDD UTL_LOCATION, &
		STRING CURPERIOD)

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints product scan report
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_OUTP_PRODUCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_OUTP_PRODUCT
	!	$ DELETE IC_OUTP_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	08/05/88 - Frank Starman
	!
	! Modification History:
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/06/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_CATEGORY.HB"
	MAP (PD_CATEGORY)	PD_CATEGORY_CDD		PD_CATEGORY

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP (PC_PRCTYPE)	PC_PRCTYPE_CDD		PC_PRCTYPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_COST
	EXTERNAL REAL    FUNCTION PC_READ_PRICE
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE
	EXTERNAL STRING  FUNCTION FUNC_BITSTRING
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_ITEM$ = SCOPE::PRG_ITEM
	TEMP_IDENT$ = SCOPE::PRG_IDENT

	SCOPE::PRG_PROGRAM = "OUTP_PRODUCT"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	IC_OUTP_PRODUCT = 0%
	REPORT$ = "IC050"

400	!
 SetScreen:
	!******************************************************************
	! Set up the report settings screen
	!******************************************************************
	!
	! Ask user to change settings
	!
	GOTO Exit1 &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

510	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.OPN"
	USE
		CONTINUE 520 IF ERR = 5%
		FILENAME$ = "PD_PRODTYPE"
		CONTINUE HelpError
	END WHEN

520	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_CATEGORY.OPN"
	USE
		CONTINUE 530 IF ERR = 5%
		FILENAME$ = "PD_CATEGORY"
		CONTINUE HelpError
	END WHEN

530	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		CONTINUE 540 IF ERR = 5%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

540	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.OPN"
	USE
		CONTINUE 550 IF ERR = 5%
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

550	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  QUERY  REPORT"
	TITLE$(2%) = "Inventory Control System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	TEXT$ = "Location " + UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::LOCNAME

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "     Product number     " + PD_PRODUCT::PRODUCT_NUM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Description        " + PD_PRODUCT::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Secondary Code     " + PD_PRODUCT::SECONDARY_CODE

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "     Unit of Meassure   " + PD_PRODUCT::UOM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     BOM UOM            " + PD_PRODUCT::BOMUOM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17200	WHEN ERROR IN
		GET #PD_PRODTYPE.CH%, KEY #0% EQ PD_PRODUCT::PROD_TYPE, &
			REGARDLESS
	USE
		PD_PRODTYPE::DESCRIPTION = &
			STRING$(LEN(PD_PRODTYPE::DESCRIPTION), A"?"B)

		CONTINUE ProdType IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODTYPE"
		CONTINUE HelpError
	END WHEN

 ProdType:
	TEXT$ = "     Type               " + PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODTYPE::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17210	WHEN ERROR IN
		GET #PD_CATEGORY.CH%, KEY #0% EQ PD_PRODUCT::CATEGORY, &
			REGARDLESS
	USE
		PD_CATEGORY::DESCRIPTION = &
			STRING$(LEN(PD_CATEGORY::DESCRIPTION), A"?"B)

		CONTINUE ProdCat IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_CATEGORY"
		CONTINUE HelpError
	END WHEN

 ProdCat:
	TEXT$ = "     Category           " + PD_PRODUCT::CATEGORY + " " + &
		PD_CATEGORY::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Label              " + PD_PRODUCT::LABEL

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Status             " + PD_PRODUCT::SSTATUS

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     On Set Date        " + PRNT_DATE(PD_PRODUCT::BDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Status Date        " + PRNT_DATE(PD_PRODUCT::EDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Weight             " + &
		FORMAT$(PD_PRODUCT::WEIGHT, "##,###,###.###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Costing Method     " + PD_PRODUCT::METHOD

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, UTL_LOCATION::LOCATION, &
		"", EFF_DATE$)

	TEXT$ = "     Standard Cost          " + &
		FORMAT$(COST, "##,###,###.###") + "   Effective from " + &
		PRNT_DATE(EFF_DATE$, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

17220	WHEN ERROR IN
		RESET #PC_PRCTYPE.CH%
	USE
		CONTINUE Balance IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

 GetNextPrice:
	WHEN ERROR IN
		GET #PC_PRCTYPE.CH%, REGARDLESS
	USE
		CONTINUE Balance IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Build Array
	!
	PRICE = PC_READ_PRICE(PD_PRODUCT::PRODUCT_NUM, UTL_LOCATION::LOCATION, &
		PC_PRCTYPE::CODE, "", "", EFF_DATE$, "")

	IF PRICE <> 0.0
	THEN
		TEXT$ = "     " + PC_PRCTYPE::CODE + " " + &
			PC_PRCTYPE::DESCRIPTION + &
			FORMAT$(PRICE, "##,###,###.###") + &
			"   Effective from " + &
			PRNT_DATE(EFF_DATE$, 8%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	GOTO GetNextPrice

 Balance:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	!
	! Read balance file
	!
	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, &
		BALANCE(,))

	TEXT$ = "     " + CURPERIOD + SPACE$(18%) + &
		"        OnHand " + &
		"         Alloc " + &
		"       OnOrder"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%)
	ALLOC = BALANCE(2%, 1%) + BALANCE(2%, 2%)
	ONORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%)

	RUNONHAND = ONHAND + BALANCE(1%, 3%)
	RUNALLOC = ALLOC + BALANCE(2%, 3%)
	RUNONORDER = ONORDER + BALANCE(3%, 3%)

	TEXT$ = "     Beginning Balance       " + &
		FORMAT$(BALANCE(1%, 1%), "###,###,###.##") + " " + &
		FORMAT$(BALANCE(2%, 1%), "###,###,###.##") + " " + &
		FORMAT$(BALANCE(3%, 1%), "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Changes                 " + &
		FORMAT$(BALANCE(1%, 2%), "###,###,###.##") + " " + &
		FORMAT$(BALANCE(2%, 2%), "###,###,###.##") + " " + &
		FORMAT$(BALANCE(3%, 2%), "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Current Posted Balance  " + &
		FORMAT$(ONHAND, "###,###,###.##") + " " + &
		FORMAT$(ALLOC, "###,###,###.##") + " " + &
		FORMAT$(ONORDER, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Current Running Balance " + &
		FORMAT$(RUNONHAND, "###,###,###.##") + " " + &
		FORMAT$(RUNALLOC, "###,###,###.##") + " " + &
		FORMAT$(RUNONORDER, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

17250	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
			UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE BinLocBad IF ERR = 155% OR ERR = 9%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

 BinLoc:
	FREE_STOCK = ONHAND + ALLOC - IC_BINMAP::SAFETY

	TEXT$ = "     Maximum Level      " + &
		FORMAT$(IC_BINMAP::MAXLEVEL, "##,###,###.##") + &
		"     Safety Stock       " + &
		FORMAT$(IC_BINMAP::SAFETY, "##,###,###.##") + &
		"     Free Stock         " + &
		FORMAT$(FREE_STOCK, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "     ABC Flag           " + &
		IC_BINMAP::ABC

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Bin Location 1     " + &
		IC_BINMAP::BIN(0%) + &
		"     Bin Location 2     " + &
		IC_BINMAP::BIN(1%) + &
		"     Bin Location 3     " + &
		IC_BINMAP::BIN(2%) + &
		"     Bin Location 4     " + &
		IC_BINMAP::BIN(3%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "                        " + &
		"123456789-123456789-123456789-123456789-123456789-12"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     CycleCount Weeeks  " + &
		FUNC_BITSTRING(8%,IC_BINMAP::CYCLEMAP, 52%, "*")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 BinLocBad:

 ExitFunction:
	CALL OUTP_FINISH(UTL_REPORTX)

	CLOSE PD_PRODTYPE.CH%
	CALL ASSG_FREECHANNEL(PD_PRODTYPE.CH%)

	CLOSE PD_CATEGORY.CH%
	CALL ASSG_FREECHANNEL(PD_CATEGORY.CH%)

	CLOSE IC_BINMAP.CH%
	CALL ASSG_FREECHANNEL(IC_BINMAP.CH%)

	CLOSE PC_PRCTYPE.CH%
	CALL ASSG_FREECHANNEL(PC_PRCTYPE.CH%)

 Exit1:
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$
	SCOPE::PRG_ITEM = TEMP_ITEM$
	SCOPE::PRG_IDENT = TEMP_IDENT$

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
