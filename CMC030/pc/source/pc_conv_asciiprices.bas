1	%TITLE "Customer Name/Address Maintenance"
	%SBTTL "PC_CONV_ASCIIPRICES"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho  83402
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
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_CONV_ASCIIPRICES/LINE
	!	$ LINK/EXEC:PC_EXE PC_CONV_ASCIIPRICES,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_CONV_ASCIIPRICES.OBJ;*
	!
	! Author:
	!
	!	01/01/92 - Kevin Handy
	!
	! Modification history:
	!
	!	04/02/93 - Kevin Handy
	!		Rounded prices to two digits as per robinsons
	!		request.
	!
	!	05/23/94 - Kevin Handy
	!		Modified to update an existing price if one is
	!		already there for a given date.
	!
	!	07/06/94 - Kevin Handy
	!		Modified to ignore zero prices, and skip
	!		bad prices.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/22/98 - Kevin Handy
	!		Added ability to create new products
	!
	!	04/24/98 - Kevin Handy
	!		Trim <CR> off end of description.
	!
	!	05/04/98 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Maps
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST) PC_COST_CDD PC_COST

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE) PC_PRICE_CDD PC_PRICE

	!
	! External functions
	!
	EXTERNAL REAL FUNCTION FUNC_ROUND

	%PAGE

100	OPEN "PRICES.TXT" FOR INPUT AS FILE 1%

110	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.MOD"

120	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.MOD"

130	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.MOD"

1000	!
	! Get one price from input file
	!
	WHEN ERROR IN
		LINPUT #1%, TEXT$
	USE
		CONTINUE 5000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

1005	PRODUCT$ = LEFT(TEXT$, 14%)
	LOCATION$ = MID(TEXT$, 15%, 4%)
	PRICETYPE$ = MID(TEXT$, 19%, 2%)
	PRICEDATE$ = MID(TEXT$, 21%, 8%)
	IF PRICETYPE$ = "@@"
	THEN
		PRODDESCR$ = EDIT$(RIGHT(TEXT$, 29%), 4%)
	ELSE
		WHEN ERROR IN
			PRICE = VAL(MID(TEXT$, 29%, 14%)) / 1000000.0
		USE
			PRICE = 0.0
		END WHEN

		!
		! Bad Price
		!
		GOTO 1000 IF PRICE = 0.0
	END IF

1010	!
	! See if the product number really exists
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ PRODUCT$, REGARDLESS
	USE
		CONTINUE 2000 IF PRICETYPE$ = "@@"
		CONTINUE 1000
	END WHEN

	GOTO 1200 IF PRICETYPE$ = "~~"	! Cost
	GOTO 1000 IF PRICETYPE$ = "@@"	! Description

1100	!
	! Look for any prices that will supercede this one
	!
	WHEN ERROR IN
		GET #PC_PRICE.CH%, KEY #0% EQ PRODUCT$ + LOCATION$
	USE
		CONTINUE 1190
	END WHEN

1110	WHILE (PC_PRICE::PRODUCT_NUM = PRODUCT$) AND (PC_PRICE::LOCATION = LOCATION$)

		IF (PC_PRICE::XDATE = PRICEDATE$) AND &
			(PC_PRICE::PCTYPE = PRICETYPE$)
		THEN
			!
			! Update the existing price
			!
			PC_PRICE::PRICECOST = FUNC_ROUND(PRICE, 2%)
			UPDATE #PC_PRICE.CH%
			PRINT PRODUCT$; PRICETYPE$; "   *"
			GOTO 1000
		END IF

		WHEN ERROR IN
			GET #PC_PRICE.CH%
		USE
			CONTINUE 1190
		END WHEN
	NEXT

1190	!
	! Add price to file
	!
	PRINT PRODUCT$; PRICETYPE$

	PC_PRICE::PRODUCT_NUM	= PRODUCT$
	PC_PRICE::LOCATION	= LOCATION$
	PC_PRICE::PCTYPE	= PRICETYPE$
	PC_PRICE::XDATE		= PRICEDATE$
	PC_PRICE::XTIME		= ""
	PC_PRICE::PRICECOST	= FUNC_ROUND(PRICE, 2%)

	PUT #PC_PRICE.CH%

	GOTO 1000

1200	!
	! Look for any prices that will supercede this one
	!
	WHEN ERROR IN
		GET #PC_COST.CH%, KEY #0% EQ PRODUCT$ + LOCATION$
	USE
		CONTINUE 1290
	END WHEN

1210	WHILE (PC_COST::PRODUCT = PRODUCT$) AND (PC_COST::LOCATION = LOCATION$)

		IF PC_COST::EFFDATE = PRICEDATE$
		THEN
			PC_COST::COST = PRICE
			UPDATE #PC_COST.CH%
			PRINT PRODUCT$; "COST *"
			GOTO 1000
		END IF

		WHEN ERROR IN
			GET #PC_COST.CH%
		USE
			CONTINUE 1290
		END WHEN
	NEXT

1290	!
	! Add price to file
	!
	PRINT PRODUCT$; "COST"

	PC_COST::PRODUCT	= PRODUCT$
	PC_COST::LOCATION	= LOCATION$
	PC_COST::EFFDATE	= PRICEDATE$
	PC_COST::COST		= PRICE

	PUT #PC_COST.CH%

	GOTO 1000

2000	!*******************************************************************
	! Create a new product
	!*******************************************************************

	PD_PRODUCT::PRODUCT_NUM		= PRODUCT$
	PD_PRODUCT::DESCRIPTION		= PRODDESCR$
	PD_PRODUCT::PROD_TYPE		= "PP"
	PD_PRODUCT::CATEGORY		= LEFT(PRODUCT$, 3%)
	PD_PRODUCT::UOM			= "EA"
	PD_PRODUCT::PACK		= ""
	PD_PRODUCT::LABEL		= ""
	PD_PRODUCT::METHOD		= "STD"
	PD_PRODUCT::BDATE		= PRICEDATE$
	PD_PRODUCT::SSTATUS		= "A"
	PD_PRODUCT::EDATE		= ""
	PD_PRODUCT::SECONDARY_CODE	= ""
	PD_PRODUCT::WEIGHT		= 0.0
	PD_PRODUCT::BOMUOM		= "EA"
	PD_PRODUCT::PRODUCT_FACTOR	= 1.0

	PUT #PD_PRODUCT.CH%

	PRINT PRODUCT$; "@@ "; PRODDESCR$

	GOTO 1000

5000	CLOSE #PC_PRICE.CH%
	CLOSE PC_COST.CH%
	CLOSE #1%

	GOTO 32767

32767	END
