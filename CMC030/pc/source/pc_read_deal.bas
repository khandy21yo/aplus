1	%TITLE "Read Promotional Price File"
	%SBTTL "PC_READ_DEAL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PC_READ_DEAL(XPRODUCT$, XSHIPDATE$, XCUSTOMER$, &
		REAL IN_PRICE, REAL OUT_PRICE, STRING OUT_ACCOUNT, &
		STRING OUT_COMMENT, REAL OUT_OFF)

	!
	! COPYRIGHT (C) 2001 BY
	!
	! Software Solutions, Inc.
	! Idaho Falls, Idaho
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function returns the Product Dollar discount amount
	!	based on any promotion or discount percent
	!	the customer qualifies for.
	!	.b
	!	Note that the promo is based on the ship date, not the
	!	order date.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	XPRODUCT$ is a product number
	!	XSHIPDATE$ is the date for shipment (promotion must be active)
	!	XCUSTOMER$ is the customer number
	!	IN_PRICE is the original price
	!
	! Output:
	!
	!	OUT_PRICE is the final discounted price.
	!
	! Example:
	!
	!	FUNCTION LONG PC_READ_DEAL("product#", "shipdate", "customer#",
	!		PC_DEAL_READ, REAL IN_PRICE, REAL OUT_PRICE)
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_READ_DEAL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PC_READ_DEAL
	!	$ DELETE PC_READ_DEAL.OBJ;*
	!
	! Author:
	!
	!	10/16/2001 - Kevin Handy
	!
	! Modification history:
	!
	!	10/23/2001 - Kevin Handy
	!		The price, when given, is an amount off, not the
	!		final amount.
	!
	!	12/30/2002 - Kevin Handy
	!		Create new parameter OUT_OFF
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (PC_DEAL_CH)	PC_DEAL.CH%
	COM (PC_DEAL_PRODUCT_CH)	PC_DEAL_PRODUCT.CH%
	COM (AR_35CUSTOM_CH)	AR_35CUSTOM.CH%

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL_PRODUCT.HB"
	MAP (PC_DEAL_PRODUCT)	PC_DEAL_PRODUCT_CDD	PC_DEAL_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL.HB"
	MAP (PC_DEAL)		PC_DEAL_CDD		PC_DEAL

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Set initial values
	!
	OUT_PRICE = 0.0
	OUT_OFF = 0.0
	OUT_ACCOUNT = "Promo Acct"
	OUT_COMMENT = ""

	!
	! Assume cannot find any promotion prices
	!
	EXIT_STATUS = CMC$_WARNING

1000	IF PC_DEAL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "PC_DEAL"
			CONTINUE HelpError
		END WHEN
	END IF

1010	IF PC_DEAL_PRODUCT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL_PRODUCT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "PC_DEAL_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

2000	!
	! Locate to first promo master by ship date
	!
	WHEN ERROR IN
		FIND #PC_DEAL_PRODUCT.CH%, &
			KEY #1% EQ XPRODUCT$, &
			REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PC_DEAL_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Get next promo master record and check for use
	!
	WHEN ERROR IN
		GET #PC_DEAL_PRODUCT.CH%, &
			REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PC_DEAL_PRODUCT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF PC_DEAL_PRODUCT::PRODUCT <> XPRODUCT$

2100	!
	! Check if this is a valid promo for this product
	!
	WHEN ERROR IN
		GET #PC_DEAL.CH%, &
			KEY #1% EQ XCUSTOMER$ + PC_DEAL_PRODUCT::DEAL, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "PC_DEAL"
		CONTINUE HelpError
	END WHEN


	!
	! Resolve if this is okay to use
	!
	GOTO GetNextRec &
		IF PC_DEAL::STARTD > XSHIPDATE$ OR PC_DEAL::ENDD < XSHIPDATE$

	!
	! This is it - matched up so return with normal status and
	! calculate the new sale price
	!
	IF PC_DEAL_PRODUCT::PRICE <> 0.0
	THEN
		OUT_PRICE = FUNC_ROUND(IN_PRICE - PC_DEAL_PRODUCT::PRICE, 2%)
		OUT_OFF = FUNC_ROUND(PC_DEAL_PRODUCT::PRICE, 2%)
	ELSE
		OUT_PRICE = FUNC_ROUND(IN_PRICE * &
			(100.0 - PC_DEAL_PRODUCT::PERCENT) / 100.0, 2%)
		OUT_OFF = FUNC_ROUND( &
			(100.0 - PC_DEAL_PRODUCT::PERCENT) / 100.0, 2%)
	END IF

	OUT_ACCOUNT = PC_DEAL_PRODUCT::ACCOUNT
	OUT_COMMENT = PC_DEAL::DESCR

	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	PC_READ_DEAL = EXIT_STATUS

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
