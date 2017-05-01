1	%TITLE "Read Promotional Price File"
	%SBTTL "OE_READ_PROMO"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_READ_PROMO(XPRODUCT$, XSHIPDATE$, XCUSTOMER$, &
		OE_PROMO_CDD OE_PROMO_READ, REAL IN_PRICE, REAL OUT_PRICE)

	!
	! COPYRIGHT (C) 1990 BY
	!
	! Computer Management Center, Inc.
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
	!	XCUSTOMER$   is the customer number
	!	IN_PRICE is the original price
	!
	! Output:
	!
	!	OUT_PRICE is the amount price discounted.
	!
	! Example:
	!
	!	FUNCTION LONG OE_READ_PROMO("product#", "shipdate", "customer#",
	!		OE_PROMO_READ, REAL IN_PRICE, REAL OUT_PRICE)
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_READ_PROMO/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_READ_PROMO
	!	$ DELETE OE_READ_PROMO.OBJ;*
	!
	! Author:
	!
	!	12/04/90 - Val James Allen
	!
	! Modification history:
	!
	!	06/26/91 - Frank F. Starman
	!		Change program name. Eliminate GOTO statements.
	!
	!	04/30/92 - Frank F. Starman
	!		Return description of account if undefined.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/19/92 - Frank F. Starman
	!		Split COM areas.
	!
	!	05/05/93 - Kevin Handy
	!		Modified to use HelpMessage instead of an "on error
	!		go back" call.
	!
	!	05/03/93 - Dan Perkins
	!		Changed common statements.  It seems that
	!		OE_PROMO_READ was supposed to hold the promo channel
	!		and return the promo record.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	10/17/2001 - Kevin Handy
	!		Zero OE_PROMO_READ::REFPROMO so it indicates promo's
	!		better to PS-MAIN-TICKETLINE.
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (OE_PROMO_CH)	OE_PROMO.CH%
	COM (OE_PRODPROMO_CH)	OE_PRODPROMO.CH%
	COM (AR_35CUSTOM_CH)	AR_35CUSTOM.CH%

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[OE.OPEN]OE_PRODPROMO.HB"
	MAP (OE_PRODPROMO)	OE_PRODPROMO_CDD	OE_PRODPROMO

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	MAP (OE_PROMO)		OE_PROMO_CDD		OE_PROMO

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Set initial value
	!
	OUT_PRICE = 0.0

	!
	! Assume cannot find any promotion prices
	!
	EXIT_STATUS = CMC$_WARNING

	OE_PROMO_READ::REFPROMO = ""
	OE_PROMO_READ::ACCOUNT = ""

1000	IF OE_PROMO.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "OE_PROMO"
			CONTINUE HelpError
		END WHEN
	END IF

1010	IF OE_PRODPROMO.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_PRODPROMO.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "OE_PRODPROMO"
			CONTINUE HelpError
		END WHEN
	END IF

1020	AR_35CUSTOM::CATEGORY = STRING$(LEN(AR_35CUSTOM::CATEGORY), A"?"B)
	AR_35CUSTOM::TTYPE    = STRING$(LEN(AR_35CUSTOM::TTYPE), A"?"B)
	AR_35CUSTOM::SALESMAN = STRING$(LEN(AR_35CUSTOM::SALESMAN), A"?"B)

	IF AR_35CUSTOM.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "AR_35CUSTOM"
			CONTINUE HelpError
		END WHEN
	END IF

1500	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, KEY #0% EQ XCUSTOMER$, REGARDLESS
	USE
		CONTINUE 2000 IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

2000	!
	! Locate to first promo master by ship date
	!
	WHEN ERROR IN
		FIND #OE_PROMO.CH%, KEY #1% GE XSHIPDATE$, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_PROMO"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Get next promo master record and check for use
	!
	WHEN ERROR IN
		GET #OE_PROMO.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 9%
		FILENAME$ = "OE_PROMO"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec IF OE_PROMO::FROMDATE > XSHIPDATE$

2100	!
	! Check if this is a valid promo for this product
	!
	WHEN ERROR IN
		GET #OE_PRODPROMO.CH%, &
			KEY #0% EQ XPRODUCT$ + OE_PROMO::REFPROMO, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR= 11%
		FILENAME$ = "OE_PRODPROMO"
		CONTINUE HelpError
	END WHEN

	!
	! Resolve if this is okay to use
	!
	GOTO GetNextRec &
		IF COMP_STRING(XCUSTOMER$, OE_PRODPROMO::CUSTOMER) = 0% &
		AND OE_PRODPROMO::CUSTOMER <> ""

	GOTO GetNextRec &
		IF COMP_STRING(AR_35CUSTOM::TTYPE, &
		OE_PRODPROMO::CUSTYPE) = 0% AND OE_PRODPROMO::CUSTYPE <> ""

	GOTO GetNextRec &
		IF COMP_STRING(AR_35CUSTOM::CATEGORY, &
		OE_PRODPROMO::CUSTCAT) = 0% AND OE_PRODPROMO::CUSTCAT <> ""

	GOTO GetNextRec &
		IF COMP_STRING(AR_35CUSTOM::SALESMAN, &
		OE_PRODPROMO::SALESMAN) = 0% AND OE_PRODPROMO::SALESMAN <> ""

	!
	! This is it - matched up so return with normal status and
	! calculate the new sale price
	!
	OE_PROMO_READ = OE_PROMO

	IF OE_PRODPROMO::PROMODOLL <> 0.0
	THEN
		OUT_PRICE = OE_PRODPROMO::PROMODOLL
	ELSE
		IF OE_PRODPROMO::PROMOPERC <> 0.0
		THEN
			OUT_PRICE = (IN_PRICE * &
				(OE_PRODPROMO::PROMOPERC / 100.0))
		END IF
	END IF

	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	OE_READ_PROMO = EXIT_STATUS

	OE_PROMO_READ::ACCOUNT = "Promo Acct" &
		IF OE_PROMO_READ::ACCOUNT = ""

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
