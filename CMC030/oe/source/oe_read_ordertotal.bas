1	%TITLE "Order Total Function"
	%SBTTL "OE_READ_ORDERTOTAL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_READ_ORDERTOTAL(ORDNUM$, BATCH_NO$, REAL TOTAL())

	!
	! COPYRIGHT (C) 1991 BY
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
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function returns the extended order price and extended
	!	ship price for a certain order number.
	!	.lm -5
	!
	!
	! Parameters:
	!
	!	TOTAL()
	!		1 - Order Total
	!		2 - Ship Total
	!		3 - Order Non-Sales Taxable
	!		4 - Ship Non-Sales Taxable
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_READ_ORDERTOTAL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_READ_ORDERTOTAL
	!	$ DELETE OE_READ_ORDERTOTAL.OBJ;*
	!
	! Author:
	!
	!	11/04/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/08/93 - Frank F. Starman
	!		Read totals from MO orderline and MO order option, too.
	!
	!	02/09/93 - Dan Perkins
	!		Fixed error in calculating MO_ORDERLINEOPT shipprice.
	!
	!	06/17/94 - Kevin Handy
	!		Added code for ::MISCH2.
	!
	!	06/17/94 - Kevin Handy
	!		Added code to store non-taxable amounts in array
	!		elements (3) and (4), as well as opening
	!		PS_CONTROL file if necessary.
	!
	!	06/22/94 - Kevin Handy
	!		Open PS_CONTROL modify to lose some access problems.
	!
	!	08/09/94 - Kevin Handy
	!		Modified get on PS_CONTROL so it doesn't lock the
	!		record perminately.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/26/95 - Kevin Handy
	!		Lose extra externals.
	!		Reformat source closer to 80 columns.
	!
	!	06/02/95 - Kevin Handy
	!		Modified to look at product category to determine
	!		taxable/nontaxable status.
	!
	!	04/15/96 - Kevin Handy
	!		Round everything to 2 decimals instead of 3,
	!		because all other programs round lines to 2.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Common Statements
	!
	COM (CH_OE_ORDERLINE)		OE_ORDERLINE.CH%
	COM (CH_MO_ORDERLINE)		MO_ORDERLINE.CH%
	COM (CH_MO_ORDERLINEOPT)	MO_ORDERLINEOPT.CH%
	COM (CH_PS_CONTROL)		PS_CONTROL.CH%
	COM (CH_PD_PRODUCT)		PD_PRODUCT.CH%

	%PAGE

	!
	! Include Scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP (MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.HB"
	MAP (MO_ORDERLINEOPT)	MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT

	%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.HB"
	MAP (PS_CONTROL)	PS_CONTROL_CDD		PS_CONTROL

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	DECLARE LONG EXIT_STATUS

	!
	! Set initial value
	!
	TOTAL(I%) = 0.0 FOR I% = 1% TO 4%

	!
	! Assume no errors
	!
	EXIT_STATUS = CMC$_NORMAL

900	!
	! Open PS_CONTROL file
	!
	IF PS_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.MOD"
			GET #PS_CONTROL.CH%, RECORD 1%, REGARDLESS
		USE
			CONTINUE 1000 IF ERR = 5%
			FILENAME$ = "PS_CONTROL"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open OE_ORDERLINE file
	!
1000	IF OE_ORDERLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.CRE"
		USE
			CONTINUE 1010 IF ERR = 5%
			FILENAME$ = "OE_ORDERLINE_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open MO_ORDERLINE file
	!
1010	IF MO_ORDERLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.CRE"
		USE
			CONTINUE 1020 IF ERR = 5%
			FILENAME$ = "MO_ORDERLINE_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open MO_ORDERLINEOPT file
	!
1020	IF MO_ORDERLINEOPT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.CRE"
		USE
			CONTINUE 1030 IF ERR = 5%
			FILENAME$ = "MO_ORDERLINEOPT_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open MO_ORDERLINEOPT file
	!
1030	IF PD_PRODUCT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
		USE
			CONTINUE 2000 IF ERR = 5%
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Find OE_ORDERLINE file
	!
2000	PS_CUSBAL$ = TRM$(PS_CONTROL::CUSBAL)

	WHEN ERROR IN
		FIND #OE_ORDERLINE.CH%, KEY #0% EQ ORDNUM$, REGARDLESS
	USE
		CONTINUE 2010 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Get OE_ORDERLINE file
	!
	WHEN ERROR IN
		GET #OE_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE 2010 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 2010 IF OE_ORDERLINE::ORDNUM <> ORDNUM$

2002	!
	! Get product master file
	!
	IF PS_CUSBAL$ <> ""
	THEN
		IF PD_PRODUCT::PRODUCT_NUM <> OE_ORDERLINE::PRODUCT
		THEN
			THIS_CATEGORY$ = ""

			WHEN ERROR IN
				GET #PD_PRODUCT.CH%, &
					KEY #0% EQ OE_ORDERLINE::PRODUCT, &
					REGARDLESS
			USE
				CONTINUE 2006
			END WHEN
		END IF

		THIS_CATEGORY$ = PD_PRODUCT::CATEGORY
	ELSE
		THIS_CATEGORY$ = ""
	END IF

2006	!
	! Calculate the price
	!
	ORDERPRICE = FUNC_ROUND(OE_ORDERLINE::ORDQTY * &
		((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
		(1 - (OE_ORDERLINE::DISCOUNT / 100)) + &
		OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

	SHIPPRICE =  FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
		((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
		(1 - (OE_ORDERLINE::DISCOUNT / 100)) + &
		OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

	TOTAL(1%) = TOTAL(1%) + ORDERPRICE
	TOTAL(2%) = TOTAL(2%) + SHIPPRICE

	IF PS_CUSBAL$ <> ""
	THEN
		IF COMP_STRING(THIS_CATEGORY$, PS_CUSBAL$)
		THEN
			TOTAL(3%) = TOTAL(3%) + &
				FUNC_ROUND(OE_ORDERLINE::ORDQTY * &
				((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
				(1 - (OE_ORDERLINE::DISCOUNT / 100))), 2%)

			TOTAL(4%) = TOTAL(4%) + &
				FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
				((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
				(1 - (OE_ORDERLINE::DISCOUNT / 100))), 2%)
		END IF
	END IF

	IF PS_CONTROL::MISCTAXABLE = "N"
	THEN
		TOTAL(3%) = TOTAL(3%) + &
			FUNC_ROUND(OE_ORDERLINE::ORDQTY * &
			OE_ORDERLINE::MISCH, 3%)
		TOTAL(4%) = TOTAL(3%) + &
			FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
			OE_ORDERLINE::MISCH, 3%)
	END IF

	IF PS_CONTROL::MISC2TAXABLE = "N"
	THEN
		TOTAL(3%) = TOTAL(3%) + &
			FUNC_ROUND(OE_ORDERLINE::ORDQTY * &
			OE_ORDERLINE::MISCH2, 3%)
		TOTAL(4%) = TOTAL(3%) + &
			FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
			OE_ORDERLINE::MISCH2, 3%)
	END IF

	!
	! Get the next record
	!
	GOTO GetNextRec

	!
	! Find MO_ORDERLINE file
	!
2010	WHEN ERROR IN
		FIND #MO_ORDERLINE.CH%, KEY #0% EQ ORDNUM$, REGARDLESS
	USE
		CONTINUE 2020 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 GetNextLine:
	!
	! Get MO_ORDERLINE file
	!
	WHEN ERROR IN
		GET #MO_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE 2020 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 2020 IF MO_ORDERLINE::ORDNUM <> ORDNUM$

	!
	! Calculate the price
	!
	ORDERPRICE = FUNC_ROUND(MO_ORDERLINE::ORDQTY * &
		MO_ORDERLINE::PRICE * (1 - (MO_ORDERLINE::DISCOUNT / 100)), 2%)

	SHIPPRICE = FUNC_ROUND(MO_ORDERLINE::SHPQTY * &
		MO_ORDERLINE::PRICE * (1 - (OE_ORDERLINE::DISCOUNT / 100)), 2%)

	TOTAL(1%) = TOTAL(1%) + ORDERPRICE
	TOTAL(2%) = TOTAL(2%) + SHIPPRICE

	!
	! Get the next record
	!
	GOTO GetNextLine

	!
	! Find MO_ORDERLINEOPT file
	!
2020	WHEN ERROR IN
		FIND #MO_ORDERLINEOPT.CH%, KEY #0% EQ ORDNUM$, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 9% OR ERR = 155%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

 GetNextLineOpt:
	!
	! Get MO_ORDERLINE file
	!
	WHEN ERROR IN
		GET #MO_ORDERLINEOPT.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 9%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF MO_ORDERLINEOPT::ORDNUM <> ORDNUM$

	!
	! Calculate the price
	!
	ORDERPRICE = FUNC_ROUND(MO_ORDERLINEOPT::ORDQTY * &
		MO_ORDERLINEOPT::PRICE, 3%)

	SHIPPRICE = FUNC_ROUND(MO_ORDERLINEOPT::SHPQTY * &
		MO_ORDERLINEOPT::PRICE, 3%)

	TOTAL(1%) = TOTAL(1%) + ORDERPRICE
	TOTAL(2%) = TOTAL(2%) + SHIPPRICE

	!
	! Get the next record
	!
	GOTO GetNextLineOpt

 ExitFunction:
	OE_READ_ORDERTOTAL = EXIT_STATUS

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR

	GOTO ExitFunction

	END FUNCTION
