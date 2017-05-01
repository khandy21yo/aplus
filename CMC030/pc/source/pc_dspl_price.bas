1	%TITLE "Display Product Price"
	%SBTTL "PC_DSPL_PRICE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PC_DSPL_PRICE( XPRODUCT$, XLOCATION$, &
		XDATE$, XTIME$, XPRICE, WPOS$, FLAG%)

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	.p
	!	This function returns price from the Price based on
	!	the price type selection.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	XPRODUCT$ is a product number
	!	XLOCATION$   is a location number
	!	XDATE$   is date
	!	XTIME$   is time
	!
	! Output:
	!
	!	price of a product
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_DSPL_PRICE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PC_DSPL_PRICE
	!	$ DELETE PC_DSPL_PRICE.OBJ;*
	!
	! Author:
	!
	!	11/05/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change last parameter on ENTR_3CHOICE from "" to 0%
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (CH_PC_PRCTYPE_READ) PC_PRCTYPE.CH%

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP	(PC_PRCTYPE)	PC_PRCTYPE_CDD	PC_PRCTYPE

	DECLARE LONG EXIT_STATUS
	DECLARE STRING PRICEARRAY(100%)

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_PRICE

	ON ERROR GOTO 19000

	XPRICE = 0.
	EXIT_STATUS = CMC$_NOOPTION

1000	IF PC_PRCTYPE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			EXIT HANDLER
		END WHEN
	END IF

2000	WHEN ERROR IN
		RESET #PC_PRCTYPE.CH%
	USE
		CONTINUE Choice IF ERR = 11% OR ERR = 9%
		EXIT HANDLER
	END WHEN

 GetNextRec:
	WHEN ERROR IN
		GET #PC_PRCTYPE.CH%, REGARDLESS
	USE
		CONTINUE Choice IF ERR = 11% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	!
	! Build Array
	!
	PRICE = PC_READ_PRICE(XPRODUCT$, XLOCATION$, PC_PRCTYPE::CODE, &
		XDATE$, XTIME$, "", "")

	IF PRICE <> 0.0
	THEN
		I% = I% + 1%
		PRICEARRAY(I%) = FORMAT$(PRICE, "###,###.### ") + &
			PC_PRCTYPE::CODE + " " + TRM$(PC_PRCTYPE::DESCRIPTION)
	END IF

	GOTO GetNextRec

 Choice:
	I% = ENTR_3CHOICE(SCOPE, WPOS$, "", PRICEARRAY(), "", &
		FLAG%, "        Price Type", "", 0%)

	IF I% > 0%
	THEN
		XPRICE = VAL(LEFT(PRICEARRAY(I%), 11%))
		EXIT_STATUS = CMC$_NORMAL
	END IF

 ExitFunction:
	PC_DSPL_PRICE = EXIT_STATUS

	EXIT FUNCTION

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	ON ERROR GO BACK

	END FUNCTION
