1	%TITLE "Calculate the variance for a AP line item"
	%SBTTL "AP_FUNC_CALCVARIANCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_FUNC_CALCVARIANCE( &
		AP_PONUM$,		! PO Number input &
		AP_POLINE$,		! PO Line number input &
		AP_ACCOUNT$,		! GL Account input &
		AP_QTY,			! Quanity input &
		AP_AMOUNT,		! Dollars input &
 &
		PO_ACCOUNT$,		! Variance account out &
		PO_VARIANCE,		! Variance amount out &
		PO_AMOUNT,		! Non-variance amount out &
 &
		COST_DATE$		! Date to use to look up cost &
		)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	This function calculates the variances for AP lines
	!	that interface to the PO system.
	!	.p
	!	Note: This function does not know about any PO lines
	!	that do not exist in the register, and will assume that
	!	they will not have a varience (since I can't get a part
	!	number anyway)
	!
	! Index:
	!
	! Input:
	!
	!	AP_PONUM$
	!		PO Number input
	!
	!	AP_POLINE$
	!		PO Line number input
	!
	!	AP_ACCOUNT$
	!		GL Account input
	!
	!	AP_QTY
	!		Quanity input
	!
	!	AP_AMOUNT
	!		Dollars input
	!
	! Output:
	!
	!	PO_ACCOUNT$
	!		Variance account out
	!
	!	PO_VARIANCE
	!		Variance amount out
	!
	!	PO_AMOUNT
	!		Non-variance amount out &
	!
	!	AP_FUNC_VARIANCE
	!		Status
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_FUNC_CALCVARIANCE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AP_FUNC_CALCVARIANCE
	!	$ DELETE AP_FUNC_CALCVARIANCE.OBJ;*
	!
	! Author:
	!
	!	09/01/92 - Kevin Handy
	!
	! Modification history:
	!
	!	11/04/94 - Kevin Handy
	!		Added parameter to PO_READ_REG_LINE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/06/96 - Kevin Handy
	!		Reformat source code
	!
	!	11/27/96 - Kevin Handy
	!		Lose unused definition of PC_COST_READ.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/07/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	08/15/2000 -Kevin Handy
	!		Add a field to determine the date to use for
	!		the cost, instead of using the PO Order date. (robson)
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	DECLARE	PO_REG_LINE_CDD		PO_REG_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE	PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Statements
	!
	COM (CH_PD_PRODUCT) &
		PD_PRODUCT.CH%, &
		PD_PRODUCT.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION PO_READ_REG_LINE
	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL LONG	FUNCTION PD_READ_ACCOUNT

	%PAGE

1000	!*******************************************************************
	! See if there is a PO number on this AP record
	!*******************************************************************

	IF AP_PONUM$ = ""
	THEN
		PO_ACCOUNT$ = SPACE$(18%)
		PO_VARIANCE = 0.0
		PO_AMOUNT = AP_AMOUNT

		AP_FUNC_CALCVARIANCE = CMC$_NORMAL
		GOTO ExitFunction
	END IF

2000	!*******************************************************************
	! See if such a PO line exists in the PO Register
	!*******************************************************************

	TEST% = PO_READ_REG_LINE( &
		AP_PONUM$, AP_POLINE$, &
		"EQ", PO_REG_LINE_READ, &
		PO_REG_SUB_LINE_READ, QTY(), "")

	IF TEST% <> CMC$_NORMAL
	THEN
		PO_ACCOUNT$ = SPACE$(18%)
		PO_VARIANCE = 0.0
		PO_AMOUNT = AP_AMOUNT

		AP_FUNC_CALCVARIANCE = TEST%
		GOTO ExitFunction
	END IF

3000	!*******************************************************************
	! PO Line exists in PO Register, now look up the standard cost
	! and calculate amount/variance
	!*******************************************************************

	UNIT_COST = PC_READ_COST(PO_REG_LINE_READ::PRODUCT, &
		PO_REG_LINE_READ::FROMLOCATION, &
		COST_DATE$, EFFDATE$)

	IF (UNIT_COST <> 0.0)
	THEN
		PO_AMOUNT = FUNC_ROUND(UNIT_COST * AP_QTY, 2%)
		PO_VARIANCE = FUNC_ROUND(AP_AMOUNT - PO_AMOUNT, 2%)
	ELSE
		PO_AMOUNT = AP_AMOUNT
		PO_VARIANCE = 0.0
	END IF

4000	!*******************************************************************
	! Now, we need to get the varience account
	!*******************************************************************

	PO_ACCOUNT$ = SPACE$(18%)

	!
	! Get product type
	!
	IF PD_PRODUCT.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
		USE
			AP_FUNC_CALCVARIANCE = CMC$_ERROR
			CONTINUE ExitFunction
		END WHEN
	END IF

4100	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PO_REG_LINE_READ::PRODUCT, REGARDLESS
	USE
		AP_FUNC_CALCVARIANCE = CMC$_ERROR
		CONTINUE ExitFunction
	END WHEN

	!
	! Hopefully pull in the account definitions
	!
	TEST% = PD_READ_ACCOUNT( &
		PO_REG_LINE_READ::FROMLOCATION, &
		PD_PRODUCT::PROD_TYPE, &
		PD_ACCOUNT_READ)

	PO_ACCOUNT$ = PD_ACCOUNT_READ::PRICEVARACCT

 ExitFunction:

	EXIT FUNCTION

	END FUNCTION
