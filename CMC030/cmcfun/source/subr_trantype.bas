1	%TITLE "Subroutine to determine Transaction Types"
	%SBTTL "SUBR_TRANTYPE"
	%IDENT "V3.6a Calico"

	SUB SUBR_TRANTYPE(STRING CUSNUM, STRING LIN, REAL ORDQTY, &
		REAL SHPQTY, REAL BCKQTY, STRING TRANTYPE(), REAL TRANQTY())

	!
	! COPYRIGHT (C) 1983 BY
	! Computer Management Center
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
	!	This subroutine will detirmine transaction types based on
	!	the amount of the quantities passed to the subroutine.
	!	Transaction types are also based on whether on not the
	!	CUSNUM field is blank.
	!	.b
	!	Transaction Types include:
	!	\========================================================
	!	.table 3,25
	!	.te
	!	SO	Sales Order
	!	.te
	!	SA	Sales
	!	.te
	!	LS	Lost Sale
	!	.te
	!	TR	Transfer
	!	.te
	!	WA	Waranty
	!	.te
	!	RT	Return
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:SUBR_TRANTYPE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP SUBR_TRANTYPE
	!	$ DELETE SUBR_TRANTYPE.OBJ;*
	!
	! Author:
	!
	!	03/19/93 - Dan Perkins
	!
	! Modification history:
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/26/93 - Frank F. Starman
	!		Fixed returned array.
	!
	!	06/23/93 Kevin Handy
	!		Clean up (Check)
	!
	!	07/18/93 - Frank F. Starman
	!		Added LL type.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	%PAGE

	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	EXTERNAL REAL FUNCTION FUNC_ROUND

	!
	! Initialize variables
	!
	TRANQTY(I%) = 0.0 FOR I% = 0% TO 9%

	TRANTYPE(1%) = "RTInv Ret  "	! Inventory Return
	TRANTYPE(2%) = "SAInvoiced "	! Sales
	TRANTYPE(3%) = "TRTransfer "	! Transfer
	TRANTYPE(4%) = "  Credit   "	! Credit
	TRANTYPE(5%) = "WAWarranty "	! Waranty

	IF LIN = "NEWL"
	THEN
		TRANTYPE(6%) = "LSLost Sale"	! Lost Sale on a new order
	ELSE
		TRANTYPE(6%) = "LLLost Sale"	! Lost Sale on a back order
	END IF

	TRANTYPE(7%) = "SOCancel   "	! Cancel
	TRANTYPE(8%) = "SOOn Order "	! Order
	TRANTYPE(9%) = "SOOrder Off"	! Order Off

	IF ORDQTY > 0.0 AND SHPQTY = 0.0 AND BCKQTY >= 0.0
	THEN
		BCKQTY = ORDQTY IF BCKQTY > ORDQTY
		TRANQTY(8%) = -BCKQTY
		TRANQTY(6%) = ORDQTY - BCKQTY
	END IF

	IF ORDQTY > 0.0 AND SHPQTY > 0.0 AND BCKQTY >= 0.0
	THEN
		BCKQTY = 0.0 IF SHPQTY >= ORDQTY
		TRANQTY(2%) = -SHPQTY
		TRANQTY(6%) = ORDQTY - SHPQTY - BCKQTY &
			IF ORDQTY - SHPQTY - BCKQTY > 0.0

		IF LIN = "NEWL"
		THEN
			TRANQTY(8%) = -BCKQTY
		ELSE
			TRANQTY(9%) = ORDQTY - BCKQTY
			TRANQTY(9%) = SHPQTY IF SHPQTY < TRANQTY(9%)
		END IF
	END IF

	IF ORDQTY > 0.0 AND SHPQTY >= 0.0 AND BCKQTY < 0.0 AND LIN <> "NEWL"
	THEN
		BCKQTY = 0.0 IF SHPQTY >= ORDQTY
		TRANQTY(2%) = -SHPQTY
		TRANQTY(7%) = -BCKQTY
		TRANQTY(7%) = ORDQTY IF TRANQTY(7%) > ORDQTY
	END IF

	!
	! return
	!
	IF ORDQTY <= 0.0 AND SHPQTY < 0.0 AND BCKQTY = 0.0
	THEN
		ORDQTY = SHPQTY IF SHPQTY > ORDQTY
		TRANQTY(1%) = -ORDQTY
		TRANQTY(4%) = -SHPQTY
	END IF

	!
	! sales warranty
	!
	IF ORDQTY <= 0.0 AND SHPQTY >= 0.0 AND BCKQTY = 0.0
	THEN
		TRANQTY(1%) = -ORDQTY
		TRANQTY(5%) = -SHPQTY
	END IF

	IF CUSNUM = ""
	THEN
		TRANQTY(3%) = TRANQTY(2%)
		TRANQTY(2%) = 0.0
	END IF

 ExitSub:
	J% = 0%
	FOR I% = 1% TO 9%
		IF FUNC_ROUND(TRANQTY(I%), 5%) <> 0.0
		THEN
			J% = J% + 1%
			TRANQTY(0%) = TRANQTY(0%) + 2% ^ I%
			TRANQTY(J%) = TRANQTY(I%)
			TRANTYPE(J%) = TRANTYPE(I%) + "(" + &
				NUM1$(FUNC_ROUND(ABS(TRANQTY(I%)), 0%)) + ")   "
			IF J% < I%
			THEN
				TRANQTY(I%) = 0.0
				TRANTYPE(I%) = "                 "
			END IF
		ELSE
			TRANTYPE(I%) = "                 "
		END IF

	NEXT I%

	TRANTYPE(0%) = NUM1$(J%)
	TRANTYPE(1%) = "  undefined...   " IF J% = 0%

32767	END SUB
