1	%TITLE "Customer Name/Address Maintenance"
	%SBTTL "PC_SPEC_KILLASCIIPRICES"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1997 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_SPEC_KILLASCIIPRICES/LINE
	!	$ LINK/EXEC:PC_EXE PC_SPEC_KILLASCIIPRICES,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_SPEC_KILLASCIIPRICES.OBJ;*
	!
	! Author:
	!
	!	07/28/97 - Kevin Handy
	!
	! Modification history:
	!
	!	08/25/97 - Kevin Handy
	!		Clean up (Check)
	!		Lose definition for FUNC_ROUND
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/03/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST) PC_COST_CDD PC_COST

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE) PC_PRICE_CDD PC_PRICE

	%PAGE

100	OPEN "PRICES.TXT" FOR INPUT AS FILE 1%

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
	WHEN ERROR IN
		PRICE = VAL(MID(TEXT$, 29%, 14%)) / 1000000.0
	USE
		CONTINUE 1000
	END WHEN

	!*******************
	! ONLY DO THE P7 PRICE
	!*******************
 !	GOTO 1000 UNLESS PRICETYPE$ = "P7"

	GOTO 1200 IF PRICETYPE$ = "~~"

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
			WHEN ERROR IN
				DELETE #PC_PRICE.CH%
			USE
				CONTINUE 1190
			END WHEN

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
			WHEN ERROR IN
				DELETE #PC_COST.CH%
			USE
				CONTINUE 1290
			END WHEN

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
	GOTO 1000

5000	CLOSE #PC_PRICE.CH%
	CLOSE PC_COST.CH%
	CLOSE #1%

	GOTO 32767

32767	END
