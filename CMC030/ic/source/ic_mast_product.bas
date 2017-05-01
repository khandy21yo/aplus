1	%TITLE "Product Master"
	%SBTTL "IC_MAST_PRODUCT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Master File\* option accesses routines
	!	where records for new or additional products are maintained.
	!	.lm -5
	!
	! Index:
	!	.x Add>Product
	!	.x Erase>Product
	!	.x Change>Product
	!	.x Maintain>Product Master File
	!	.x Product Master File>Maintenance
	!	.x Product>Add
	!	.x Product>Erase
	!	.x Product>Change
	!	.x Maintain>Price
	!	.x Price>Maintenance
	!	.x Add>Price
	!	.x Erase>Price
	!	.x Change>Price
	!	.x Price>Add
	!	.x Price>Erase
	!	.x Price>Change
	!	.x Maintain>Cost
	!	.x Cost>Maintenance
	!	.x Add>Cost
	!	.x Erase>Cost
	!	.x Change>Cost
	!	.x Cost>Add
	!	.x Cost>Erase
	!	.x Cost>Change
	!	.x Maintain>Bin Location & Cycle Map
	!	.x Bin Location & Cycle Map>Maintenance
	!	.x Add>Bin Location & Cycle Map
	!	.x Erase>Bin Location & Cycle Map
	!	.x Change>Bin Location & Cycle Map
	!	.x Bin Location & Cycle Map>Add
	!	.x Bin Location & Cycle Map>Erase
	!	.x Bin Location & Cycle Map>Change
	!
	! Option:
	!
	!	PD_MAIN_PRODUCT$HELP
	!	PC_MAIN_PRICE$HELP
	!	PC_MAIN_COST$HELP
	!	IC_MAIN_BINMAP$HELP
	!	IC_MAIN_BINMAPSCAN$HELP
	!	OE_MAIN_PRODPROMO$HELP
	!	PD_MAIN_SUBSTITUTE$HELP
	!	PO_MAIN_PRODCROSS$HELP
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAST_PRODUCT/NOLINE
	!	$ LINK/EXE=IC_EXE: IC_MAST_PRODUCT,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_MAST_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	05/12/88 - Frank Starman
	!
	! Modification History:
	!
	!	02/20/92 - Frank F. Starman
	!		Remove the pack.
	!
	!	05/27/94 - Kevin Handy
	!		Added in the Substitute part number display.
	!
	!	05/27/94 - Kevin Handy
	!		Added "promo" option from OE_MAST_PRODUCT.
	!
	!	05/27/94 - Kevin Handy
	!		Added product cross reference from PO_MAST_PRODUCT.
	!
	!	05/31/94 - Kevin Handy
	!		Changed the letter for "Vendor-product" from a "P"
	!		which conflicted with "Price" to an "O".
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Include OE_WINDOW.INC
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/13/97 - Kevin Handy
	!		Added abaiably to see OE_MAIN_PROMO.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE


	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)		PD_PRODUCT_CDD	PD_PRODUCT
	MAP	(PD_PRODUCT_OLD)	PD_PRODUCT_CDD	PD_PRODUCT_OLD
	MAP	(PD_PRODUCT_ONE)	PD_PRODUCT_CDD	PD_PRODUCT_ONE

	EXTERNAL LONG FUNCTION MAIN_WINDOW
	EXTERNAL LONG FUNCTION MAIN_JOURNAL

	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR

	EXTERNAL LONG FUNCTION IC_MAIN_BINMAP
	EXTERNAL LONG FUNCTION IC_MAIN_BINMAPSCAN

	EXTERNAL LONG FUNCTION OE_MAIN_PRODPROMO
	EXTERNAL LONG FUNCTION OE_MAIN_PROMO

	EXTERNAL LONG FUNCTION PC_MAIN_PRICE
	EXTERNAL LONG FUNCTION PC_MAIN_COST
	EXTERNAL LONG FUNCTION PC_MAIN_PRCTYPE

	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PD_MAIN_PRODTYPE
	EXTERNAL LONG FUNCTION PD_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION PD_MAIN_LABEL
	EXTERNAL LONG FUNCTION PD_MAIN_SUBSTITUTE
	EXTERNAL LONG FUNCTION PO_MAIN_PRODCROSS

	EXTERNAL LONG FUNCTION UTL_MAIN_MEASURE
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PD_MAIN_PRODUCT.ID
		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_OPTLIST
			MVALUE = MVALUE + &
				" Price cosT cYcle proMo Substitute " + &
				"vendOr-product"

		CASE OPT_MOREMENU
			PD_PRODUCT_ONE = PD_PRODUCT
			SELECT EDIT$(MVALUE, -1%)

			!
			! Cycle Map
			!
			CASE "CYCLE"
				MAINT_GROUP = &
					MAIN_WINDOW(IC_MAIN_BINMAP.ID, "")

			!
			! Price
			!
			CASE "PRICE"
				MAINT_GROUP = &
					MAIN_WINDOW(PC_MAIN_PRICE.ID, "")

			!
			! Cost
			!
			CASE "COST"
				MAINT_GROUP = &
					MAIN_WINDOW(PC_MAIN_COST.ID, "")

			!
			! Promotional
			!
			CASE "PROMO"
				MAINT_GROUP = MAIN_WINDOW(OE_MAIN_PRODPROMO.ID, "")

			!
			! Substitute
			!
			CASE "SUBSTITUTE"
				MAINT_GROUP = &
					MAIN_JOURNAL(PD_MAIN_SUBSTITUTE.ID, "")

			!
			! Vendor Cross Reference
			!
			CASE "VENDOR-PRODUCT"
				MAINT_GROUP = &
					MAIN_WINDOW(PO_MAIN_PRODCROSS.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF PD_PRODUCT::PRODUCT_NUM <> &
					PD_PRODUCT_OLD::PRODUCT_NUM
				THEN
					PD_PRODUCT_ONE = PD_PRODUCT_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						PC_MAIN_COST.ID, "C")

					MAINT_GROUP = MAIN_WINDOW( &
						PC_MAIN_PRICE.ID, "C")

					MAINT_GROUP = MAIN_WINDOW( &
						IC_MAIN_BINMAP.ID, "C")

					MAINT_GROUP = MAIN_WINDOW( &
						OE_MAIN_PRODPROMO.ID, "C")

					MAINT_GROUP = MAIN_JOURNAL( &
						PD_MAIN_SUBSTITUTE.ID, "C")

					MAINT_GROUP = MAIN_WINDOW( &
						PO_MAIN_PRODCROSS.ID, "C")
				END IF
			!
			! Need to remove text
			!
			CASE "Erase"
				PD_PRODUCT_ONE = PD_PRODUCT
				MAINT_GROUP = MAIN_WINDOW(PC_MAIN_COST.ID, "E")
				MAINT_GROUP = MAIN_WINDOW(PC_MAIN_PRICE.ID, "E")
				MAINT_GROUP = MAIN_WINDOW(IC_MAIN_BINMAP.ID, "E")
				MAINT_GROUP = MAIN_WINDOW(OE_MAIN_PRODPROMO.ID, "E")
				MAINT_GROUP = MAIN_JOURNAL(PD_MAIN_SUBSTITUTE.ID, "E")
				MAINT_GROUP = MAIN_WINDOW(PO_MAIN_PRODCROSS.ID, "E")

			END SELECT
		END SELECT

	CASE IC_MAIN_BINMAP.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = PD_PRODUCT::PRODUCT_NUM
			CASE ELSE
				MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM
			END SELECT
		END SELECT

		MAINT_GROUP = IC_MAIN_BINMAP(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " recOrd"
		CASE OPT_MOREMENU
			MAINT_GROUP = 16% OR MAIN_WINDOW(IC_MAIN_BINMAPSCAN.ID, "")
		END SELECT

	CASE IC_MAIN_BINMAPSCAN.ID
			MAINT_GROUP = IC_MAIN_BINMAPSCAN(SMG_WINDOW, &
				MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODTYPE.ID
		MAINT_GROUP = PD_MAIN_PRODTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_CATEGORY.ID
		MAINT_GROUP = PD_MAIN_CATEGORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_LABEL.ID
		MAINT_GROUP = PD_MAIN_LABEL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID
		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_MEASURE.ID
		MAINT_GROUP = UTL_MAIN_MEASURE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_COST.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM

		CASE OPT_SUBWIND

			SELECT MLOOP
			CASE 6%
				MVALUE = PD_PRODUCT::PRODUCT_NUM
			CASE ELSE
				MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM
			END SELECT

		END SELECT

		MAINT_GROUP = PC_MAIN_COST(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_PRICE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM

		CASE OPT_SUBWIND

			SELECT MLOOP
			CASE 6%
				MVALUE = PD_PRODUCT::PRODUCT_NUM
			CASE ELSE
				MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM
			END SELECT

		END SELECT

		MAINT_GROUP = PC_MAIN_PRICE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_PRODPROMO.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = PD_PRODUCT::PRODUCT_NUM
			CASE ELSE
				MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM
			END SELECT
		END SELECT

		MAINT_GROUP = OE_MAIN_PRODPROMO(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_PROMO.ID

		MAINT_GROUP = OE_MAIN_PROMO(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_PRODCROSS.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = PD_PRODUCT::PRODUCT_NUM

			CASE ELSE
				MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM

			END SELECT

		END SELECT

		MAINT_GROUP = PO_MAIN_PRODCROSS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_PRCTYPE.ID
		MAINT_GROUP = PC_MAIN_PRCTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_SUBSTITUTE.ID
		MAINT_GROUP = PD_MAIN_SUBSTITUTE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_VENDOR.ID
		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
