1	%TITLE "Product Master Maintenance"
	%SBTTL "PC_MAST_PRODUCT"
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
	!	The ^*Product Master\* file option accesses
	!	routines where records for new or additional products are
	!	entered and maintained.  The screen also
	!	accesses the Standard Cost File and Product Price File
	!	.lm -5
	!
	! Index:
	!	.x Add>Product
	!	.x Erase>Product
	!	.x Change>Product
	!	.x Maintain>Product
	!	.x Product>Maintain
	!	.x Product>Add
	!	.x Product>Erase
	!	.x Product>Change
	!
	! Option:
	!
	!	PD_MAIN_PRODUCT$HELP
	!	PC_MAIN_PRICE$HELP
	!	PC_MAIN_PRICESCAN$HELP
	!	PC_MAIN_COST$HELP
	!	PC_MAIN_COSTSCAN$HELP
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_MAST_PRODUCT/NOLINE
	!	$ LINK/EXE=PC_EXE: PC_MAST_PRODUCT,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_MAST_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	06/23/88 - Frank Starman
	!
	! Modification History:
	!
	!	02/26/92 - Kevin Handy
	!		Added modification history section.
	!
	!	02/26/92 - Kevin Handy
	!		Disabled calling function PD_MAIN_PACK since
	!		Frank deleted it so that nothing could compile.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
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


	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD PD_PRODUCT
	MAP (PD_PRODUCT_OLD)	PD_PRODUCT_CDD PD_PRODUCT_OLD
	MAP (PD_PRODUCT_ONE)	PD_PRODUCT_CDD PD_PRODUCT_ONE

	EXTERNAL LONG FUNCTION MAIN_WINDOW

	EXTERNAL LONG FUNCTION PC_MAIN_PRICE
	EXTERNAL LONG FUNCTION PC_MAIN_PRICESCAN
	EXTERNAL LONG FUNCTION PC_MAIN_COST
	EXTERNAL LONG FUNCTION PC_MAIN_COSTSCAN

	EXTERNAL LONG FUNCTION PC_MAIN_PRCTYPE

	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PD_MAIN_PRODTYPE
	EXTERNAL LONG FUNCTION PD_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION PD_MAIN_LABEL

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
			MVALUE = MVALUE + " Price cosT "

		CASE OPT_MOREMENU
			PD_PRODUCT_ONE = PD_PRODUCT
			SELECT EDIT$(MVALUE, -1%)
			!
			! Price
			!
			CASE "PRICE"
				MAINT_GROUP = MAIN_WINDOW(PC_MAIN_PRICE.ID, "")
			!
			! Cost
			!
			CASE "COST"
				MAINT_GROUP = MAIN_WINDOW(PC_MAIN_COST.ID, "")
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
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				PD_PRODUCT_ONE = PD_PRODUCT
				MAINT_GROUP = MAIN_WINDOW(PC_MAIN_COST.ID, "E")
				MAINT_GROUP = MAIN_WINDOW(PC_MAIN_PRICE.ID, "E")
			END SELECT
		END SELECT

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

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " recOrd"

		CASE OPT_MOREMENU
			MAINT_GROUP = 16% OR MAIN_WINDOW(PC_MAIN_COSTSCAN.ID, "")
		END SELECT

	CASE PC_MAIN_COSTSCAN.ID
		MAINT_GROUP = PC_MAIN_COSTSCAN(SMG_WINDOW, &
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

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " recOrd"

		CASE OPT_MOREMENU
			MAINT_GROUP = 16% OR MAIN_WINDOW(PC_MAIN_PRICESCAN.ID, "")
		END SELECT

	CASE PC_MAIN_PRICESCAN.ID
		MAINT_GROUP = PC_MAIN_PRICESCAN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_PRCTYPE.ID
		MAINT_GROUP = PC_MAIN_PRCTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
