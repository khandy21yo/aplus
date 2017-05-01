1	%TITLE "Customer Master"
	%SBTTL "OE_MAST_CUSTOM"
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
	!	The ^*Customer Master\* option maintains
	!	specific information relative to each customer including the following:
	!	.table 3,25
	!	.te
	!	Number
	!	.te
	!	Name
	!	.te
	!	Alphabetic Sort
	!	.te
	!	Customer Type
	!	.te
	!	Category
	!	.te
	!	Onset Date
	!	.te
	!	Status Flags and Date
	!	.te
	!	Address
	!	.te
	!	Phone Number
	!	.te
	!	Method
	!	.te
	!	Statement Flag
	!	.te
	!	Service Charge Flag
	!	.end table
	!	More information is entered concerning each customer using the
	!	commands in the Command Menu. One of the commands is ^*Page\*, which
	!	will page to a second screen where tax code information, location, terms,
	!	carrier, salesman, credit limit, discount, and backorder information
	!	may be entered. Another command is ^*conTact\*. Accessing
	!	this command will present a screen where information may be entered
	!	concerning the contact person for this particular customer account.
	!	There is also a ^*Shipto\* command where a screen will contain information
	!	concerning order shipment, i.e. Name, address, carrier, salesman, and etc.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Maintain Masterfile
	!	.x Maintain>Customer Masterfile
	!	.x Address>Customer
	!	.x Maintain>Customer Address
	!	.x Add>Customer
	!	.x Erase>Customer
	!	.x Change>Customer
	!	.x Customer>Add
	!	.x Customer>Erase
	!	.x Customer>Change
	!
	! Option:
	!	AR_MAIN_35CUSTOM$HELP
	!	OE_MAIN_CONTACT$HELP
	!	OE_MAIN_SHIPTO$HELP
	!	OE_MAIN_CUSTDISC$HELP
	!	PC_MAIN_DEAL$HELP
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAST_CUSTOM/LINE
	!	$ LINK/EXEC:OE_EXE OE_MAST_CUSTOM,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_MAST_CUSTOM.OBJ;*
	!
	! Author:
	!
	! Modification history:
	!
	!	04/16/93 - Frank F. Starman
	!		Added OE_CUSTDISC
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
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

	!
	! Map's
	!
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Includes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD AR_35CUSTOM
	MAP (AR_35CUSTOM_OLD)	AR_35CUSTOM_CDD AR_35CUSTOM_OLD
	MAP (AR_35CUSTOM_ONE)	AR_35CUSTOM_CDD AR_35CUSTOM_ONE

	%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL.HB"
	MAP (PC_DEAL)		PC_DEAL_CDD PC_DEAL
	MAP (PC_DEAL_OLD)	PC_DEAL_CDD PC_DEAL_OLD
	MAP (PC_DEAL_ONE)	PC_DEAL_CDD PC_DEAL_ONE

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION AR_MAIN_CONTACT
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION OE_MAIN_SHIPTO
	EXTERNAL LONG FUNCTION OE_MAIN_CUSTDISC
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION MAIN_WINDOW
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN
	EXTERNAL LONG FUNCTION PC_MAIN_PRCTYPE
	EXTERNAL LONG FUNCTION PC_MAIN_DEAL
	EXTERNAL LONG FUNCTION PC_MAIN_DEAL_PRODUCT
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PD_MAIN_PRODTYPE
	EXTERNAL LONG FUNCTION PD_MAIN_CATEGORY

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_ENTRY
			AR_35CUSTOM_ONE = AR_35CUSTOM
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Shipto deaL prOd_disc"
		CASE OPT_MOREMENU
			AR_35CUSTOM_ONE = AR_35CUSTOM
			SELECT EDIT$(MVALUE, -1%)
			CASE "SHIPTO"
			!++
			! Abstract:SHIPTO
			!--
				MAINT_GROUP = MAIN_WINDOW(OE_MAIN_SHIPTO.ID, "")

			CASE "PROD_DISC"
			!++
			! Abstract:DISCOUNT
			!--
				MAINT_GROUP = &
					MAIN_WINDOW(OE_MAIN_CUSTDISC.ID, "")

			CASE "DEAL"
			!++
			! Abstract:DISCOUNT
			!--
				MAINT_GROUP = &
					MAIN_WINDOW(PC_MAIN_DEAL.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE
			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF AR_35CUSTOM::CUSNUM <> &
					AR_35CUSTOM_OLD::CUSNUM
				THEN
					AR_35CUSTOM_ONE = AR_35CUSTOM_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						OE_MAIN_SHIPTO.ID, "C")
					MAINT_GROUP = MAIN_WINDOW( &
						OE_MAIN_CUSTDISC.ID, "C")
					MAINT_GROUP = MAIN_WINDOW( &
						PC_MAIN_DEAL.ID, "C")
				END IF
			!
			! Erase records in subwindow
			!
			CASE "Erase"
				AR_35CUSTOM_ONE = AR_35CUSTOM
				MAINT_GROUP = MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
					"E")
				MAINT_GROUP = MAIN_WINDOW(OE_MAIN_CUSTDISC.ID, &
					"E")
				MAINT_GROUP = MAIN_WINDOW(PC_MAIN_DEAL.ID, "E")

			END SELECT
		END SELECT

	CASE OE_MAIN_SHIPTO.ID
		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = AR_35CUSTOM_ONE::CUSNUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = AR_35CUSTOM::CUSNUM
			CASE ELSE
				MVALUE = AR_35CUSTOM_ONE::CUSNUM
			END SELECT
		END SELECT

			MAINT_GROUP = OE_MAIN_SHIPTO(SMG_WINDOW, MOPTION, &
				MLOOP, MFLAG, MVALUE)


	CASE OE_MAIN_CUSTDISC.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = AR_35CUSTOM_ONE::CUSNUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = AR_35CUSTOM::CUSNUM
			CASE ELSE
				MVALUE = AR_35CUSTOM_ONE::CUSNUM
			END SELECT
		END SELECT

		MAINT_GROUP = OE_MAIN_CUSTDISC(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)


	CASE AR_MAIN_CONTACT.ID
		MAINT_GROUP = AR_MAIN_CONTACT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CUSTYPE.ID
		MAINT_GROUP = AR_MAIN_CUSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_CATEGORY.ID
		MAINT_GROUP = OE_MAIN_CATEGORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SALESTAX.ID
		MAINT_GROUP = OE_MAIN_SALESTAX(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID
		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_COUNTRY.ID
		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_TERMS.ID
		MAINT_GROUP = UT_MAIN_TERMS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_CARRIER.ID
		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SA_MAIN_SALESMAN.ID
		MAINT_GROUP = SA_MAIN_SALESMAN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_PRCTYPE.ID
		MAINT_GROUP = PC_MAIN_PRCTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODTYPE.ID
		MAINT_GROUP = PD_MAIN_PRODTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_CATEGORY.ID
		MAINT_GROUP = PD_MAIN_CATEGORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID
		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_DEAL.ID
		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = AR_35CUSTOM_ONE::CUSNUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%, 3%
				MVALUE = AR_35CUSTOM::CUSNUM
			CASE ELSE
				MVALUE = AR_35CUSTOM_ONE::CUSNUM
			END SELECT
		END SELECT

		MAINT_GROUP = PC_MAIN_DEAL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_ENTRY
			PC_DEAL_ONE = PC_DEAL

		END SELECT

	CASE PC_MAIN_DEAL_PRODUCT.ID
		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = PC_DEAL::DEAL
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%, 3%
				MVALUE = PC_DEAL::DEAL
			CASE ELSE
				MVALUE = PC_DEAL_ONE::DEAL
			END SELECT
		END SELECT

		MAINT_GROUP = PC_MAIN_DEAL_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
