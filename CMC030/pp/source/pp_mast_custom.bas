1	%TITLE "Pacific Pride Customer Master"
	%SBTTL "PP_MAST_CUSTOM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	!	commands in the Command Menu, which includes maintenance of the
	!	Card File and the Card Exemption File. One of the commands is ^*Page\*, which
	!	will page to a second screen where tax code information, location, terms,
	!	carrier, salesman, credit limit, discount, and backorder information
	!	may be entered. Another command is ^*conTact\*. Accessing
	!	this command will present a screen where information may be entered
	!	concerning the contact person for this particular customer account.
	!	.b
	!	The ^*Card File\* contains information on the Card Number issued to a user,
	!	the Type of Card, a description field, and a field for an odometer reading.
	!	.b
	!	The ^*Card Exemption File\* lists the state, authority, and product that would
	!	be exempted for any reason.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!	AR_MAIN_35CUSTOM$HELP
	!	PP_MAIN_CARD$HELP
	!	PP_MAIN_CARDEXEMPT$HELP
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_MAST_CUSTOM/LINE
	!	$ LINK/EXEC:PP_EXE PP_MAST_CUSTOM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_MAST_CUSTOM.OBJ;*
	!
	! Author:
	!
	!	12/21/92 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
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

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	COM (CH_AR_35CUSTOM) &
		AR_35CUSTOM.CH%

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

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM
	MAP (AR_35CUSTOM_OLD)	AR_35CUSTOM_CDD		AR_35CUSTOM_OLD
	MAP (AR_35CUSTOM_ONE)	AR_35CUSTOM_CDD		AR_35CUSTOM_ONE

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD)		PP_CARD_CDD		PP_CARD
	MAP (PP_CARD_OLD)	PP_CARD_CDD		PP_CARD_OLD
	MAP (PP_CARD_ONE)	PP_CARD_CDD		PP_CARD_ONE

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION AR_MAIN_CONTACT
	EXTERNAL LONG FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PP_MAIN_CARD
	EXTERNAL LONG FUNCTION PP_MAIN_CARDEXEMPT
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UTL_MAIN_STATE

	EXTERNAL LONG FUNCTION MAIN_WINDOW

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
			MVALUE = MVALUE + " cardnO"

		CASE OPT_MOREMENU
			AR_35CUSTOM_ONE = AR_35CUSTOM

			SELECT EDIT$(MVALUE, -1%)

			CASE "CARDNO"
	!++
	! Abstract:CARDNO
	!--
				MAINT_GROUP = MAIN_WINDOW(PP_MAIN_CARD.ID, "")

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
						PP_MAIN_CARD.ID, "C")
				END IF
			!
			! Erase records in subwindow
			!
			CASE "Erase"
				AR_35CUSTOM_ONE = AR_35CUSTOM

				MAINT_GROUP = &
					MAIN_WINDOW(PP_MAIN_CARD.ID, "E")

			END SELECT

		END SELECT

	CASE AR_MAIN_CONTACT.ID

		MAINT_GROUP = AR_MAIN_CONTACT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CUSTYPE.ID

		MAINT_GROUP = AR_MAIN_CUSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PP_MAIN_CARD.ID

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

		CASE OPT_OPTLIST

			MVALUE = MVALUE + " exempT"

		CASE OPT_MOREMENU
			PP_CARD_ONE = PP_CARD

			SELECT EDIT$(MVALUE, -1%)

			CASE "EXEMPT"
	!++
	! Abstract:PPCARDEXEMPT
	!--
				MAINT_GROUP = MAIN_WINDOW(PP_MAIN_CARDEXEMPT.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Add
			!
			CASE "Add"
				PP_CARD_ONE = PP_CARD

				MAINT_GROUP = MAIN_WINDOW( &
					PP_MAIN_CARDEXEMPT.ID, "A")

				PP_CARD = PP_CARD_ONE

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF PP_CARD::CUSNUM + PP_CARD::CARD <> &
					PP_CARD_OLD::CUSNUM + PP_CARD_OLD::CARD
				THEN
					PP_CARD_ONE = PP_CARD_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						PP_MAIN_CARDEXEMPT.ID, "C")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				PP_CARD_ONE = PP_CARD

				MAINT_GROUP = &
					MAIN_WINDOW(PP_MAIN_CARDEXEMPT.ID, "E")

			END SELECT

		END SELECT

		MAINT_GROUP = PP_MAIN_CARD(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PP_MAIN_CARDEXEMPT.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = PP_CARD::CUSNUM + PP_CARD::CARD

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = PP_CARD::CUSNUM + PP_CARD::CARD

			CASE ELSE
				MVALUE = PP_CARD_ONE::CUSNUM + PP_CARD_ONE::CARD

			END SELECT

		END SELECT

		MAINT_GROUP = PP_MAIN_CARDEXEMPT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_CATEGORY.ID

		MAINT_GROUP = OE_MAIN_CATEGORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SALESTAX.ID

		MAINT_GROUP = OE_MAIN_SALESTAX(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SA_MAIN_SALESMAN.ID

		MAINT_GROUP = SA_MAIN_SALESMAN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_CARRIER.ID

		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_COUNTRY.ID

		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_STATE.ID

		MAINT_GROUP = UTL_MAIN_STATE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_TERMS.ID

		MAINT_GROUP = UT_MAIN_TERMS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
