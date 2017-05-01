1	%TITLE "Support System Customer Address Maintenance"
	%SBTTL "SS_MAST_CUSTOM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	!	.p
	!		This program calls the functions which maintain
	!	the SS (System Support) Customer file, and the related
	!	files, such as the Customer License file.  Thus, this is
	!	the Customer maintenance Master program.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SS_SOURCE:SS_MAST_CUSTOM/LINE
	!	$ LINK/EXE=SS_EXE: SS_MAST_CUSTOM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SS_MAST_CUSTOM.OBJ;*
	!
	! Author:
	!
	!	08/02/89 - Aaron Redd
	!
	! Modification history:
	!
	!	12/08/92 - Dan Perkins
	!		Removed cOntact from OPT_OPTLIST.  It was appearing
	!		twice on the option line since this is internal
	!		in the AR_MAIN_CUSTOM program.
	!
	!	12/10/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/21/92 - Kevin Handy
	!		Updated with additional tables that AR_MAIN_35CUSTOM
	!		now uses.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Included files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initialize maintenence
	!*******************************************************************

	!
	! Initialize the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	%PAGE

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************
	V% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Included files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions (and related memory MAPs)
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM
	MAP (AR_35CUSTOM_OLD)	AR_35CUSTOM_CDD		AR_35CUSTOM_OLD
	MAP (AR_35CUSTOM_ONE)	AR_35CUSTOM_CDD		AR_35CUSTOM_ONE

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	MAIN_WINDOW
	EXTERNAL LONG	FUNCTION	MAIN_JOURNAL
	EXTERNAL LONG	FUNCTION	AR_MAIN_CONTACT
	EXTERNAL LONG	FUNCTION	AR_MAIN_35CUSTOM
	EXTERNAL LONG	FUNCTION	SS_MAIN_CUS_SYSMENU
	EXTERNAL LONG	FUNCTION	SS_MAIN_LICENSE
	EXTERNAL LONG	FUNCTION	UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
			!
			! Modify the menu
			!
			CASE OPT_OPTLIST
				MVALUE = MVALUE + " System License"

			!
			! Optional menu items
			!
			CASE OPT_MOREMENU
				AR_35CUSTOM_ONE = AR_35CUSTOM

				SELECT MVALUE

				!
				! List of Contacts
				!
				CASE "cOntact"
					MAINT_GROUP = MAIN_JOURNAL(AR_MAIN_CONTACT.ID, "")

				!
				! List of systems customer has bought
				!
				CASE "System"
					MAINT_GROUP = MAIN_WINDOW(SS_MAIN_CUS_SYSMENU.ID, "")

				!
				! License maintenance
				!
				CASE "License"
					MAINT_GROUP = MAIN_WINDOW(SS_MAIN_LICENSE.ID, "")

				END SELECT

			!
			! Handle finishing various options specially
			!
			CASE OPT_AFTEROPT

				SELECT MVALUE

				!
				! Change records
				!
				CASE "Change", "Blank", "Initialize"
					!
					! Change line items to match new header
					! if the key was changed.
					!
					IF (AR_35CUSTOM_OLD::CUSNUM <> AR_35CUSTOM::CUSNUM)
					THEN
						AR_35CUSTOM_ONE = AR_35CUSTOM_OLD
						MAINT_GROUP = MAIN_WINDOW(SS_MAIN_CUS_SYSMENU.ID, "C")
						MAINT_GROUP = MAIN_JOURNAL(AR_MAIN_CONTACT.ID, "C")
						MAINT_GROUP = MAIN_WINDOW(SS_MAIN_LICENSE.ID, "C")
					END IF

				!
				! Erase record
				!
				CASE "Erase"
					!
					! Erase any line items under the header
					!
					AR_35CUSTOM_ONE = AR_35CUSTOM_OLD
					MAINT_GROUP = MAIN_WINDOW(SS_MAIN_CUS_SYSMENU.ID, "E")
					MAINT_GROUP = MAIN_JOURNAL(AR_MAIN_CONTACT.ID, "E")
					MAINT_GROUP = MAIN_WINDOW(SS_MAIN_LICENSE.ID, "E")

				END SELECT

		END SELECT

	CASE AR_MAIN_CONTACT.ID

		MAINT_GROUP = AR_MAIN_CONTACT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SS_MAIN_CUS_SYSMENU.ID

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

		MAINT_GROUP = SS_MAIN_CUS_SYSMENU(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SS_MAIN_LICENSE.ID

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

		MAINT_GROUP = SS_MAIN_LICENSE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_COUNTRY.ID

		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
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

	END SELECT

32767	END FUNCTION
