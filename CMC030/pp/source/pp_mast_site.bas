1	%TITLE "Pacific Pride Site Maintenance"
	%SBTTL "PP_MAST_SITE"
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
	!	.lm +5
	!	.b
	!	The ^*Site Maintenance\* option
	!	accesses the file where the Pacific Pride Site records are
	!	maintained.  Data in this file includes the host number,
	!	site code, site type, site name, address, city, state, zip,
	!	local sale location, foreign sale location, and foreign purchase
	!	location.
	!	.b
	!	Also included in this program is the Site Product File which
	!	identifies a particular product, the associated taxing agencies,
	!	and the general ledger account numbers to which taxes will be
	!	assigned.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!	PP_MAIN_SITE$HELP
	!	PP_MAIN_SITE_PRODUCT$HELP
	!
	! Compile:
	!
	!	$ BAS SOURCE:[PP.SOURCE]PP_MAST_SITE/LINE
	!	$ LINK/EXEC:PP_EXE PP_MAST_SITE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_MAST_SITE.OBJ;*
	!
	! Author:
	!
	!	12/29/92 - Dan Perkins
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

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"

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

	V% = MAIN_WINDOW(PP_MAIN_SITE.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE)		PP_SITE_CDD		PP_SITE
	MAP (PP_SITE_OLD)	PP_SITE_CDD		PP_SITE_OLD
	MAP (PP_SITE_ONE)	PP_SITE_CDD		PP_SITE_ONE

	EXTERNAL LONG FUNCTION PP_MAIN_SITE
	EXTERNAL LONG FUNCTION PP_MAIN_SITE_PRODUCT

	EXTERNAL LONG FUNCTION MAIN_WINDOW
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION UTL_MAIN_STATE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PP_MAIN_SITE.ID

		MAINT_GROUP = PP_MAIN_SITE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			PP_SITE_ONE = PP_SITE

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Product"

		CASE OPT_MOREMENU
			PP_SITE_ONE = PP_SITE

			SELECT EDIT$(MVALUE, -1%)

			CASE "PRODUCT"
	!++
	! Abstract:PRODUCT
	!--
				MAINT_GROUP = &
					MAIN_WINDOW(PP_MAIN_SITE_PRODUCT.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF PP_SITE::HOST + &
					PP_SITE::SITE + &
					PP_SITE::STYPE <> &
					PP_SITE_OLD::HOST + &
					PP_SITE_OLD::SITE + &
					PP_SITE_OLD::STYPE
				THEN
					PP_SITE_ONE = PP_SITE_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						PP_MAIN_SITE_PRODUCT.ID, "C")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				PP_SITE_ONE = PP_SITE

				MAINT_GROUP = &
					MAIN_WINDOW(PP_MAIN_SITE_PRODUCT.ID, "E")

			END SELECT

		END SELECT

	CASE PP_MAIN_SITE_PRODUCT.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = PP_SITE_ONE::HOST + &
				PP_SITE_ONE::SITE + &
				PP_SITE_ONE::STYPE

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = PP_SITE::HOST + &
					PP_SITE::SITE + &
					PP_SITE::STYPE

			CASE ELSE
				MVALUE = PP_SITE_ONE::HOST + &
					PP_SITE_ONE::SITE + &
					PP_SITE_ONE::STYPE

			END SELECT

		END SELECT

		MAINT_GROUP = PP_MAIN_SITE_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_STATE.ID

		MAINT_GROUP = UTL_MAIN_STATE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
