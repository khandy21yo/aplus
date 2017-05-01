1	%TITLE "Asset Master Maintenance"
	%SBTTL "AD_MAST_ASSET"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987 BY
	!
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
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	The ^*Asset Master\* option enters a new asset
	!	and elect its depreciation object.
	!	.LM -5
	!
	! Index:
	!	.x Asset>Master File
	!	.x Master>Asset File
	!	.x Depreciation>Description File
	!
	! Option:
	!	AD_MAIN_ASSET$HELP
	!	AD_MAIN_DEPRECIATION$HELP
	!	AD_MAIN_DEPRECIATIONSCAN$HELP
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAST_ASSET/LINE
	!	$ LINK/EXE=AD_EXE: AD_MAST_ASSET,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_MAST_ASSET.OBJ;*
	!
	! Author:
	!
	!	12/02/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/25/91 - Craig Tanner
	!		Removed use of AD_RETIRED file. AD_ASSET and AD_RETIRED
	!		combined in to AD_35ASSET.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	10/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
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
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AD_MAIN_ASSET.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE


	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP	(AD_35ASSET)	AD_35ASSET_CDD	AD_35ASSET
	MAP	(AD_35ASSET_OLD)	AD_35ASSET_CDD	AD_35ASSET_OLD
	MAP	(AD_35ASSET_ONE)	AD_35ASSET_CDD	AD_35ASSET_ONE

	EXTERNAL LONG FUNCTION AD_MAIN_ASSET
	EXTERNAL LONG FUNCTION AD_MAIN_ASSTYPE
	EXTERNAL LONG FUNCTION AD_MAIN_OBJECT
	EXTERNAL LONG FUNCTION AD_MAIN_DEPCLASS
	EXTERNAL LONG FUNCTION AD_MAIN_METHOD
	EXTERNAL LONG FUNCTION AD_MAIN_CONVENTION
	EXTERNAL LONG FUNCTION AD_MAIN_DEPRECIATION
	EXTERNAL LONG FUNCTION AD_MAIN_DEPRECIATIONSCAN

	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_DEPARTMENT

	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AD_MAIN_ASSET.ID

		MAINT_GROUP = AD_MAIN_ASSET(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " dePreciation"

		CASE OPT_MOREMENU
			AD_35ASSET_ONE = AD_35ASSET
			SELECT EDIT$(MVALUE, -1%)
			!
			! Asset Depreciation
			!
			CASE "DEPRECIATION"
				MAINT_GROUP = &
					MAIN_WINDOW(AD_MAIN_DEPRECIATION.ID, "")
			END SELECT

		CASE OPT_AFTEROPT
			SELECT MVALUE
			CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			IF AD_35ASSET_OLD::ASSET_NUM <> AD_35ASSET::ASSET_NUM
			THEN
				AD_35ASSET_ONE = AD_35ASSET_OLD
				MAINT_GROUP = &
					MAIN_WINDOW(AD_MAIN_DEPRECIATION.ID, &
					"C" + AD_35ASSET_OLD::ASSET_NUM)

			END IF
			!
			! Need to remove text
			!
			CASE "Erase"
				AD_35ASSET_ONE = AD_35ASSET
				MAINT_GROUP = &
					MAIN_WINDOW(AD_MAIN_DEPRECIATION.ID, "E")
			END SELECT
		END SELECT

	CASE AD_MAIN_ASSTYPE.ID

		MAINT_GROUP = AD_MAIN_ASSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_DEPARTMENT.ID

		MAINT_GROUP = UTL_MAIN_DEPARTMENT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_OBJECT.ID

		MAINT_GROUP = AD_MAIN_OBJECT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_METHOD.ID

		MAINT_GROUP = AD_MAIN_METHOD(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_CONVENTION.ID

		MAINT_GROUP = AD_MAIN_CONVENTION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_DEPRECIATION.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = AD_35ASSET_ONE::ASSET_NUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = AD_35ASSET::ASSET_NUM
			CASE ELSE
				MVALUE = AD_35ASSET_ONE::ASSET_NUM
			END SELECT
		END SELECT

		MAINT_GROUP = AD_MAIN_DEPRECIATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " recOrd"

		CASE OPT_MOREMENU
			AD_35ASSET_ONE = AD_35ASSET
			SELECT EDIT$(MVALUE, -1%)
			!
			! Asset Depreciation
			!
			CASE "RECORD"
				MAINT_GROUP = &
					MAIN_WINDOW(AD_MAIN_DEPRECIATIONSCAN.ID, "")
			END SELECT
		END SELECT

	CASE AD_MAIN_DEPRECIATIONSCAN.ID

		MAINT_GROUP = AD_MAIN_DEPRECIATIONSCAN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_DEPCLASS.ID

		MAINT_GROUP = AD_MAIN_DEPCLASS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
