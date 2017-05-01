1	%TITLE "Vendor Master"
	%SBTTL "AP_MAST_VENDOR"
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
	!	.B
	!	.LM +5
	!	The ^*Vendor Master\* file option maintains
	!	the following information relative to each vendor:
	!	.TABLE 3,25
	!	.TE
	!	Vendor Number
	!	.te
	!	Vendor Name
	!	.te
	!	Alpha Sort
	!	.te
	!	Remittance Address
	!	.te
	!	Purchase Order Address
	!	.te
	!	Remittance Phone Number
	!	.te
	!	Purchase Order Phone Number
	!	.te
	!	Federal ID Number
	!	.te
	!	1099 Requirement
	!	.te
	!	Flag to Purge
	!	.te
	!	Standard Terms
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.X Vendor>Maintenance
	!	.x Vendor>Master File
	!	.x Maintenance>Vendor
	!	.x Master File>Vendor
	!
	! Option:
	!	AP_MAIN_VENDOR$HELP
	!	AP_MAIN_CONTACT$HELP
	!
	! Index:
	!	.x Vendor Master File
	!
	! Option:
	!
	!
	! Author:
	!
	!	07/30/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAST_VENDOR/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_MAST_VENDOR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_MAST_VENDOR.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/06/96 - Kevin Handy
	!		Reformat source code
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

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

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

	V% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR
	EXTERNAL LONG FUNCTION AP_MAIN_CONTACT
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT
	CASE AP_MAIN_VENDOR.ID
		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_CONTACT.ID
		MAINT_GROUP = AP_MAIN_CONTACT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_COUNTRY.ID
		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)
	END SELECT

32767	END FUNCTION
