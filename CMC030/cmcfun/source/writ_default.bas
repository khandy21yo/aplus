1	%TITLE "Save Current Defaults in Set File"
	%SBTTL "WRIT_DEFAULT"
	%IDENT "V3.6a Calico"

	SUB WRIT_DEFAULT(CDD_WINDOW_CDD SMG_WINDOW)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!	This function will save current defaults
	!	in the set file, overwriting any defaults that
	!	may already be there.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	SMG_WINDOW
	!		Passed definition of the current window
	!
	!	Returned value
	!		This subroutine returns the current defaults in the file.
	!
	! Example:
	!
	!	CALL WRIT_DEFAULT(SMG_WINDOW)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:WRIT_DEFAULT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP WRIT_DEFAULT
	!	$ DELETE WRIT_DEFAULT.OBJ;*
	!
	!
	! Author:
	!
	!	08/26/87 - Kevin Handy
	!
	! Modification history:
	!
	!	12/17/87 - Kevin Handy
	!		Modified so that function can create a set file
	!		if one does not yet exist.
	!
	!	08/12/88 - Kevin Handy
	!		Modified to remove LUN business and use ASSG_ instead.
	!
	!	01/22/91 - Frank F. Starman
	!		Add a new hard flag F.
	!
	!	03/11/91 - Frank F. Starman
	!		Add a new hard flag A.
	!
	!	01/31/92 - Frank F. Starman
	!		Replace delete records with update.
	!
	!	02/05/92 - Kevin Handy
	!		Clean out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Change field 'PROGRM' to 'PROGRAMNAME'
	!
	!	04/08/99 - Kevin Handy
	!		Added definition of SCOPE to avoid crashes
	!
	!	06/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD files
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	MAP (UTL_SET)	UTL_SET_CDD	UTL_SET

	COM (CH_UTL_SET) &
		UTL_SET.CH%, &
		UTL_SET.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAINT_GROUP

	%PAGE

	!*******************************************************************

	!
	! Figure out a system name
	!
	TEMP% = INSTR(1%, SMG_WINDOW::NHELP, "_")

	IF TEMP%
	THEN
		SYSTEM$ = LEFT(SMG_WINDOW::NHELP, TEMP% - 1%)
	ELSE
		SYSTEM$ = ""
	END IF

1000	!
	! Allocate a channel for save if possible
	!

1100	!
	! Open SET file for modification if possible
	!
	IF (UTL_SET.CH% = 0%) OR (UTL_SET.READONLY% <> 0%)
	THEN
		WHEN ERROR IN
			CLOSE #UTL_SET.CH% IF UTL_SET.CH%
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.CRE"
		USE
			UTL_SET.CH% = -UTL_SET.CH%
			CALL ENTR_3MESSAGE(SCOPE, &
				"Unable to open set file for save!", 0%)
			CONTINUE 1400
		END WHEN
		UTL_SET.READONLY% = 0%
	END IF

1300	!
	! Go through all items and dump into set file
	!
	FOR LOOP% = 1% TO SMG_WINDOW::NITEMS

		!
		! Get value
		!
		CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 193%, MVALUE$)

		!
		! Create/write a record if not blank
		!
		UTL_SET::PROGRAMNAME	= SMG_WINDOW::NHELP
		UTL_SET::ITEM		= NUM1$(LOOP%)

		WHEN ERROR IN
			GET #UTL_SET.CH%, &
				KEY #0% EQ UTL_SET::PROGRAMNAME + UTL_SET::ITEM
		USE
			IF ERR = 155% OR ERR = 131%
			THEN
				GET.FLAG% = 0%
				CONTINUE 1310
			END IF
			EXIT HANDLER
		END WHEN

		GET.FLAG% = -1%

1310		!
		! Update record
		!
		SELECT SMG_WINDOW::HFLAG(LOOP%)

		CASE 1%
			UTL_SET::HARD		= "H"
			UTL_SET::ALLOWUND	= "N"

		CASE 2%, 3%
			UTL_SET::HARD		= "F"
			UTL_SET::ALLOWUND	= "N"

		CASE 4%
			UTL_SET::HARD		= "S"
			UTL_SET::ALLOWUND	= "Y"

		CASE 5%
			UTL_SET::HARD		= "H"
			UTL_SET::ALLOWUND	= "Y"

		CASE 6%, 7%
			UTL_SET::HARD		= "F"
			UTL_SET::ALLOWUND	= "Y"

		CASE ELSE
			UTL_SET::HARD		= "S"
			UTL_SET::ALLOWUND	= "N"
		END SELECT

		UTL_SET::SDATA		= MVALUE$

		IF GET.FLAG%
		THEN
			UPDATE #UTL_SET.CH%
		ELSE
			!
			! Create record
			!
			UTL_SET::PROGRAMNAME	= SMG_WINDOW::NHELP
			UTL_SET::ITEM		= NUM1$(LOOP%)
			UTL_SET::SYSTEM		= SYSTEM$
			UTL_SET::FDATA		= ""

			PUT #UTL_SET.CH%
		END IF

1380	NEXT LOOP%

1400	END SUB
