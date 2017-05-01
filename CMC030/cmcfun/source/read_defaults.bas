1	%TITLE "Load/Initilize Defaults for a Window"
	%SBTTL "READ_DEFAULTS"
	%IDENT "V3.6a Calico"

	SUB READ_DEFAULTS(CDD_WINDOW_CDD SMG_WINDOW)

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
	!
	! Abstract:HELP
	!	.b
	!	Maintainence
	!	.b
	!	Initilization
	!	.b
	!	.lm +5
	!	This subroutine is used to generate defaults for a
	!	maintainence window.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	This subroutine is used to generate defaults for a
	!	maintainence window.
	!
	! Example:
	!
	!	CALL READ_DEFAULTS(SMG_WINDOW)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_DEFAULTS/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP READ_DEFAULTS
	!	$ DELETE READ_DEFAULTS.OBJ;*
	!
	! Author:
	!
	!	02/23/87 - Kevin Handy
	!
	! Modification history:
	!
	!	08/12/88 - Kevin Handy
	!		Modified to open device file if it is not already
	!		open.  Added REGARDLESS to gets.
	!
	!	05/15/89 - Kevin Handy
	!		Fix so that if set file doesn't
	!		exist, it will still blank out fields.
	!
	!	01/22/91 - Frank F. Starman
	!		Associate a new value with HFLAG F.
	!
	!	01/31/92 - Frank F. Starman
	!		Call OPT_SETDEFAULT to set a field format
	!
	!	10/29/92 - Kevin Handy
	!		Modified to set UTL_SET.READONLY flag properly.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Change field 'PROGRAM' to 'PROGRAMNAME'
	!
	!	06/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	MAP (UTL_SET)		UTL_SET_CDD	UTL_SET

	COM (CH_UTL_SET) &
		UTL_SET.CH%, &
		UTL_SET.READONLY%

	!
	! External functions
	!
	EXTERNAL INTEGER FUNCTION MAINT_GROUP

	%PAGE

100	!
	! Initilize the defaults to null and soft
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETDEFAULT, 0%, 1%, "")

	FOR LOOP% = 1% TO SMG_WINDOW::NITEMS
		CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 97%, "")
		SMG_WINDOW::HFLAG(LOOP%) = 0% IF LOOP% <= 64%
	NEXT LOOP%

200	!
	! Open set file if not already open
	!
	IF (UTL_SET.CH% = 0%)
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.OPN"
		USE
			CALL ASSG_FREECHANNEL(UTL_SET.CH%)
			CONTINUE 600
		END WHEN
		UTL_SET.READONLY% = -1%
	END IF

500	!
	! Search for the first item
	!
	WHEN ERROR IN
		GET #UTL_SET.CH%, KEY #0% GE SMG_WINDOW::NHELP, REGARDLESS
	USE
		CONTINUE 600
	END WHEN

510	!
	! Loop through all items that match
	!
	WHILE UTL_SET::PROGRAMNAME = SMG_WINDOW::NHELP
		!
		! Get the item number to set
		!
		WHEN ERROR IN
			LOOP% = VAL%(UTL_SET::ITEM)
		USE
			CONTINUE 520
		END WHEN

		!
		! Add it if it is within range
		!
		IF LOOP% >= 1% AND LOOP% <= SMG_WINDOW::NITEMS
		THEN
			CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 97%, &
				TRM$(UTL_SET::SDATA))

			IF LOOP% < 64%
			THEN
				SELECT	UTL_SET::HARD
				CASE "H"
					SMG_WINDOW::HFLAG(LOOP%) = 1%
				CASE "F"
					SMG_WINDOW::HFLAG(LOOP%) = 3%
				CASE "S"
					SMG_WINDOW::HFLAG(LOOP%) = 0%
				END SELECT
				SMG_WINDOW::HFLAG(LOOP%) = &
					SMG_WINDOW::HFLAG(LOOP%) + 4% &
					IF UTL_SET::ALLOWUND = "Y"
			END IF
		END IF

		IF TRM$(UTL_SET::FDATA) <> ""
		THEN
			V% = MAINT_GROUP(SMG_WINDOW, OPT_SETDEFAULT, &
				LOOP%, 1%, &
				TRM$(UTL_SET::FDATA))
		END IF

520		!
		! Get the next record
		!
		WHEN ERROR IN
			GET #UTL_SET.CH%, REGARDLESS
		USE
			CONTINUE 600
		END WHEN
	NEXT

600	!
	! Store the Defaults values
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETDEFAULT, 0%, 0%, "")

32767	END SUB
