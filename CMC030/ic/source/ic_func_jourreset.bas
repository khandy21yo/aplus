1	%TITLE "Reset Adjustment Journal"
	%SBTTL "IC_FUNC_JOURRESET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_FUNC_JOURRESET
	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Resets the adjustment journal.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_FUNC_JOURRESET/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_FUNC_JOURRESET
	!	$ DELETE IC_FUNC_JOURRESET.OBJ;*
	!
	! Author:
	!
	!	07/31/88 - Frank Starman
	!
	! Modification history:
	!
	!	10/26/92 - Frank Starman
	!		Added IC_WRIT_35BALANCE function.
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/25/95 - Kevin Handy
	!		Lose call to SMG$DELETE_VIRTUAL_DISPLAY which killed
	!		a display that was never created.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/98 - Kevin Handy
	!		Lose file layout IC_JOURCOUNT, which wasn't used.
	!
	!	09/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP (IC_JOURADJUST)	IC_JOURADJUST_CDD	IC_JOURADJUST

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_IC_JOURADJUST) &
		IC_JOURADJUST.CH%, &
		IC_JOURADJUST.READONLY%

	COM (BATCH_IC_CYCLEJOUR) &
		BATCH_NO$	= 2%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION IC_WRIT_35BALANCE

	%PAGE

	ON ERROR GOTO 19000

	IC_FUNC_JOURRESET = 0%

300	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.OPN"
	USE
		CONTINUE ExitFunction IF ERR = 5%
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

	SCOPE::PRG_ITEM = "CONFIRM"

	TEMP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Adjustment file exist. Confirm reset - then press <DO>", &
		"N", 0%, "", "")

	CLOSE IC_JOURADJUST.CH%

	IF TEMP$ <> "Y"
	THEN
		GOTO ExitFunction
	END IF

310	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.UPD"

		RESET #IC_JOURADJUST.CH%
	USE
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

500	WHEN ERROR IN
		GET #IC_JOURADJUST.CH%
	USE
		CONTINUE 1000 IF ERR = 11% OR ERR = 131%
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

	V% = IC_WRIT_35BALANCE(IC_JOURADJUST::PRODUCT, &
		IC_JOURADJUST::LOCATION, &
		IC_CYCLEJOUR::TRANSTYPE, -IC_JOURADJUST::QUANTITY)

	GOTO 500

1000	CLOSE IC_JOURADJUST.CH%
	CALL READ_DEVICE("IC_JOURADJUST", IC_JOURADJUST.DEV$, STAT%)

 !	WHEN ERROR IN
 !		KILL IC_JOURADJUST.DEV$ + "IC_JOURADJUST_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE ExitFunction
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(IC_JOURADJUST.DEV$ + &
		"IC_JOURADJUST_" + BATCH_NO$ + ".JRL;*")

 ExitFunction:
	CALL ASSG_FREECHANNEL(IC_JOURADJUST.CH%)

	EXIT FUNCTION

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	IC_FUNC_JOURRESET = 1%
	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
