1	%TITLE "Calculation Rate"
	%SBTTL "AD_FUNC_METHOD"
	%IDENT "V3.6a Calico"

	FUNCTION REAL AD_FUNC_METHOD(CALCUL$, RECOVERY$, DEP.YEAR, CONV)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center
	!	Idaho Falls, Idaho
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
	! Computer Management Center
	!
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function returns percentage rate.
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	CALCUL$ = Depreciation method type
	!	RECOVERY$ = RECOVERY of a asset
	!	DEP.YEAR = Depreciation year
	!	CONV = Convention coefficient
	!
	! Output:
	!
	!	AD_FUNC_METHOD = Percentage rate
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_FUNC_METHOD/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AD_FUNC_METHOD
	!	$ DELETE AD_FUNC_METHOD.OBJ;*
	!
	! Author:
	!
	!	12/19/87 - Frank Starman
	!
	! Modification history:
	!
	!	04/04/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6.
	!
	!	09/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/07/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Clean up source code
	!		Don't throw errors back to calling routine
	!
	!	01/23/2001 - Kevin Handy
	!		Redo calculation of type "02" to lose FOR
	!		loop, and a lot of floating point problems therin.
	!		Added a "FIX(xx + .0001)" to other loops for floating
	!		point rounding problems.
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	RATE = 0.0

	WHEN ERROR IN
		RECOVERY = VAL(RECOVERY$)
	USE
		RECOVERY = 0.0
	END WHEN

	GOTO ExitFunc IF RECOVERY = 0.0

	SELECT CALCUL$

	CASE "02"
		!
		! Straight line
		!
		IF DEP.YEAR > RECOVERY
		THEN
			RATE = 1.0
		ELSE

			FOR I = 1.0 TO FIX(DEP.YEAR - 1.0 + 0.0001)
				RATE = RATE + 1.0 / RECOVERY
			NEXT I

			RATE = RATE + CONV / RECOVERY

 ! MIN(0, FIX(DEP.YEAR - 1.0 + .0001)) / RECOVERY

		END IF

	CASE "03"
		!
		! 125% declining
		!
		RATE = CONV * 1.25 / RECOVERY

		FOR I = 2.0 TO FIX(DEP.YEAR + .0001)
			GOTO Case03 IF I > RECOVERY
			RATE = RATE + (1.0 - RATE) * 1.25 / RECOVERY
		NEXT I
 Case03:
		RATE = RATE + (1.0 - CONV) * (1.0 - RATE) * 1.25 / RECOVERY &
			IF DEP.YEAR > RECOVERY

	CASE "04"
		!
		! 150% declining
		!
		RATE = CONV * 1.5 / RECOVERY

		FOR I = 2.0 TO FIX(DEP.YEAR + .0001)
			GOTO Case04 IF I > RECOVERY
			RATE = RATE + (1.0 - RATE) * 1.5 / RECOVERY
		NEXT I
 Case04:
		RATE = RATE + (1.0 - CONV) * (1.0 - RATE) * 1.5 / RECOVERY &
			IF DEP.YEAR > RECOVERY

	CASE "05"
		!
		! 175% declining
		!
		RATE = CONV * 1.75 / RECOVERY

		FOR I = 2.0 TO FIX(DEP.YEAR + .00001)
			GOTO Case05 IF I > RECOVERY
			RATE = RATE + (1.0 - RATE) * 1.75 / RECOVERY
		NEXT I
 Case05:
		RATE = RATE + (1.0 - CONV) * (1.0 - RATE) * 1.75 / RECOVERY &
			IF DEP.YEAR > RECOVERY

	CASE "06"
		!
		! Double declining
		!
		RATE = CONV * 2.0 / RECOVERY

		FOR I = 2.0 TO FIX(DEP.YEAR + .00001)
			GOTO Case06 IF I > RECOVERY
			RATE = RATE + (1.0 - RATE) * 2.0 / RECOVERY
		NEXT I
 Case06:
		RATE = RATE + (1.0 - CONV) * (1.0 - RATE) * 2.0 / RECOVERY &
			IF DEP.YEAR > RECOVERY

	CASE "07"
		!
		! 125% declining to straight line
		!
		RATE = CONV * 1.25 / RECOVERY

		FOR I = 2.0 TO FIX(DEP.YEAR + .00001)
			GOTO Case07 IF I > RECOVERY
			DECL.RATE = (1.0 - RATE) * 1.25 / RECOVERY
			SL.RATE   = (1.0 - RATE) / (RECOVERY - I + 2.0 - CONV)
			IF DECL.RATE > SL.RATE
			THEN
				RATE = RATE + DECL.RATE
			ELSE
				RATE = RATE + SL.RATE
			END IF
		NEXT I
 Case07:
		RATE = 1.0 IF DEP.YEAR > RECOVERY

	CASE "08"
		!
		! 150% declining to straight line
		!
		RATE = CONV * 1.5 / RECOVERY

		FOR I = 2.0 TO FIX(DEP.YEAR + .00001)
			GOTO Case08 IF I > RECOVERY
			DECL.RATE = (1.0 - RATE) * 1.5 / RECOVERY
			SL.RATE   = (1.0 - RATE) / (RECOVERY - I + 2.0 - CONV)
			IF DECL.RATE > SL.RATE
			THEN
				RATE = RATE + DECL.RATE
			ELSE
				RATE = RATE + SL.RATE
			END IF
		NEXT I
 Case08:
		RATE = 1.0 IF DEP.YEAR > RECOVERY

	CASE "09"
		!
		! 175% declining to straight line
		!
		RATE = CONV * 1.75 / RECOVERY

		FOR I = 2.0 TO FIX(DEP.YEAR + .0001)
			GOTO Case09 IF I > RECOVERY
			DECL.RATE = (1.0 - RATE) * 1.75 / RECOVERY
			SL.RATE   = (1.0 - RATE) / (RECOVERY - I + 2.0 - CONV)
			IF DECL.RATE>SL.RATE
			THEN
				RATE = RATE + DECL.RATE
			ELSE
				RATE = RATE + SL.RATE
			END IF
		NEXT I
 Case09:
		RATE = 1.0 IF DEP.YEAR > RECOVERY

	CASE "10"
		!
		! Double decl to straight line
		!
		RATE = CONV * 2.0 / RECOVERY

		FOR I = 2.0 TO FIX(DEP.YEAR + .0001)
			GOTO Case10 IF I > RECOVERY
			DECL.RATE = (1.0 - RATE) * 2.0 / RECOVERY
			SL.RATE   = (1.0 - RATE) / (RECOVERY - I + 2.0 - CONV)
			IF DECL.RATE > SL.RATE
			THEN
				RATE = RATE + DECL.RATE
			ELSE
				RATE = RATE + SL.RATE
			END IF
		NEXT I
 Case10:
		RATE = 1.0 IF DEP.YEAR > RECOVERY

	CASE "11"
		!
		! Sum of the year's digits
		!
		DENOMINATOR = RECOVERY * (RECOVERY + 1.0) / 2.0

		FOR I = 1.0 TO DEP.YEAR - 1.0
			RATE = RATE + (RECOVERY - I + 1.0) / DENOMINATOR
		NEXT I

		RATE = RATE + CONV * (RECOVERY - DEP.YEAR + 1.0) / DENOMINATOR

		RATE = 1.0 IF DEP.YEAR > RECOVERY

	END SELECT

 ExitFunc:
	AD_FUNC_METHOD = RATE
	EXIT FUNCTION

	END FUNCTION

