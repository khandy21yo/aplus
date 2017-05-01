1	%TITLE "Initilize for Printer Control Sequences"
	%SBTTL "OUTP_INITIALIZE"
	%IDENT "V3.6a Calico"

	SUB OUTP_INITIALIZE(PRINT_TYPE$)
	!
	!	COPYRIGHT (C) 1986 BY
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
	!	.p
	!	This function reads in a printer definition file into
	!	the working arrays.
	!
	! Parameters:
	!
	!	PRINT_TYPE$
	!		The passed printer definition file the user enters.
	!
	!	Returned value
	!		Puts a printer definition file into the working
	!		arrays.
	!
	! Example:
	!
	!	CALL OUTP_INITIALIZE("PRINT.COM")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_INITIALIZE/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_INITIALIZE
	!	$ DELETE OUTP_INITIALIZE.OBJ;*
	!
	! AUTHOR:
	!
	!	07/22/86 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	08/24/87 - Kevin Handy
	!		Modified to pull information out of a library.
	!
	!	10/20/92 - Kevin Handy
	!		Modified to use PRINTX structure instead of
	!		map statement.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	10/13/97 - Kevin Handy
	!		Lose some unused variables.
	!		Enable several error messages.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/07/99 - Kevin Handy
	!		Map out SCOPE, so it doesn't think that it is a
	!		floating point variable.
	!
	!	08/24/2001 - Kevin Handy
	!		Modified error message so I can differentiate it
	!		from others that were exactly like.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"
	MAP (PRINTX) PRINTX_CDD PRINTX

	!
	! Declare variables
	!
	DECLARE RFA TXRFA

	%PAGE

100	PRINTX::ITEMS = 0%
	PRINTX::GROUPS = 0%
	EXIT SUB IF PRINT_TYPE$ = ""

	!
	! Set up the control structure if necessary
	!
	IF (LBR$INI_CONTROL(LR_INDEX%, LBR$C_READ) AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		EXIT SUB
	END IF

1000	!
	! Open work file
	!
	IF (LBR$OPEN(LR_INDEX%, "CMC:PRINT_TYPE", , "CMC:.TLB") AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to open library CMC:PRINT_TYPE", 0%)
		EXIT SUB
	END IF

1300	!
	! Search for key in file
	!
	IF (LBR$LOOKUP_KEY(LR_INDEX%, PRINT_TYPE$, TXRFA) AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to find PrintType " + &
			PRINT_TYPE$, 0%)
		GOTO CloseLibrary
	END IF

2000	!
	! Search for a line
	!
	TEXT$ = SPACE$(132%)
	IF (LBR$GET_RECORD(LR_INDEX%, TEXT$) AND 1%) <> 1%
	THEN
		GOTO CloseLibrary
	END IF

	!
	! Process line
	!
	INLINE$ = TRM$(TEXT$)

	GOTO 2000 IF (INLINE$ = "") OR (LEFT(INLINE$, 1%) = "!")

	!
	! Data item (true) or new group (false)
	!
	IF EDIT$(LEFT(INLINE$, 1%), 2%) = ""
	THEN
		!
		! Split up item definition
		!
		I% = INSTR(2%, INLINE$, '9'C)
		I% = LEN(INLINE$) + 1% IF I% = 0%

		PRINTX::ITEMS = PRINTX::ITEMS + 1%
		PRINTX::ITEM(PRINTX::ITEMS) = SEG$(INLINE$, 2%, I% - 1%)
		PRINTX::SEQU(PRINTX::ITEMS) = RIGHT(INLINE$, I% + 1%)
	ELSE
		!
		! Split up group definition
		!
		PRINTX::GROUPS = PRINTX::GROUPS + 1%
		I% = INSTR(1%, INLINE$, " ")
		J% = INSTR(I% + 1%, INLINE$, " ")

		!
		! If it starts with a "'", then it is hidden
		!
		IF (LEFT(INLINE$, 1%) = "'")
		THEN
			PRINTX::GROUPP(PRINTX::GROUPS) = &
				(PRINTX::ITEMS + 1%) OR 2048%
			PRINTX::GROUPX(PRINTX::GROUPS) = &
				SEG$(INLINE$, 2%, I% - 1%)
		ELSE
			PRINTX::GROUPP(PRINTX::GROUPS) = (PRINTX::ITEMS + 1%)
			PRINTX::GROUPX(PRINTX::GROUPS) = LEFT(INLINE$, I% - 1%)
		END IF

		PRINTX::DEFLT(PRINTX::GROUPS) = SEG$(INLINE$, I% + 1%, J% - 1%)
		PRINTX::DESCR(PRINTX::GROUPS) = RIGHT(INLINE$, J% + 1%)
	END IF

	GOTO 2000

3000	!
	! Now we are done
	!
 CloseLibrary:
	ST% = LBR$CLOSE(LR_INDEX%)
	PRINTX::GROUPP(PRINTX::GROUPS + 1%) = PRINTX::ITEMS + 1%

	END SUB
