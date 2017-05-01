1	%TITLE "AST Routine for Unsolicited Input"
	%SBTTL "OUTP_XUNSOL"
	%IDENT "V3.6a Calico"

	SUB OUTP_XUNSOL BY REF(P1%, P2%, P3%, P4%, P5%, P6%)

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
	!	This is an AST routine that gets called through
	!	OUTP_LINE to handle unsolicited input. DO NOT
	!	CALL THIS AS A REGULAR SUB BECAUSE IT WILL
	!	NOT WORK PROPERLY.
	!	This function is called when the user types something
	!	while a report is printing.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	The input is five parameters passed through by the
	!	AST trapping routine.
	!
	!	Returns (1) to signify succuss, and lots of other values
	!	for failure.
	!
	! Example:
	!
	!	DO NOT CALL AS A NORMAL SUB.  This function is
	!	special to OUTP_LINE("", ).
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_XUNSOL/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_XUNSOL
	!	$ DELETE OUTP_XUNSOL.OBJ;*
	!
	! Author:
	!
	!	09/01/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Change TESTCHAR from a LONG to a WORD
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Maps
	!
	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	DECLARE WORD TESTCHAR

	%PAGE

6000	!
	! Get input until terminater
	!
	SMG_STATUS% = SMG$READ_KEYSTROKE &
	( &
		P2%,		! Keyboard ID &
		TESTCHAR,	! Return character &
		,		! Prompt &
	)

	!
	! Handle control/Z
	!
	IF SMG_STATUS% = 1213442%
	THEN
		SMG_STATUS% = 1%
		TESTCHAR = 26%
	END IF

	IF (SMG_STATUS% AND 1%) = 0%
	THEN
		SELECT SMG_STATUS%

		CASE 60%, 2104%
			GOTO 6000

		CASE ELSE
			PRINT "Input error (XUNSOL):"; SMG_STATUS%
			STOP
		END SELECT
	END IF

	GOTO 6000 IF (TESTCHAR = SMG$K_TRM_CANCELLED) OR &
		(TESTCHAR = SMG$K_TRM_BUFFER_FULL) OR &
		(TESTCHAR = SMG$K_TRM_UNKNOWN)

	CALL FUNC_SCOSEQ(TESTCHAR)

	!
	! Handle the Gold-Key redefinitions
	!
	IF (TESTCHAR = SMG$K_TRM_PF1)
	THEN
		SMG_STATUS% = SMG$READ_KEYSTROKE &
		( &
			P2%,		! Keyboard ID &
			TESTCHAR,	! Return character &
			,		! Prompt &
					! Time out &
		)

		SELECT CHR$(TESTCHAR)

		!
		! Interrupt (F6), Gold-I
		!
		CASE "I", "i"
			TESTCHAR = SMG$K_TRM_F6

		!
		! Resume (F7), Gold-R
		!
		CASE "R", "r"
			TESTCHAR = SMG$K_TRM_F7

		!
		! Cancel (F8), Gold-C
		!
		CASE "C", "c"
			TESTCHAR = SMG$K_TRM_F8

		!
		! Main screen, (F9), Gold-M
		!
		CASE "M", "m"
			TESTCHAR = SMG$K_TRM_F9

		!
		! eXit (F10), Gold-X
		!
		CASE "X", "x"
			TESTCHAR = SMG$K_TRM_F10

		!
		! List choices (F14), Gold-L
		!
		CASE "L", "l"
			TESTCHAR = SMG$K_TRM_F14

		!
		! Help (Help, F15), Gold-H
		!
		CASE "H", "h"
			TESTCHAR = SMG$K_TRM_HELP

		!
		! Do (Do, F16), Gold-D
		!
		CASE "D", "d"
			TESTCHAR = SMG$K_TRM_DO

		!
		! Magic key (F17), Gold-*
		!
		CASE "*"
			TESTCHAR = SMG$K_TRM_F17

		!
		! Top key (F18), Gold-T
		!
		CASE "T", "t"
			TESTCHAR = SMG$K_TRM_F18

		!
		! Bottom key (F19), Gold-B
		!
		CASE "B", "b"
			TESTCHAR = SMG$K_TRM_F19

		!
		! Find (Find), Gold-F
		!
		CASE "F", "f"
			TESTCHAR = SMG$K_TRM_FIND

		!
		! Insert here, Gold-+
		!
		CASE "+"
			TESTCHAR = SMG$K_TRM_INSERT_HERE

		!
		! Remove, Gold--
		!
		CASE "-"
			TESTCHAR = SMG$K_TRM_REMOVE

		!
		! Select, Gold-S
		!
		CASE "S", "s"
			TESTCHAR = SMG$K_TRM_SELECT

		!
		! Prev-screen, Gold-P
		!
		CASE "P", "p"
			TESTCHAR = SMG$K_TRM_PREV_SCREEN

		!
		! Next screen, Gold-N
		!
		CASE "N", "n"
			TESTCHAR = SMG$K_TRM_NEXT_SCREEN

		!
		! Dead key, Gold-<space>, Gold-D
		!
		CASE " ", "D", "d"
			GOTO 6000

		!
		! Otherwise bad key
		!
		CASE ELSE
			SCOPE::SCOPE_EXIT = -TESTCHAR
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 6000
		END SELECT
	END IF

	IF ((TESTCHAR >= 32%) AND (TESTCHAR <= 126%)) OR &
		((TESTCHAR >= 160%) AND (TESTCHAR <= 254%))
	THEN
		RRR_FLAG% = 0%
	ELSE
		RRR_FLAG% = TESTCHAR
	END IF

 ExitFunction:
7000	!

	END SUB
