1	%TITLE "Make TeX Screens for Ad Sheets"
	%SBTTL "TK_SPEC_SCREENTEX"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	!	This program is used to create screens for TeX
	!	documentataion that are overlayed on top of
	!	each other.
	!
	! Index:
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Enviornment:
	!
	!	VAX/VMS BASIC
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_SCREENTEX
	!	$ LINK/EXE=TK_EXE: TK_SPEC_SCREENTEX, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_SCREENTEX.OBJ;*
	!
	! Author:
	!
	!	06/28/88 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	!
	! Declare variables
	!
	MAP (LBR_JUNKJUNK) TEXT$ = 132%, TEXT1$ = 256%		! Read buffer
	DECLARE RFA TXRFA

	!
	! Declare variables
	!
	DECLARE INTEGER CONSTANT MAXLINE=66%

	DIM TEXARY$(MAXLINE)

	%PAGE

	!
	! Initialization
	!
	TEXARY$(I%) = SPACE$(512%) FOR I% = 1% TO MAXLINE

	!
	! Get and open output file
	!
	OUT.CH% = 15%

	LINPUT "Output file"; OUT_NAME$
	OPEN OUT_NAME$ FOR OUTPUT AS FILE OUT.CH%, &
		RECORDSIZE 250%

	!
	! Query user for all necessary information for one screen
	!
 GetName:
	LINPUT "Library name"; LIB_NAME$
	GOTO HandleOutput IF LIB_NAME$ = ""

	LINPUT "Key name    "; KEY_NAME$
	INPUT "Start line  "; START_LINE%
	INPUT "Start col   "; START_COL%
	INPUT "Max lines   "; MAX_LINE%

	FLAG_ONE% = -1%

	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LR.INDEX%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "Unable to initilize library"; ST%
		GOTO GetName
	END IF

	!
	! Open the library function
	!
	ST% = LBR$OPEN(LR.INDEX%, LIB_NAME$, , ".TLB")

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "Unable to open library"
		GOTO GetName
	END IF

	!
	! Point to text
	!
	ST% = LBR$LOOKUP_KEY(LR.INDEX%, KEY_NAME$, TXRFA)

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "Error in LIB find"; ST%
		GOTO GetName
	END IF

	!
	! Copy over text
	!
 Loop:
	TEXT$ = ""
	ST% = LBR$GET_RECORD(LR.INDEX%, TEXT$)

	IF (ST% AND 1%) = 1%
	THEN
		INLINE$ = TRM$(TEXT$)
		GOTO Loop IF (INLINE$ = "") AND (FLAG_ONE% <> 0%)

		!
		! Handle first line of report
		!
		IF FLAG_ONE%
		THEN
			INLINE_LENGTH% = LEN(INLINE$)

			TEXARY$(START_LINE%) = &
				LEFT(TEXARY$(START_LINE%), START_COL% - 1%) + &
				STRING$(INLINE_LENGTH% + 2%, 254%) + &
				RIGHT(TEXARY$(START_LINE%), &
					START_COL% + INLINE_LENGTH% + 2%)

			START_LINE% = START_LINE% + 1%

			FLAG_ONE% = 0%
		END IF

		INLINE$ = INLINE$ + SPACE$(INLINE_LENGTH% - LEN(INLINE$))

		!
		! Add text into array
		!
		TEXARY$(START_LINE%) = &
			LEFT(TEXARY$(START_LINE%), START_COL% - 1%) + &
			'255'C + INLINE$ + '255'C + &
			RIGHT(TEXARY$(START_LINE%), &
				START_COL% + INLINE_LENGTH% + 2%)

		START_LINE% = START_LINE% + 1%

		!
		! Get next line of text, unless we have reached
		! the maximum number of lines.
		!
		MAX_LINE% = MAX_LINE% - 1%

		GOTO Loop IF MAX_LINE% > 0%

	END IF

	!
	! Close library file
	!
 CloseLibrary:
	ST% = LBR$CLOSE(LR.INDEX%)

	IF FLAG_ONE% = 0%
	THEN
		TEXARY$(START_LINE%) = &
			LEFT(TEXARY$(START_LINE%), START_COL% - 1%) + &
			STRING$(LEN(INLINE$) + 1%, 254%) + &
			RIGHT(TEXARY$(START_LINE%), &
				START_COL% + LEN(INLINE$) + 2%)
	END IF

	GOTO GetName

 HandleOutput:

	FOR I% = 1% TO MAXLINE

		INLINE$ = TRM$(TEXARY$(I%))

		PRINT #OUT.CH%, "\hbox{\ \ \ \ \ ";

		WHILE INLINE$ <> ""

			INCHAR$ = LEFT(INLINE$, 1%)
			INLINE$ = RIGHT(INLINE$, 2%)

			IF CCPOS(OUT.CH%) > 100%
			THEN
				PRINT #OUT.CH%, "%"
			END IF

			SELECT INCHAR$

			!
			! Horozontal line
			!
			CASE '254'C
				A% = 1%
				A% = A% + 1% WHILE MID(INLINE$, A%, 1%) == '254'C
				PRINT #OUT.CH%, "\hxbar{"; NUM1$(A%); "}";
				INLINE$ = RIGHT(INLINE$, A%)

			!
			! Vertical line
			!
			CASE '255'C
				PRINT #OUT.CH%, "{\vxbar}";

			!
			! Space
			!
			CASE " "
				A% = 1%
				A% = A% + 1% WHILE MID(INLINE$, A%, 1%) == " "
				PRINT #OUT.CH%, "\hxspace{"; &
					NUM1$(A%); "}";
				INLINE$ = RIGHT(INLINE$, A%)

			CASE "$"
				PRINT #OUT.CH%, "{\$}";

			CASE "%"
				PRINT #OUT.CH%, "{\%}";

			CASE "#"
				PRINT #OUT.CH%, "{\#}";

			CASE "_"
				PRINT #OUT.CH%, "{\_}";

			CASE "{"
				PRINT #OUT.CH%, "{\{}";

			CASE "}"
				PRINT #OUT.CH%, "{\}}";

			CASE "\"
				PRINT #OUT.CH%, "{\verb'\'}";

			!
			! Normal character
			!
			CASE ELSE
				PRINT #OUT.CH%, INCHAR$;

			END SELECT

		NEXT

		PRINT #OUT.CH%, "}"

	NEXT I%

	GOTO 32767

32767	END
