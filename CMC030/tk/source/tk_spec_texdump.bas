1	%TITLE "Library Dump"
	%SBTTL "TK_SPEC_TEXDUMP"
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
	! any other copies therof may not be provided or otherwise made
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
	!	This program is used to print keys and text in a
	!	document library
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	02/25/88 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_TEXDUMP/LINE
	!	$ LINK/EXE=TK_EXE:*.EXE TK_SPEC_TEXDUMP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_TEXDUMP.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/19/2000 - Kevin Handy
	!		Lose "ON ERROR GOTO 19000/ ON ERROR GOTO 0" loop
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION LIBR_NODIGSR

	!
	! Declare
	!
	DECLARE INTEGER CONSTANT NUM_LINES = 6000	! Size of the array
	DECLARE INTEGER CONSTANT LIB_INDEX = 2000	! Size of the array

	!
	! Dimension statements
	!
	DIM LINE_NUM$(NUM_LINES), LIB_INDEX$(LIB_INDEX), LIB_CONNECT%(LIB_INDEX)
	DIM RFA LIB_RFA(LIB_INDEX)

	%PAGE

	FONAME.CH% = 10%

 Init:	!
	! Initilize for output
	!
	LINPUT "Library name"; LFILE$
	LINPUT "Wildcard    "; WLDCRD$
	LINPUT "Output file "; FONAME$

300	!
	! Open output file
	!
	OPEN FONAME$ FOR OUTPUT AS FILE FONAME.CH%, &
		RECORDSIZE 255%, &
		DEFAULTNAME ".TEX", &
		ALLOW READ

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	!
	! Look up entries in library
	!
	WLDCRD$ = "*" IF WLDCRD$ = ""
	CALL LIBR_INDEX(LFILE$, WLDCRD$, LIB_INDEX$(), LIB_RFA())

	PRINT #FONAME.CH%, "\documentstyle{Book}"
	PRINT #FONAME.CH%, "\nofiles"
	PRINT #FONAME.CH%, "\textwidth 5in"
	PRINT #FONAME.CH%, "\evensidemargin 1in"
	PRINT #FONAME.CH%, "\begin{document}"

	!
	! Print library entries
	!
	FOR ENTRY% = 1% TO LIB_INDEX
		GOTO ExitProgram IF LIB_INDEX$(ENTRY%) = ""

		!
		! Get Next Entry if this is a connected key
		!
		GOTO NextText IF LIB_CONNECT%(ENTRY%)

		FOR LOOP% = ENTRY% + 1% TO LIB_INDEX
			IF LIB_CONNECT%(LOOP%) = 0% AND LIB_RFA(ENTRY%) = LIB_RFA(LOOP%)
			THEN
				LIB_CONNECT%(LOOP%) = -1%
			END IF
		NEXT LOOP%

		PRINT "% "; LIB_INDEX$(ENTRY%)

		ST% = LIBR_NODIGSR(LFILE$, LIB_INDEX$(ENTRY%), LINE_NUM$())

		IF (ST% AND 1%) = 0%
		THEN
			!
			! If text not found in main help file, check out
			! the default help file.
			!
			LINE_NUM$(0%) = "1"
			LINE_NUM$(1%) = "********* No Text for this key ***********"
		END IF

		CURR_LINE% = VAL%(LINE_NUM$(0%))

		MAX_LINE% = 0%
		MAX_LINE% = LEN(LINE_NUM$(I%)) IF LEN(LINE_NUM$(I%)) >MAX_LINE% &
			FOR I% = 1% TO CURR_LINE%

		DVWIDTH$ = "\tiny"
		DVCHAR = 27.45
		XWIDTH = (1.0 * MAX_LINE%) / DVCHAR

 !		PRINT #FONAME.CH%, "{\begin{figure}[htbp]"
		PRINT #FONAME.CH%, "\begin{center}\begin{tabular}{|c|}"
		PRINT #FONAME.CH%, "\hline\begin{minipage}[t]{"; &
			NUM1$(XWIDTH); "in}"
		PRINT #FONAME.CH%, DVWIDTH$; "\begin{verbatim}"

		FOR LOOP% = 1% TO CURR_LINE%

			PRINT #FONAME.CH%, LINE_NUM$(LOOP%)

		NEXT LOOP%

		PRINT #FONAME.CH%, ""
		PRINT #FONAME.CH%, "\end{verbatim}\end{minipage}\\"
		PRINT #FONAME.CH%, "\hline\end{tabular}\end{center}"
 !		PRINT #FONAME.CH%, "\end{figure}}"
		PRINT #FONAME.CH%, "\vfill"

 NextText:
	NEXT Entry%

 ExitProgram:

	PRINT #FONAME.CH%, "\end{document}"

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

32767	END
