1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "GL_CONV_NEWBALANCE"
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
	!	This program is used in the conversion from RSTS/E
	!	to VMS.
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_CONV_NEWBALANCE/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_CONV_NEWBALANCE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CONV_NEWBALANCE.OBJ;*
	!
	! Author:
	!
	!	02/17/93 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/14/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.HB"
	MAP (GL_BUD_YYYY)	GL_BUD_YYYY_CDD	GL_BUD_YYYY

	%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.HB"
	MAP (GL_GJ_LINE)	GL_GJ_LINE_CDD	GL_GJ_LINE

	%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.HB"
	MAP (GL_FINSTA)		GL_FINSTA_CDD	GL_FINSTA

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(GLSYS.CH%, STAT%)

	DIM PERIOD$(12%), MONTH$(12%)

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

	CALL READ_DEVICE("GLSYS_ASC", GLSYS_ASC.DEV$, STAT%)

250	WHEN ERROR IN
		OPEN GLSYS_ASC.DEV$ + "GLSYS.ASC" FOR INPUT AS FILE GLSYS.CH%, &
			RECORDSIZE 512%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "GLSYS.ASC file is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

550	WHEN ERROR IN
		INPUT LINE #GLSYS.CH%, INP$
	USE
		CONTINUE 560 IF STRG$ <> ""
		CONTINUE 600
	END WHEN

	IF INSTR(1%, INP$, CHR$(13%)) = 0%
	THEN
		STRG$ = STRG$ + INP$
		GOTO 550
	END IF

 !	CALL ENTR_3MESSAGE(SCOPE, LEFT(INP$, 10%), 1%)

	STRG$ = STRG$ + INP$

560	TEMP$ = "<STARTFILE>"
	IF INSTR(1%, STRG$, TEMP$)
	THEN
		ON_LOOP% = 5%
		ON_LOOP% = 1% IF INSTR(1% + LEN(TEMP$), STRG$, "CONTRL")
		ON_LOOP% = 2% IF INSTR(1% + LEN(TEMP$), STRG$, "CHART")
		ON_LOOP% = 3% IF INSTR(1% + LEN(TEMP$), STRG$, "FINSTA")
		ON_LOOP% = 4% IF INSTR(1% + LEN(TEMP$), STRG$, ".FS")
		ON_LOOP% = 6% IF INSTR(1% + LEN(TEMP$), STRG$, "BDGT")
		ON_LOOP% = 7% IF INSTR(1% + LEN(TEMP$), STRG$, "GJL")

		IF ON_LOOP% = 0%
		THEN
			PRINT STRG$
			STOP
		END IF

		ON ON_LOOP% GOSUB 1000, 2000, 3000, 4000, 5000, 6000, 7000

		STRG$ = ""
		GOTO 550
	END IF

	TEMP$ = "<ENDFILE>"
	IF INSTR(1%, STRG$, TEMP$)
	THEN

		ON ON_LOOP% GOSUB 1200, 2200, 3200, 4200, 5200, 6200, 7200

		STRG$ = ""
		GOTO 550
	END IF

	ON ON_LOOP% GOSUB 1100, 2100, 3100, 4100, 5100, 6100, 7100

	STRG$ = ""

	GOTO 550

600	GOTO ExitProgram

1000	!
	! GL_PERIOD file does not exist, so create it
	!
	RETURN

1100	!
	! Create a period record
	!
	RETURN

1200	RETURN

2000	!
	! CHART file does not exist, so create it
	!
	CALL ENTR_3MESSAGE(SCOPE, "Updating GL_CHART file", 1%)

2050	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.MOD"
	!======================================================================
	! GL_CHART file (create, open read/write)
	!======================================================================

2090	! Convert the chart of accounts
	RETURN

2100	GL_CHART::ACCT	= EDIT$(MID(STRG$, 2%, 8%), 8% + 128%)

	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ GL_CHART::ACCT
	USE
		PRINT "Bad account '"; GL_CHART::ACCT; "'"
		CONTINUE 2190
	END WHEN

	GL_CHART::CPERIOD= LASTPERCLO%

	GL_CHART::DOLLAR(I%) = 0.0 FOR I% = 0% TO 20%
	GL_CHART::UNIT(I%) = 0.0 FOR I% = 0% TO 20%
	GL_CHART::HOUR(I%) = 0.0 FOR I% = 0% TO 20%

	GL_CHART::DOLLAR(I% - 1%) = VAL(MID(STRG$, 81% + I% * 14%, 12%)) / 100.0 &
		FOR I% = 1% TO 15%

	UPDATE #GL_CHART.CH%

2190	RETURN

2200	CLOSE #GL_CHART.CH%

	CALL ASSG_FREECHANNEL(GL_CHART.CH%)

	RETURN

3000	!
	! GL_FINSTA file does not exist, so create it
	!
	RETURN

3100	RETURN

3200	RETURN

4000	RETURN

4100	RETURN

4200	RETURN

5000	!
	! GL LEDGER file does not exist, so create it
	!
	RETURN

5100	RETURN

5200	RETURN

6000	!
	! BUDGET file does not exist, so create it
	!
	RETURN

6100	RETURN

6200	RETURN

7000	!
	! JOURNAL file does not exist, so create it
	!
	RETURN

7100	RETURN

7200	RETURN

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	GOTO 32767

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	PRINT ERR, ERL
	RESUME HelpError

32767	END
