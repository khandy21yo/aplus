1	%TITLE "Dump Out Financial Statement Control Files"
	%SBTTL "GL_RPRT_FINR02"
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
	! ID:FINR02
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Financial Statement Command File Report\* prints the
	!	Command Files for the financial statements.
	!	.lm -5
	!
	! Index:
	!	.x Financial Command File>Print
	!	.x Print>Financial Command File
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_FINR02/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_FINR02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_FINR02.OBJ;*
	!
	! Author:
	!
	!	11/26/86 - Kevin Handy
	!
	! Modification history:
	!
	!	08/31/88 - Kevin Handy
	!		Several fixes for minor problems.
	!		- Not reading device file.
	!		- Title not showing file name
	!		- not printing file name as first line.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	08/10/2001 - Kevin Handy
	!		Dimension NAM$() to 100
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.HB"
	MAP	(GL_FINSTA)	GL_FINSTA_CDD	GL_FINSTA

	DIM NAM$(100%)

	%PAGE

	!******************************************************************
	! Take care of anything else before starting report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Assign channel number(s)
	!
	CALL ASSG_CHANNEL(FS.CH%, STAT%)

	%PAGE

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	%PAGE

 ReportTitle:
	TITLE$(1%) = "Financial Report File List"
	TITLE$(2%) = ""
	TITLE$(3%) = ""

	LYT_LINE$ = "$:080"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	!
	! How many files are there?
	!
	CALL READ_DEVICE("GL_FINCMD", GL_FINCMD.DEV$, ST%)
	CALL FIND_FILE(GL_FINCMD.DEV$ + "*.FS", NAM$(), 0%, "", "")

	COUNTER% = VAL%(NAM$(0%))

	IF COUNTER% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"*** No financial files to print ***", 0%)
		GOTO ExitProgram
	END IF

 GetNextRec:
17020	!******************************************************************
	! Main report loop starts here
	!******************************************************************
	FOR FILE_COUNTER% = 1% TO COUNTER%

		!
		! Check status
		!
		GOTO ExitProgram IF UTL_REPORTX::STAT

17300		!
		! Print out one file (name)
		!
		TITLE$(2%) = "File: " + NAM$(FILE_COUNTER%)

		TEXT$ = "*** File: " + NAM$(FILE_COUNTER%) + " ***"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 900%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Output text of file
		!
17310		WHEN ERROR IN
			OPEN NAM$(FILE_COUNTER%) FOR INPUT AS FILE FS.CH%
		USE
			CONTINUE 17900
		END WHEN

17320		WHEN ERROR IN
			LINPUT #FS.CH%, INLINE$
		USE
			CONTINUE 17900
		END WHEN

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), INLINE$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOTO 17320

		!
		! Close this file, and go for next one
		!
17900		CLOSE FS.CH%

	NEXT FILE_COUNTER%

	%PAGE

	!******************************************************************
	! Handle totals and other items before exiting
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!

 ExitProgram:
	!
	! Finish up report
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%PAGE

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	%PAGE

32767	!******************************************************************
	! End of report GL_RPRT_FINR02
	!******************************************************************
	END
