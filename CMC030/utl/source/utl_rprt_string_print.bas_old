1	%TITLE "String User Report Settings"
	%SBTTL "UTL_RPRT_STRING_PRINT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:UT018
	!
	! Abstract:HELP
	!	.p
	!	The ^*List String Report Setups\* option in the Utility Report String menu
	!	provides the means to list the string report setup file. This option does
	!	not print the actual string reports.
	!
	! Index:
	!	.x List>String Report Setups
	!	.x Setup>String Reports>List
	!	.x String Reports>Setup>List
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_STRING_PRINT/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_STRING_PRINT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_STRING_PRINT.OBJ;*
	!
	! Author:
	!
	!	06/07/89 - Aaron Redd
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*STRING/LIST\*
	!	.p
	!	Allows to list the string report setup file.
	!	.p
	!	This option does not print the actual string reports.
	!	.p
	!	^*Format: STRING/LIST\*
	!
	! Index:
	!	.x STRING/LIST
	!	.x List>String Report Setups
	!	.x Setup>String Reports>List
	!	.x String Reports>Setup>List
	!
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*STRING/LIST\*
	!	.p
	!	displays or prints a list of the string report setup file.
	!	.note
	!	This option does not print the actual string reports.
	!	.end note
	!	^*Format: STRING/LIST\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /STRING/LIST
	!	.end literal
	!
	! Index:
	!	.x List>String Report Setups
	!	.x Setup>String Reports>List
	!	.x String Reports>Setup>List
	!--

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STRING_PRINT.HB"
	MAP	(UTL_STRING_PRINT)	UTL_STRING_PRINT_CDD	UTL_STRING_PRINT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE				UTL_REPORTX_CDD		UTL_REPORTX

	!
	! Declare variables and constants
	!
	DECLARE		STRING	TEXT, PRIM.KEY,FROM.ITEM,TO.ITEM
	DECLARE		LONG	LOOP
	DECLARE		WORD	CONSTANT PRINT.WIDTH = 132%

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initialize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, PRINT.WIDTH)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM.ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* field in the ^*String User Report Settings Screen\*
	!	determines the item with which the report will begin printing.
	!	.p
	!	If this field is blank, the list will begin with the first record
	!	in the file.
	!
	! Index:
	!	.x From Item
	!	.x Item>From
	!
	!--

	TO.ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field in the ^*String User Report Settings Screen\* determines
	!	the item with which the report will end printing.
	!	.p
	!	If this field is blank, the list will end with the last record
	!	in the file.
	!
	! Index:
	!	.X To Item
	!	.x Item>To
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_STRING_PRINT.OPN"
	USE
		FILENAME$ = "UTL_STRING_PRINT"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "STRING PRINT LIST"
	TITLE$(2%) = "Utility system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "System Group  RprtSeq# Titles               " + &
		"Rprt#  Output Device        Flags Codes  Description"
	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM.ITEM = ""
		THEN
			RESET #UTL_STRING_PRINT.CH%
		ELSE
			FIND #UTL_STRING_PRINT.CH%, KEY #0% GE FROM.ITEM, REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #UTL_STRING_PRINT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_STRING_PRINT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	PRIM.KEY = UTL_STRING_PRINT::SYSTEM + &
		UTL_STRING_PRINT::GROUPING + &
		UTL_STRING_PRINT::REPSEQ

	GOTO ExitTotal IF (PRIM.KEY > TO.ITEM) AND (TO.ITEM <> "")

17300	!
	! Print out the main line and first array items
	!
	TEXT = UTL_STRING_PRINT::SYSTEM + " " + &
		UTL_STRING_PRINT::GROUPING + " " + &
		UTL_STRING_PRINT::REPSEQ + "   " + &
		UTL_STRING_PRINT::TITLES + " " + &
		UTL_STRING_PRINT::REPNUM + " " + &
		UTL_STRING_PRINT::OUTDEV + " " + &
		UTL_STRING_PRINT::FLAGS(0%) + "     " + &
		UTL_STRING_PRINT::CODES(0%) + " " + &
		UTL_STRING_PRINT::DESCRS(0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print out the rest of the array items
	!
	FOR LOOP = 1% to 9%

		TEXT = SPACE$(72%) + &
			UTL_STRING_PRINT::FLAGS(LOOP) + "     " + &
			UTL_STRING_PRINT::CODES(LOOP) + " " + &
			UTL_STRING_PRINT::DESCRS(LOOP)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP

	!
	! Print a blank line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

 ExitTotal:
17400	!
	! Handle totals, etc. at the end of the report
	!

 ExitProgram:
	!
	! Finish up the program
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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%PAGE

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Trap previously untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	!***************************************************************
	! End of report UTL_RPRT_STRING_PRINT
	!***************************************************************
	END
