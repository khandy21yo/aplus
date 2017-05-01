1	%TITLE "Purchase Order Notes List"
	%SBTTL "PO_RPRT_NOTES"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:PO009
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Notes List\* option will provide a report
	!	which contains the following:
	!	.table 3,25
	!	.te
	!	PO Note Code
	!	.te
	!	Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>PO Notes
	!	.x PO Notes>Report
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_NOTES/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_NOTES, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_NOTES.OBJ;*
	!
	! Author:
	!
	!	05/31/90 - Aaron Redd
	!
	! Modification History:
	!
	!	06/19/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	10/09/91 - JEFF BEARD
	!		CLEANED UP CODE
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_NOTES.HB"
	MAP (PO_NOTES) PO_NOTES_CDD PO_NOTES

	!
	! Declare variables and constants
	!
	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD, LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	printing to begin with the
	!	selected item.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	Note code in the file.
	!	.lm -5
	!
	! Index:
	!	.x From>Note Code
	!	.x Note Code>From
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	printing to end with the selected
	!	Note code.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	Note code in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Note Code
	!	.x Note Code>To
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	Note codes to be printed by entering a
	!	"wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Note Code>Wildcard
	!	.x Wildcard>Note Code
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_NOTES.OPN"
	USE
		FILENAME$ = "PO_NOTES"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Purchase  Order  Notes  List"
	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Code          Description"
	TITLE$(5%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$NoteCode:003,$CodeDescription:054"

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #PO_NOTES.CH%
		ELSE
			FIND #PO_NOTES.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PO_NOTES"
		CONTINUE HelpError
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
		GET #PO_NOTES.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PO_NOTES"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitProgram IF (PO_NOTES::NOTECODE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(PO_NOTES::NOTECODE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	TEXT = PO_NOTES::NOTECODE + SPACE$(12%) + PO_NOTES::DESCR

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitProgram:
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

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
