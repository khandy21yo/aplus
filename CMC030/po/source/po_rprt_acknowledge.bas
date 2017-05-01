1	%TITLE "Acknowledgement Code List"
	%SBTTL "PO_RPRT_ACKNOWLEDGE"
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
	! ID:PO008
	!
	! Abstract:HELP
	!	.p
	!	The ^*Acknowledgement Code List\* option provides a report
	!	which contains the following:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Acknowledgement Code
	!	.le
	!	Description
	!	.els
	!
	! Index:
	!	.x Report>Acknowledgement
	!	.x Acknowledgement>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_ACKNOWLEDGE/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_ACKNOWLEDGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_ACKNOWLEDGE.OBJ;*
	!
	! Author:
	!
	!	04/06/90 - Kevin Handy
	!
	! Modification History:
	!
	!	06/19/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	09/7/91 - JEFF BEARD
	!		Cleaned up code
	!
	!	04/12/93 - Kevin Handy
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
	!	11/20/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_ACKNOWLEDGE.HB"
	MAP (PO_ACKNOWLEDGE) PO_ACKNOWLEDGE_CDD PO_ACKNOWLEDGE

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
	!
	!	^*(01) From Acknowledgement Code\*
	!	.p
	!	The ^*From Acknowledgement Code\* field causes the
	!	printing to begin with a
	!	selected code.
	!	.p
	!	A blank setting will cause the report to begin with the first
	!	Acknowledgement Code in the file.
	!
	! Index:
	!	.x From>Acknowledgement
	!	.x Acknowledgement>From
	! Datatype:TEXT
	! Size:2
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!
	!	^*(02) To Acknowledgement Code\*
	!	.p
	!	The ^*To Acknowledgement Code\* field causes the
	!	printing to end with the selected code.
	!	.p
	!	A blank setting will cause the report to end with the last
	!	code in the file.
	!
	! Index:
	!	.x To>Acknowledgement
	!	.x Acknowledgement>To
	! Datatype:TEXT
	! Size:2
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects designated
	!	Codes to be printed by entering a
	!	"wildcard" value.
	!
	! Index:
	!	.x Acknowledgement>Wildcard
	!	.x Wildcard>Acknowledgement
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ACKNOWLEDGE.OPN"
	USE
		FILENAME$ = "PO_ACKNOWLEDGE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Purchase  Order  Type  List"
	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "PO Type      Description"
	TITLE$(5%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$POType:002,$TypeDescr:053"

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #PO_ACKNOWLEDGE.CH%
		ELSE
			FIND #PO_ACKNOWLEDGE.CH%, &
				KEY #0% GE FROM_ITEM, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PO_ACKNOWLEDGE"
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
		GET #PO_ACKNOWLEDGE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PO_ACKNOWLEDGE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitProgram IF (PO_ACKNOWLEDGE::CODE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(PO_ACKNOWLEDGE::CODE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

	!
	! Print out one line
	!
	TEXT = PO_ACKNOWLEDGE::CODE + SPACE$(11%) + &
		PO_ACKNOWLEDGE::DESCR

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
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
	RESUME HelpError

32767	END
