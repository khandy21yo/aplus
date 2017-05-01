1	%TITLE "Customer Type Description List"
	%SBTTL "AR_RPRT_CUSTYPE"
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
	! ID:AR028
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Customer Type list\* option will provide a report
	!	which contains the following:
	!	.table 3,25
	!	.te
	!	Customer Type Code
	!	.te
	!	Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Customer Type
	!	.x Customer Type>Report
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_CUSTYPE/LINE
	!	$ LINK/EXE=AR_EXE: AR_RPRT_CUSTYPE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_CUSTYPE.OBJ;*
	!
	! Author:
	!
	!	03/14/90 - J. Shad Rydalch
	!
	! Modification History:
	!
	!	06/21/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	03/32/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/22/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.HB"
	MAP (AR_CUSTYPE) AR_CUSTYPE_CDD AR_CUSTYPE

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
	!	^*(01) From Type\*
	!	.b
	!	.lm +5
	!	A ^*From Type\* field causes the
	!	printing to begin with a
	!	selected type.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	Customer Type code in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Type\*
	!	.b
	!	.lm +5
	!	A ^*To Type\* field causes the
	!	printing to end with a selected Customer
	!	Type
	!	.b
	!	A blank field will cause the report to end with the last
	!	Customer Type code in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	Customer Type codes to be printed by entering a
	!	"wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.OPN"
	USE
		FILENAME$ = "AR_CUSTYPE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CUSTOMER  TYPE  LIST"
	TITLE$(2%) = "Accounts Receivable System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "CustomerType Description"
	TITLE$(5%) = "."

	!
	! Line layout
	!
	LYT_LINE = "$CustomerType:002,$Description:080"

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #AR_CUSTYPE.CH%
		ELSE
			FIND #AR_CUSTYPE.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_CUSTYPE"
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
		GET #AR_CUSTYPE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_CUSTYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AR_CUSTYPE::CUSTYPE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AR_CUSTYPE::CUSTYPE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	TEXT = AR_CUSTYPE::CUSTYPE + SPACE$(11%) + &
		AR_CUSTYPE::DESCRIPTION

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

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
