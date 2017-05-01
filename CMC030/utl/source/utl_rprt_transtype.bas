1	%TITLE "Transaction Type Description List"
	%SBTTL "UTL_RPRT_TRANSTYPE"
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
	! ID:UT016
	!
	! Abstract:HELP
	!	.p
	!	The ^*Transaction Type\* option provides a report
	!	which contains the following:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Transaction Type Code
	!	.le
	!	Description
	!	.els
	!
	! Index:
	!	.x Report>Transaction Type
	!	.x Transaction Type>Report
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_TRANSTYPE/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_TRANSTYPE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_TRANSTYPE.OBJ;*
	!
	! AUTHOR:
	!
	!	05/03/88 - Frantisek Starman
	!
	! MODIFICATION HISTORY:
	!
	!	06/17/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE) UTL_TRANSTYPE_CDD UTL_TRANSTYPE

	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD
	DECLARE WORD CONSTANT PRINT.WIDTH = 132%

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, PRINT.WIDTH)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Transaction Type\*
	!	.p
	!	A ^*From Transaction Type\* value
	!	causes the printing to begin with the
	!	selected transaction type.
	!	.p
	!	A blank setting will cause the report to begin with
	!	the first transaction type in the file.
	!
	! Index:
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Transaction Type\*
	!	.p
	!	A ^*To Transaction Type\* value
	!	causes the printing to end with the
	!	selected transaction type.
	!	.p
	!	A blank setting causes the report to end with
	!	the last transaction type in the file.
	!
	! Index:
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects designated
	!	transaction types to be printed by entering a
	!	"wildcard" value.
	!
	! Index:
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.OPN"
	USE
		FILENAME$ = "UTL_TRANSTYPE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "TRANSACTION  DESCRIPTION  LIST"
	TITLE$(2%) = "Utility system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890
	TITLE$(4%) = "TType Description          Class"

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #UTL_TRANSTYPE.CH%
		ELSE
			FIND #UTL_TRANSTYPE.CH%, &
				KEY #0% GE FROM_ITEM, &
				REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
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
		GET #UTL_TRANSTYPE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_TRANSTYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (UTL_TRANSTYPE::CODE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(UTL_TRANSTYPE::CODE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	TEXT = UTL_TRANSTYPE::CODE + "    " + &
		UTL_TRANSTYPE::DESCRIPTION + " " + &
		UTL_TRANSTYPE::CLASS

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)

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
