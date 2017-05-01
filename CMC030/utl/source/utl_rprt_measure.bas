1	%TITLE "Units of Measure Description List"
	%SBTTL "UTL_RPRT_MEASURE"
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
	! ID:UT014
	!
	! Abstract:HELP
	!	.p
	!	The ^*Unit of Measure List\* provides a report
	!	which contains the following:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	UOM Code
	!	.le
	!	Description
	!	.els
	!
	! Index:
	!	.x Report>Units of Measure
	!	.x Units of Measure>Report
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_MEASURE/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_MEASURE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_MEASURE.OBJ;*
	!
	! AUTHOR:
	!
	!	10/31/87 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
	!
	!	06/16/93 - Kevin Handy
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
	!	07/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	!++
	! Abstract:COMMAND
	!	^*UOM/LIST\*
	!	.p
	!	Allows to print the units of measure codes.
	!	.p
	!	^*Format: UOM/LIST\*
	! Index:
	!	.x UOM/LIST
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MEASURE.HB"
	MAP (UTL_MEASURE) UTL_MEASURE_CDD UTL_MEASURE

	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD, PRINT_FORM
	DECLARE BYTE J
	DECLARE LONG LOOP, TEST_LOOP, I
	DECLARE WORD CONSTANT PRINT_WIDTH = 132%, INIT_DIM = 1000%

	DIMENSION STRING CODE(INIT_DIM), DESCR(INIT_DIM)
	DIMENSION LONG COLUMN(5%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, PRINT_WIDTH)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Code\*
	!	.p
	!	A ^*From Code\* field causes the
	!	printing of the Units of Measure report to begin with a
	!	selected item.
	!	.p
	!	A blank setting will cause the report to begin with the first
	!	UOM code in the file.
	!
	! Index:
	!	.x From Code>Units of Measure
	!	.x Units of Measure>From Code
	!	.x From Code>UOM
	!	.x UOM>From Code
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Code\*
	!	.p
	!	A ^*To Code\* field causes the
	!	printing to end with the selected Units of
	!	Measure code.
	!	.p
	!	A blank setting will cause the report to end with the last
	!	UOM code in the file.
	!
	! Index:
	!	.x To Code>Units of Measure
	!	.x Units of Measure>To Code
	!	.x To Code>UOM
	!	.x UOM>To Code
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects designated
	!	Units of Measure codes to be printed by entering a
	!	"wildcard" value in this field.
	!
	! Index:
	!	.x Wildcard>Units of Measure
	!	.x Units of Measure>Wildcard
	!	.x Wildcard>UOM
	!	.x UOM>Wildcard
	!
	!--

	PRINT_FORM = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Format (H,V)\*
	!	.p
	!	The ^*Format (H,V)\* field determines the
	!	format in which the Units of Measure Report will be printed.
	!	.p
	!	This field must have a value. Valid options are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	H = Horizontal
	!	.le
	!	V = Vertical
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Format>Units of Measure
	!	.x Units of Measure>Format
	!	.x Format>UOM
	!	.x UOM>Format
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_MEASURE.OPN"
	USE
		FILENAME$ = "UTL_MEASURE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "MEASUREMENT  DESCRIPTION  LIST"
	TITLE$(2%) = "Utility system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890
	SELECT PRINT_FORM

	CASE "V"
		TITLE$(4%) = "Code Description"

	CASE "H"
		TITLE$(4%) = "Code Description          " + &
			"Code Description          " + &
			"Code Description          " + &
			"Code Description          " + &
			"Code Description         "

	END SELECT

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #UTL_MEASURE.CH%
		ELSE
			FIND #UTL_MEASURE.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
		END IF
	USE
		FILENAME$ = "UTL_MEASURE"
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
		GET #UTL_MEASURE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_MEASURE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (UTL_MEASURE::CODE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(UTL_MEASURE::CODE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	SELECT PRINT_FORM

	CASE "V"
		TEXT = UTL_MEASURE::CODE + "   " + &
			UTL_MEASURE::DESCRIPTION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)

	CASE "H"
		LOOP = LOOP + 1%
		CODE(LOOP) = UTL_MEASURE::CODE
		DESCR(LOOP) = UTL_MEASURE::DESCRIPTION

	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	IF PRINT_FORM = "H" AND LOOP > 0%
	THEN
		TEST_LOOP = LOOP
		FOR I = 2% TO 5%
			COLUMN(I) = COLUMN(I - 1%) + &
				INT(TEST_LOOP / (7.0 - I) + 0.9)
			TEST_LOOP = LOOP - COLUMN(I)
		NEXT I

		FOR I = 1% TO COLUMN(2%) - 1% STEP 1%
			TEXT = ""
			FOR J = 1% TO 5%
				TEXT = TEXT + CODE(I+COLUMN(J)) + "   " + &
					DESCR(I + COLUMN(J)) + " "
			NEXT J
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)
		NEXT I

		TEXT = ""
		FOR J = 1% TO LOOP - 5% * (COLUMN(2%) - 1%)
			TEXT = TEXT + CODE(COLUMN(2%) + COLUMN(J)) + "   " + &
				DESCR(COLUMN(2%) + COLUMN(J)) + " "
		NEXT J
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)

	END IF

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
