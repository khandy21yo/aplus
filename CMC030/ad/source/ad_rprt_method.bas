1	%TITLE "Depreciation Method List"
	%SBTTL "AD_RPRT_METHOD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986 BY
	!
	! Computer Management Center
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
	! ID:AD014
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Depreciation Method\* report prints
	!	the Method Definition Table. This report contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Code
	!	.le
	!	Description
	!	.le
	!	Method
	!	.le
	!	Type
	!	.els
	!
	! Index:
	!	.X Depreciation Method>Report
	!	.x Report>Depreciation Method
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_METHOD/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_METHOD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_METHOD.OBJ;*
	!
	! AUTHOR:
	!
	!	12/06/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/27/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.HB"
	MAP (AD_METHOD) AD_METHOD_CDD AD_METHOD

	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD
	DECLARE WORD CONSTANT PRINT_WIDTH = 132%, CLASS = 12%

	DIMENSION STRING CALC(CLASS)

	%PAGE

	CALC(0%) = STRING$(40%, A"?"B)
	CALC(1%) = "01    Optional Depreciation Table"
	CALC(2%) = "02    Straight Line"
	CALC(3%) = "03    125% Declining Balance"
	CALC(4%) = "04    150% Declining Balance"
	CALC(5%) = "05    175% Declining Balance"
	CALC(6%) = "06    200% Declining Balance"
	CALC(7%) = "07    125% Declining to Straight Line"
	CALC(8%) = "08    150% Declining to Straight Line"
	CALC(9%) = "09    175% Declining to Straight Line"
	CALC(10%) = "10    200% Declining to Straight Line"
	CALC(11%) = "11    Sum of the Years-Digits"
	CALC(12%) = "12    Units-of-Production"


	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, PRINT_WIDTH)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Method\*
	!	.b
	!	.lm +5
	!	The ^*From Method\* field causes the
	!	printing to begin with the selected depreciation
	!	method.
	!	.b
	!	A blank setting will cause the report to start with the first
	!	depreciation method record in the file.
	!	.lm -5
	!
	! Index:
	!	.x Depreciation Method>From Method
	!	.x From Method>Deprecaition Method
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Method\*
	!	.b
	!	.lm +5
	!	The ^*To Method\* field causes the printing
	!	to end with the selected depreciation method.
	!	.b
	!	A blank setting will cause the report to end with the last depreciation
	!	method record in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Method>Depreciation Method
	!	.x Depreciation Method>To Method
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting prints a
	!	report including selected depreciation method codes only
	!	using the wildcarding technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Depreciation Method
	!	.x Depreciation Method>Wildcard
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.OPN"
	USE
		FILENAME$ = "AD_METHOD"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "METHOD  DESCRIPTION  LIST"
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Code Description                        " + &
		"      MethodType"

	TITLE$(5%) = "."

	LYT_LINE$ = "$Code:005,$Description:046,$MethodType:057"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #AD_METHOD.CH%
		ELSE
			FIND #AD_METHOD.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
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
		GET #AD_METHOD.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_METHOD"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AD_METHOD::DEP_METHOD > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AD_METHOD::DEP_METHOD, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	TEXT = AD_METHOD::DEP_METHOD  + " " + &
		AD_METHOD::DESCRIPTION + " " + &
		CALC(VAL%(AD_METHOD::CALCULATION))

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT, 0%)

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
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
