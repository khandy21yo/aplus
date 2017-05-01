1	%TITLE "Retired Assets List"
	%SBTTL "AD_RPRT_RETIRED"
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
	! ID:AD020
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Retired Asset\* option
	!	prints the Retired Asset List. This list contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Asset Number
	!	.le
	!	Description
	!	.le
	!	Retirement Date
	!	.le
	!	Proceeds Notes
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Retired Asset>Report
	!	.x Report>Retired Asset
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_RETIRED/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_RETIRED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_RETIRED.OBJ;*
	!
	! AUTHOR:
	!
	!	12/23/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/19/92 - Dan Perkins
	!		Resolved conflict between FROM_ITEM and FROM_ITEM$.
	!		Removed unused variable TEST_PRINTING.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/07/95 - Kevin Handy
	!		Added grand total
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	09/10/97 - Kevin Handy
	!		Change "x"+"y" to "xy"
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/23/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET) AD_35ASSET_CDD AD_35ASSET

	!
	! External functions
	!
	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD, SORT_BY
	DECLARE WORD CONSTANT PRINT_WIDTH = 132%

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, PRINT_WIDTH)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort (A,R)\* field prints the report in a
	!	selected order.
	!	.b
	!	Valid settings are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*A\* = Asset Number
	!	.le
	!	^*R\* = Retired
	!	.els
	!	.lm -5
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Retired Asset List
	!	.x Retired Asset List>Sort
	!
	!--

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	report to begin with the selected item.
	!	.b
	!	A blank setting causes the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Retired Assets List
	!	.x Retired Assets List>From Item
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report
	!	to end with the selected item.
	!	.b
	!	A blank setting causes the report to print to the end of the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Retired Assets List
	!	.x Retired Assets List>To Item
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a "wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Retired Asset List
	!	.x Retired Asset List>Wildcard
	!
	! Required:
	!--


	SELECT SORT_BY
	CASE "R"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  RETIRED  DATE"
		FROM_ITEM = DATE_STOREDATE( &
			EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	report to begin with the selected item.
	!	.b
	!	A blank setting causes the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Retired Assets List
	!	.x Retired Assets List>From Item
	!
	!--

		TO_ITEM = DATE_STOREDATE( &
			EDIT$(UTL_REPORTX::OPTDEF(2%), 132%))

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report
	!	to end with the selected item.
	!	.b
	!	A blank setting causes the report to print to the end of the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Retired Assets List
	!	.x Retired Assets List>To Item
	!
	!--

		WLDCRD = "*"
	CASE "A"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  ASSET  NUMBER"
	END SELECT

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
	USE
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "RETIRED  ASSET  LIST  " + ADD_TITLE$
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Asset#     Description                  " + &
		"            RetDate          Proceeds Notes"

	TITLE$(5%) = "."

	LYT_LINE$ = "$Asset#:011,$Description:052,DRetDate:069" + &
			",VProceeds:078,$Notes:084"
	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	TOTAL = 0.0

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #AD_35ASSET.CH%, KEY #SORT_KEY%
		ELSE
			FIND #AD_35ASSET.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM, &
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
		GET #AD_35ASSET.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY

	CASE "A"
		GOTO ExitTotal IF (AD_35ASSET::ASSET_NUM > TO_ITEM) &
			AND TO_ITEM <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AD_35ASSET::ASSET_NUM, -1%), &
			WLDCRD) = 0% &
			AND WLDCRD <> ""

	CASE "R"
		GOTO ExitTotal IF (AD_35ASSET::RET_DATE > TO_ITEM) &
			AND TO_ITEM <> ""
	END SELECT

17300	!
	! Print out one line
	!
	GOTO 17350 IF EDIT$(AD_35ASSET::RET_DATE, -1%) = ""

	TEXT = AD_35ASSET::ASSET_NUM + " " + &
		AD_35ASSET::DESCRIPTION + " " + &
		PRNT_DATE(AD_35ASSET::RET_DATE, 8%) + " " + &
		FORMAT$(AD_35ASSET::PROCEEDS, " ##,###,###.##") + " " + &
		AD_35ASSET::NOTES

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT, 0%)

	TOTAL = TOTAL + AD_35ASSET::PROCEEDS

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	TEXT = "       " + &
		"Grand Total                                  " + &
		"           " + &
		FORMAT$(TOTAL, " ##,###,###.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -2%)
	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT, -2%)

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
