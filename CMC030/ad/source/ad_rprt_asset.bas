1	%TITLE "Asset Description List"
	%SBTTL "AD_RPRT_ASSET"
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
	! ID:AD009
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	The ^*Asset Description\* option prints
	!	the Asset Description list. This list contains the following fields:
	!	.table 3,25
	!	.te
	!	Asset Number
	!	.te
	!	Description/Serial Number
	!	.te
	!	Location
	!	.te
	!	Department
	!	.te
	!	Type
	!	.te
	!	Service Date
	!	.te
	!	Cost
	!	.te
	!	Salvage
	!	.te
	!	Section 179
	!	.te
	!	Investment Tax Credit Amount
	!	.te
	!	Investment Tax Credit Reduce
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Asset Description>Report
	!	.x Report>Asset Description
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_ASSET/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_ASSET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_ASSET.OBJ;*
	!
	! Author:
	!
	!	12/10/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	06/26/88 - Frank F. Starman
	!		Option print by service date and location
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/18/2000 - Kevin Handy
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

	DECLARE STRING TEST_PRINTING

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort (A,D,L,S,T)\*
	!	.B
	!	.LM +5
	!	The ^*Sort\* field prints the report
	!	in a selected order by entering that selection in this field.
	!	.B
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*A\* = Asset Number
	!	.te
	!	^*D\* = Asset Description
	!	.te
	!	^*L\* = Asset Location
	!	.te
	!	^*S\* = Service Date
	!	.te
	!	^*T\* = Asset Type
	!	.end table
	!	An entry is required in this field.
	!	.LM -5
	!
	! Index:
	!	.x Sort>Asset Description List
	!	.x Asset Description List>Sort
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.B
	!	.LM +5
	!	The ^*From Item\* field causes the
	!	report to begin with the selected item.
	!	.B
	!	A blank setting will cause the report to begin with the first
	!	item in the file.
	!	.LM -5
	!
	! Index:
	!	.x From Item>Asset Description List
	!	.x Asset Description List>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.B
	!	.LM +5
	!	The ^*To Item\* field causes the report
	!	to end with the selected item.
	!	.B
	!	A blank setting will cause the report to print to the end of the
	!	file.
	!	.LM -5
	!
	! Index:
	!	.x To Item>Asset Description List
	!	.x Asset Description List>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.B
	!	.LM +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a "wildcard" value in this field.
	!	.LM -5
	!
	! Index:
	!	.x Wildcard>Asset Description  List
	!	.x Asset Description  List>Wildcard
	!
	!--

	SELECT SORT_BY$
	CASE "A"
		SORY_KEY% = 0%
		ADD_TITLE$ = "BY  ASSET  NUMBER"
	CASE "D"
		SORY_KEY% = 4%
		ADD_TITLE$ = "BY  DESCRIPTION"
	CASE "S"
		SORY_KEY% = 2%
		ADD_TITLE$ = "BY  SERVICE  DATE"
		FROM_ITEM$ = DATE_STOREDATE( &
			EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))
		TO_ITEM$ = DATE_STOREDATE( &
			EDIT$(UTL_REPORTX::OPTDEF(2%), 132%))
		WLDCRD$ = "*"
	CASE "L"
		SORY_KEY% = 3%
		ADD_TITLE$ = "BY  LOCATION"
	CASE "T"
		SORY_KEY% = 1%
		ADD_TITLE$ = "BY  ASSET  TYPE"
	END SELECT

300	!
	! Open asset file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
	USE
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ASSET  DESCRIPTION  LIST  " + ADD_TITLE$
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Asset#     Description/SerialNumber                 " + &
		"Loc  Dept   Tp ServDate        Cost" + &
		"     Salvage Section179     ITCAmt  ITCReduce"

	TITLE$(5%) = "."

	LYT_LINE$ = "$Asset#:011,$Description/SerialNumber:052,$Loc:057," + &
		"$Dept:064,$Tp:067,DServDate:083,VCost:092," + &
		"VSalvage:100,VSection179:115,VITCAmt:123," + &
		"VITCReduce:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AD_35ASSET.CH%, KEY #SORY_KEY%
		ELSE
			FIND #AD_35ASSET.CH%, &
				KEY #SORY_KEY% GE FROM_ITEM$, &
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
	SELECT SORT_BY$

	CASE "A"

		GOTO ExitTotal &
			IF (AD_35ASSET::ASSET_NUM > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_35ASSET::ASSET_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "D"

		GOTO ExitTotal &
			IF (AD_35ASSET::DESCRIPTION > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_35ASSET::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "L"
		GOTO ExitTotal IF (AD_35ASSET::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <>"" AND &
			COMP_STRING(EDIT$(AD_35ASSET::LOCATION, -1%), &
			WLDCRD$) = 0%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 0%) &
			IF TEST_PRINTING <> AD_35ASSET::LOCATION AND &
			TEST_PRINTING <> ""
		TEST_PRINTING = AD_35ASSET::LOCATION

	CASE "T"
		GOTO ExitTotal IF (AD_35ASSET::ASSET_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <>"" AND &
			COMP_STRING(EDIT$(AD_35ASSET::ASSET_TYPE, -1%), &
			WLDCRD$) = 0%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 0%) &
			IF TEST_PRINTING <> AD_35ASSET::ASSET_TYPE  AND &
			TEST_PRINTING <> ""
		TEST_PRINTING = AD_35ASSET::ASSET_TYPE

	CASE "S"

		GOTO ExitTotal &
			IF (AD_35ASSET::SERVDATE > TO_ITEM$) AND TO_ITEM$ <> ""

	END SELECT

17300	!
	! Print out one line
	!
	IF LEN(TRM$(AD_35ASSET::SERIAL_NUM) + TRM$(AD_35ASSET::DESCRIPTION)) < &
		LEN(AD_35ASSET::DESCRIPTION)
	THEN
		TEXT$ = AD_35ASSET::ASSET_NUM + " " + &
			LEFT(TRM$(AD_35ASSET::DESCRIPTION) + " " + &
				TRM$(AD_35ASSET::SERIAL_NUM) + &
				SPACE$(40%), &
				LEN(AD_35ASSET::DESCRIPTION)) + " " + &
			AD_35ASSET::LOCATION + " " + &
			AD_35ASSET::DEPT_NUM + " " + &
			AD_35ASSET::ASSET_TYPE + " " + &
			PRNT_DATE(AD_35ASSET::SERVDATE, 6%) + " " + &
			FORMAT$(AD_35ASSET::COST, "########.##") + " " + &
			FORMAT$(AD_35ASSET::SALVAGE, "########.##") + " " + &
			FORMAT$(AD_35ASSET::BONUS, "#######.##") + " " + &
			FORMAT$(AD_35ASSET::ITC, "#######.##") + " " + &
			FORMAT$(AD_35ASSET::ITCREDUCE, "#######.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 1%)
	ELSE
		TEXT$ = AD_35ASSET::ASSET_NUM + " " + &
			AD_35ASSET::DESCRIPTION + " " + &
			AD_35ASSET::LOCATION + " " + &
			AD_35ASSET::DEPT_NUM + " " + &
			AD_35ASSET::ASSET_TYPE + " " + &
			PRNT_DATE(AD_35ASSET::SERVDATE, 6%) + " " + &
			FORMAT$(AD_35ASSET::COST, "########.##") + " " + &
			FORMAT$(AD_35ASSET::SALVAGE, "########.##") + " " + &
			FORMAT$(AD_35ASSET::BONUS, "#######.##") + " " + &
			FORMAT$(AD_35ASSET::ITC, "#######.##") + " " + &
			FORMAT$(AD_35ASSET::ITCREDUCE, "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

		TEXT$ = SPACE$(LEN(AD_35ASSET::ASSET_NUM)) + "  " + &
			AD_35ASSET::SERIAL_NUM
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

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
