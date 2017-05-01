1	%TITLE "Asset Depreciation List"
	%SBTTL "AD_RPRT_DEPRECIAT"
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
	! ID:AD010
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Asset Depreciation\* option
	!	prints the Asset Depreciation List. This list contains the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Asset Number
	!	.le
	!	Description
	!	.le
	!	Object
	!	.le
	!	Class
	!	.le
	!	Description
	!	.le
	!	Property Type
	!	.le
	!	Method
	!	.le
	!	Recovery Period
	!	.le
	!	First Year Convention
	!	.le
	!	Disposal Year Convention
	!	.els
	!
	! Index:
	!	.x Asset Depreciation>Report
	!	.x Report>Asset Depreciation
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_DEPRECIAT/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_DEPRECIAT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_DEPRECIAT.OBJ;*
	!
	! AUTHOR:
	!
	!	12/10/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	09/19/88 - Frank F. Starman
	!		New lay out for AD_DEPRECIATION File
	!
	!	03/18/92 - Dan Perkins
	!		Changed FROM_ITEM$ to FROM_ITEM as declared.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 Calico standards.
	!
	!	05/12/95 - Kevin Handy
	!		Fix source code so no lines are longer then 128.
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/25/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.HB"
	MAP (AD_DEPRECIATION) AD_DEPRECIATION_CDD AD_DEPRECIATION

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET) AD_35ASSET_CDD AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS) AD_DEPCLASS_CDD AD_DEPCLASS

	!
	! External functions
	!
	DECLARE STRING TEXT, TEST_PRINTING, FROM_ITEM, TO_ITEM, WLDCRD, SORT_BY
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
	!	Entry in the ^*Sort (A,O)\* field prints the
	!	report in the selected order.
	!	.b
	!	Valid settings are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*A\* = Asset Number
	!	.le
	!	^*O\* = Object
	!	.le
	!	^*C\* = Classification
	!	.els
	!	.lm -5
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Asset Depreciation List
	!	.x Asset Depreciation List>Sort
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
	!	A blank setting will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Asset Depreciation List
	!	.x Asset Deprecaition List>From Item
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
	!	A blank setting will cause the report to print to the end of the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Asset Depreciation List
	!	.x Asset Depreciation List>To Item
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
	!	.x Wildcard>Asset Depreciation List
	!	.x AssetDepreciation List>Wildcard
	!
	!--

	SELECT SORT_BY
	CASE "A"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  ASSET NUMBER"

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  DEPRECIATION  CLASS"

	CASE "O"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  OBJECT"
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.OPN"
	USE
		FILENAME$ = "AD_DEPRECIATION.OPN"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AD_35ASSET.OPN"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AD_DEPCLASS.OPN"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ASSET  DEPRECIATION  LIST  " + ADD_TITLE$
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Asset#     Description                  " + &
		"            Obj  Class Description    " + &
		"                          PT Meth RecPer " + &
		"FYConv DYConv"

	TITLE$(5%) = "."

	lyt_line$ = "$Asset#:011,$Description:052,$Obj:057,$Class:063," + &
		"$Description:104,$PT:107,$Meth:112,$RecPer:119," + &
		"$FYConv:126,$DYConv:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #AD_DEPRECIATION.CH%, KEY #SORT_KEY%
		ELSE
			FIND #AD_DEPRECIATION.CH%, &
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
		GET #AD_DEPRECIATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY

	CASE "A"
		GOTO ExitTotal IF (AD_DEPRECIATION::ASSET_NUM > TO_ITEM) &
			AND TO_ITEM <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AD_DEPRECIATION::ASSET_NUM, -1%), &
			WLDCRD) = 0% AND WLDCRD <> ""

	CASE "O"
		GOTO ExitTotal IF (AD_DEPRECIATION::DEP_OBJECT > TO_ITEM) &
			AND TO_ITEM <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AD_DEPRECIATION::DEP_OBJECT, -1%), &
			WLDCRD) = 0% AND WLDCRD <> ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", -1%) &
			IF TEST_PRINTING <> AD_DEPRECIATION::DEP_OBJECT AND &
			TEST_PRINTING <> ""

		TEST_PRINTING = AD_DEPRECIATION::DEP_OBJECT

	CASE "C"
		GOTO ExitTotal IF (AD_DEPRECIATION::DEPCLASS > TO_ITEM) &
			AND TO_ITEM <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AD_DEPRECIATION::DEPCLASS, -1%), &
			WLDCRD) = 0% AND WLDCRD <> ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", -1%) &
			IF TEST_PRINTING <> AD_DEPRECIATION::DEPCLASS AND &
			TEST_PRINTING <> ""

		TEST_PRINTING = AD_DEPRECIATION::DEPCLASS

	END SELECT

17200	IF AD_DEPRECIATION::ASSET_NUM = TEST_ASSET$
	THEN
		AD_35ASSET::DESCRIPTION = &
			SPACE$(LEN(AD_35ASSET::DESCRIPTION))
	ELSE
		WHEN ERROR IN
			GET #AD_35ASSET.CH%, &
				KEY #0% EQ AD_DEPRECIATION::ASSET_NUM, &
				REGARDLESS
		USE
			AD_35ASSET::DESCRIPTION = &
				STRING$(LEN(AD_35ASSET::DESCRIPTION), A"?"B)

			CONTINUE 17210 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AD_35ASSET"
			CONTINUE HelpError
		END WHEN

	END IF

17210	AD_DEPCLASS::DESCRIPTION = &
		STRING$(LEN(AD_DEPCLASS::DESCRIPTION), A"?"B)
	AD_DEPCLASS::PROPTYPE = &
		STRING$(LEN(AD_DEPCLASS::PROPTYPE), A"?"B)
	AD_DEPCLASS::DEPMETHOD = &
		STRING$(LEN(AD_DEPCLASS::DEPMETHOD), A"?"B)
	AD_DEPCLASS::YEARS = &
		STRING$(LEN(AD_DEPCLASS::YEARS), A"?"B)
	AD_DEPCLASS::FYCONV = &
		STRING$(LEN(AD_DEPCLASS::FYCONV), A"?"B)
	AD_DEPCLASS::DYCONV = &
		STRING$(LEN(AD_DEPCLASS::DYCONV), A"?"B)

	WHEN ERROR IN
		GET #AD_DEPCLASS.CH%, &
			KEY #0% EQ AD_DEPRECIATION::DEPCLASS, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

17300	!
	! Print out one line
	!
	TEXT = AD_DEPRECIATION::ASSET_NUM + " " + &
		AD_35ASSET::DESCRIPTION + " " + &
		AD_DEPRECIATION::DEP_OBJECT + "    " + &
		AD_DEPRECIATION::DEPCLASS + "  " + &
		AD_DEPCLASS::DESCRIPTION + " " + &
		AD_DEPCLASS::PROPTYPE + " " + &
		AD_DEPCLASS::DEPMETHOD + " " + &
		AD_DEPCLASS::YEARS + "   " + &
		AD_DEPCLASS::FYCONV + "     " + &
		AD_DEPCLASS::DYCONV

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT, 0%)
	TEST_ASSET$ = AD_DEPRECIATION::ASSET_NUM

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
