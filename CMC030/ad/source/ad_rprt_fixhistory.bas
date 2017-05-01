1	%TITLE "Fixed Asset History Report"
	%SBTTL "AD_RPRT_FIXHISTORY"
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
	! ID:AD003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Fixed Asset History Report\* prints a listing of the amount of
	!	depreciation calculated for each asset during every period. The report
	!	is printed in asset number order and contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Asset Number
	!	.le
	!	Description
	!	.le
	!	Cost
	!	.le
	!	Salvage
	!	.le
	!	Section 179
	!	.le
	!	Investment Tax Credit Amount
	!	.le
	!	Location
	!	.le
	!	Type
	!	.le
	!	Investment Tax Reduce
	!	.le
	!	Service Date
	!	.le
	!	Retirement Date
	!	.le
	!	Class
	!	.le
	!	Method
	!	.le
	!	Recovery Period
	!	.le
	!	Initial Units
	!	.le
	!	Adjusted Basis Status
	!	.le
	!	Total Depreciation Amount
	!	.le
	!	Total Depreciation Units
	!	.le
	!	Period
	!	.els
	!
	! Index:
	!	.x Asset Depreciation History>Report
	!	.x Report>Asset Depreciation History
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_FIXHISTORY/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_FIXHISTORY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_FIXHISTORY.OBJ;*
	!
	! AUTHOR:
	!
	!	10/04/88 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/19/92 - Dan Perkins
	!		Changed PRIOD$ to PERIOD$.  Added END SELECT to
	!		SELECT SORT_BY$.
	!
	!	03/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/26/92 - Kevin Handy
	!		Removed several syntax errors (good testing).
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change string$(...,ascii(" ")) to "" in several
	!		places.
	!
	!	01/29/96 - Kevin Handy
	!		Change string$(...,ascii(" ")) to SPACE$(...) in
	!		several places.
	!
	!	08/27/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/03/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET) AD_35ASSET_CDD AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.HB"
	MAP (AD_DEPRECIATION) AD_DEPRECIATION_CDD AD_DEPRECIATION

	%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.HB"
	MAP (AD_HISTORY) AD_HISTORY_CDD AD_HISTORY

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT) AD_OBJECT_CDD AD_OBJECT

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS) AD_DEPCLASS_CDD AD_DEPCLASS

	%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.HB"
	MAP (AD_ASSTYPE) AD_ASSTYPE_CDD AD_ASSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION) UTL_LOCATION_CDD UTL_LOCATION

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
	!	.b
	!	.lm +5
	!	The ^*Sort\* field causes the
	!	report to print in the selected order.
	!	.b
	!	Valid entries are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*A\* = Asset Number
	!	.le
	!	^*D\* = Asset Description
	!	.le
	!	^*L\* = Asset Location
	!	.le
	!	^*S\* = Service Date
	!	.le
	!	^*T\* = Asset Type
	!	.els
	!	.lm -5
	!	.b
	!	An entry is required in this field.
	!
	! Index:
	!	.x Sort>Fixed Asset History Report
	!	.x Fixed Asset History Report>Sort
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes
	!	the report to begin with the selected item.
	!	.b
	!	A blank field will cause the report to begin with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Fixed Asset History Report
	!	.x Fixed Asset History Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	report to end with the selected item.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Fixed Asset History Report
	!	.x Fixed Asset History Report>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting prints a report
	!	including only selected items using the wildcarding technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Fixed Asset History Report
	!	.x Fixed Asset History Report>Wildcard
	!
	!--

	OBJECT$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Object\*
	!	.b
	!	.lm +5
	!	The ^*Object\* field prints a report including only
	!	selected Objects by entering the desired selection.
	!	.lm -5
	!
	! Index:
	!	.x Object>Fixed Asset History Report
	!	.x Fixed Asset History Report>Object
	!
	!--

	ONLY_TOTAL$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) Only Totals (Y,N)\*
	!	.b
	!	.lm +5
	!	The ^*Only Totals\* field prints the entire report by
	!	entering ^*N\* or only report totals by entering a ^*Y\*.
	!	.lm -5
	!
	! Index:
	!	.x Only Totals>Fixed Asset History Report
	!	.x Fixed Asset History Report>Only Totals
	!
	!--

	FROM_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)

	!++
	! Abstract:FLD09
	!	^*(09) From Period\*
	!	.b
	!	.lm +5
	!	The ^*From Period\* field causes
	!	the report to begin with the selected period.
	!	.b
	!	A blank setting will cause the report to begin with the
	!	first period in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Period>Fixed Asset History Report
	!	.x Fixed Asset History Report>From Period
	!
	!--

	TO_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) To Period\*
	!	.b
	!	.lm +5
	!	The ^*To Period\* field causes
	!	the report to end with the selected period.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last period in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Period>Fixed Asset History Report
	!	.x Fixed Asset History Report>To Period
	!
	!--

	PERIOD$ = " FROM PERIOD " + FROM_PERIOD$ IF FROM_PERIOD$ <> ""
	PERIOD$ = PERIOD$ + " TO PERIOD " + TO_PERIOD$ IF TO_PERIOD$ <> ""

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes
	!	the report to begin with the selected item.
	!	.b
	!	A blank field will cause the report to begin with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Fixed Asset History Report
	!	.x Fixed Asset History Report>From Item
	!
	!--

	TO_ITEM$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(2%), 132%))

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	report to end with the selected item.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Fixed Asset History Report
	!	.x Fixed Asset History Report>To Item
	!
	!--

	SELECT SORT_BY$
	CASE "A"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  ASSET  NUMBER"

	CASE "D"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  DESCRIPTION"

	CASE "S"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  SERVICE  DATE"
		FROM_ITEM$ = DATE_STOREDATE( &
			EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))

	CASE "L"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY  LOCATION"
	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  ASSET  TYPE"
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
	USE
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.OPN"
	USE
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.OPN"
		GET #AD_OBJECT.CH%, KEY #0% EQ OBJECT$, REGARDLESS
	USE
		AD_OBJECT::DESCRIPTION = &
			STRING$(LEN(AD_OBJECT::DESCRIPTION), A"?"B)

		CONTINUE 350 IF ERR = 5% OR ERR = 155%
		FILENAME$ = "AD_OBJECT"
		CONTINUE HelpError
	END WHEN

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "AD_ASSTYPE"
		CONTINUE HelpError
	END WHEN

370	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "FIXED  ASSET  HISTORY  REPORT  " + ADD_TITLE$
	TITLE$(2%) = " FOR  OBJECT  " + OBJECT$ + " " + &
		TRM$(AD_OBJECT::DESCRIPTION) + PERIOD$
	TITLE$(3%) = "Asset Depreciation System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890
	IF ONLY_TOTAL$ <> "Y"
	THEN
		TITLE$(5%) = &
			"Asset#     Description                  " + &
			"            Loc  Tp    ServDate  " + &
			" Class Mthd  RecPeriod      AdjBasis       " + &
			"TotDepAmt Period"
		TITLE$(6%) = "                  Cost   Salvage   Sect179    ITCAmt " + &
			"ITCReduce RetDate                InitUnits" + &
			"                   TotDepUnits"

		TITLE$(7%) = "."
	ELSE
		TITLE$(5%) = "                  Cost   Salvage   Sect179    ITCAmt " + &
			"ITCReduce                                 " + &
			"      AdjBasis       TotDepAmt"

		TITLE$(6%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AD_35ASSET.CH%, KEY #SORT_KEY%
		ELSE
			FIND #AD_35ASSET.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AD_35ASSET"
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
		GET #AD_35ASSET.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

	HISTORY_AMOUNT_CTD = 0.0
	HISTORY_UNIT_CTD = 0.0
	HISTORY_LASTPER$ = &
		STRING$(LEN(AD_HISTORY::PERIOD), A"?"B)

17100	WHEN ERROR IN
		FIND #AD_HISTORY.CH%, &
			KEY #0% GE AD_35ASSET::ASSET_NUM + OBJECT$ + &
			FROM_PERIOD$, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

17105	WHEN ERROR IN
		GET #AD_HISTORY.CH%, REGARDLESS
	USE
		CONTINUE CheckRec IF ERR = 11%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

	GOTO CheckRec &
		IF AD_HISTORY::PERIOD > TO_PERIOD$ AND TO_PERIOD$ <> "" OR &
		AD_HISTORY::ASSET_NUM + AD_HISTORY::DEP_OBJECT <> &
		AD_35ASSET::ASSET_NUM + OBJECT$
	HISTORY_AMOUNT_CTD = HISTORY_AMOUNT_CTD + AD_HISTORY::AMOUNT_HIS
	HISTORY_UNIT_CTD = HISTORY_UNIT_CTD + AD_HISTORY::UNIT_HIS
	HISTORY_LASTPER$ = AD_HISTORY::PERIOD

	GOTO 17105

 CheckRec:
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
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_35ASSET::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "L"
		GOTO ExitTotal IF (AD_35ASSET::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <>"" AND &
			COMP_STRING(EDIT$(AD_35ASSET::LOCATION, -1%), &
			WLDCRD$) = 0%

		GOTO TestLocation IF TEST_STRING$ = AD_35ASSET::LOCATION

		GOSUB 18000 IF TEST_STRING$ <> ""

17130		WHEN ERROR IN
			GET #UTL_LOCATION.CH%, &
				KEY #0% EQ AD_35ASSET::LOCATION, &
				REGARDLESS
		USE
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B)

			CONTINUE 17135 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "UTL_LOCATION"
			CONTINUE HelpError
		END WHEN

17135		TEXT$ = ":::: " + &
			TRM$(AD_35ASSET::LOCATION) + " " + &
			TRM$(UTL_LOCATION::LOCNAME) + " "
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
 TestLocation:

		TEST_STRING$ = AD_35ASSET::LOCATION

	CASE "T"
		GOTO ExitTotal IF (AD_35ASSET::ASSET_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <>"" AND &
			COMP_STRING(EDIT$(AD_35ASSET::ASSET_TYPE, -1%), &
			WLDCRD$) = 0%

		GOTO TestType IF TEST_STRING$ = AD_35ASSET::ASSET_TYPE

		GOSUB 18000 IF TEST_STRING$ <> ""

17150		WHEN ERROR IN
			GET #AD_ASSTYPE.CH%, &
				KEY #0% EQ AD_35ASSET::ASSET_TYPE, &
				REGARDLESS
		USE
			AD_ASSTYPE::DESCRIPTION = &
				STRING$(LEN(AD_ASSTYPE::DESCRIPTION), A"?"B)

			CONTINUE 17155 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AD_ASSTYPE"
			CONTINUE HelpError
		END WHEN

17155		TEXT$ = ":::: " + &
			TRM$(AD_35ASSET::ASSET_TYPE) + " " + &
			TRM$(AD_ASSTYPE::DESCRIPTION) + " "
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A"?"B)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
 TestType:
		TEST_STRING$ = AD_35ASSET::ASSET_TYPE

	CASE "S"
		GOTO ExitTotal &
			IF (AD_35ASSET::SERVDATE > TO_ITEM$) AND TO_ITEM$ <> ""

	END SELECT

	TEXT$ = AD_35ASSET::ASSET_NUM + " " + &
		TRM$(AD_35ASSET::DESCRIPTION) + &
		STRING$(LEN(AD_35ASSET::DESCRIPTION) - &
			LEN(TRM$(AD_35ASSET::DESCRIPTION)), &
			A"."B) + " " + &
		AD_35ASSET::LOCATION + " " + &
		AD_35ASSET::ASSET_TYPE + "    " + &
		PRNT_DATE(AD_35ASSET::SERVDATE, 8%)

	TEXT2$ = SPACE$(LEN(AD_35ASSET::ASSET_NUM)) + " " + &
		FORMAT$(AD_35ASSET::COST, "########.##") + " " + &
		FORMAT$(AD_35ASSET::SALVAGE, "######.##") + " " + &
		FORMAT$(AD_35ASSET::BONUS, "######.##") + " " + &
		FORMAT$(AD_35ASSET::ITC, "######.##") + " " + &
		FORMAT$(AD_35ASSET::ITCREDUCE, "######.##")

	SUBTOTAL_COST = SUBTOTAL_COST + AD_35ASSET::COST
	SUBTOTAL_SALVAGE = SUBTOTAL_SALVAGE + AD_35ASSET::SALVAGE
	SUBTOTAL_BONUS = SUBTOTAL_BONUS + AD_35ASSET::BONUS
	SUBTOTAL_ITC = SUBTOTAL_ITC + AD_35ASSET::ITC
	SUBTOTAL_ITCREDUCE = SUBTOTAL_ITCREDUCE + AD_35ASSET::ITCREDUCE

	RET_DATE$ = SPACE$(LEN(AD_35ASSET::RET_DATE) + 2%)

	IF EDIT$(AD_35ASSET::RET_DATE, -1%) <> ""
	THEN
		RET_DATE$ = PRNT_DATE(AD_35ASSET::RET_DATE, 8%)
	END IF

17210	TEXT2$ = TEXT2$ + " " + &
		RET_DATE$ + &
		SPACE$(10%) + &
		FORMAT$(AD_35ASSET::UNITS, "<%>#,###,###.#")

	!
	! Read Depreciation file
	!
	AD_DEPRECIATION::DEPCLASS = ""
	AD_DEPCLASS::DEPMETHOD = ""
	AD_DEPCLASS::YEARS = ""
	ADJUST_BASIS = AD_35ASSET::COST

	WHEN ERROR IN
		GET #AD_DEPRECIATION.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM + OBJECT$, &
			REGARDLESS
	USE
		CONTINUE 17230 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

17220	!
	! Read depreciation class
	!
	WHEN ERROR IN
		GET #AD_DEPCLASS.CH%, &
			KEY #0% EQ AD_DEPRECIATION::DEPCLASS, &
			REGARDLESS
	USE
		AD_DEPCLASS::DEPMETHOD = &
			STRING$(LEN(AD_DEPCLASS::DEPMETHOD), A"?"B)
		AD_DEPCLASS::YEARS = &
			STRING$(LEN(AD_DEPCLASS::YEARS), A"?"B)

		CONTINUE 17230 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::SALVAGE &
		IF AD_DEPCLASS::SALVFACTOR = "Y"
	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::BONUS &
		IF AD_DEPCLASS::BONUSFACTOR = "Y"
	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::ITCREDUCE &
		IF AD_DEPCLASS::ITCFACTOR = "Y"

17230	!
	!
	TEXT$ = TEXT$ + " " + &
		AD_DEPRECIATION::DEPCLASS + "  " + &
		AD_DEPCLASS::DEPMETHOD + "       " + &
		AD_DEPCLASS::YEARS + " " + &
		FORMAT$(ADJUST_BASIS, "##,###,###.##")

	SUBTOTAL_ADJUST_BASIS = SUBTOTAL_ADJUST_BASIS + ADJUST_BASIS

	TEXT$ = TEXT$ + "   " + &
		FORMAT$(HISTORY_AMOUNT_CTD, "##,###,###.##") + " " + &
		HISTORY_LASTPER$

	TEXT2$ = TEXT2$ + SPACE$(17%) + &
		FORMAT$(HISTORY_UNIT_CTD, "<%>#,###,###.##")

	SUBTOTAL_AMOUNT_CTD = SUBTOTAL_AMOUNT_CTD + HISTORY_AMOUNT_CTD

17300	!
	! Print out
	!
	IF ONLY_TOTAL$<>"Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT2$, 0%)
	END IF
	PRINT_SUBTOTAL% = -1%

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	GOSUB 18000 IF PRINT_SUBTOTAL%

	IF PRINT_TOTAL%
	THEN
		!
		! Print final total
		!
		TEXT$ = STRING$(132%, A":"B)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 8%)
		TEXT$ = "A          " + &
			FORMAT$(A_COST, "########.##") + " " + &
			FORMAT$(A_SALVAGE, "######.##") + " " + &
			FORMAT$(A_BONUS, "######.##") + " " + &
			FORMAT$(A_ITC, "######.##") + " " + &
			FORMAT$(A_ITCREDUCE, "######.##") + &
			"_____Active" + &
			STRING$(23%, A"_"B) + &
			FORMAT$(A_ADJUST_BASIS, "##,###,###.##") + "   " + &
			FORMAT$(A_AMOUNT_CTD, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "N          " + &
			FORMAT$(N_COST, "########.##") + " " + &
			FORMAT$(N_SALVAGE, "######.##") + " " + &
			FORMAT$(N_BONUS, "######.##") + " " + &
			FORMAT$(N_ITC, "######.##") + " " + &
			FORMAT$(N_ITCREDUCE, "######.##") + &
			"_____Not Depreciable" + &
			STRING$(14%, A"_"B) + &
			FORMAT$(N_ADJUST_BASIS, "##,###,###.##") + "   " + &
			FORMAT$(N_AMOUNT_CTD, "##,###,###.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "F          " + &
			FORMAT$(F_COST, "########.##") + " " + &
			FORMAT$(F_SALVAGE, "######.##") + " " + &
			FORMAT$(F_BONUS, "######.##") + " " + &
			FORMAT$(F_ITC, "######.##") + " " + &
			FORMAT$(F_ITCREDUCE, "######.##") + &
			"_____Fully Depreciated" + &
			STRING$(12%, A"_"B) + &
			FORMAT$(F_ADJUST_BASIS, "##,###,###.##") + "   " + &
			FORMAT$(F_AMOUNT_CTD, "##,###,###.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "A + N + F  " + &
			FORMAT$(A_COST + F_COST + N_COST, "########.##") + " " + &
			FORMAT$(A_SALVAGE + F_SALVAGE + N_SALVAGE, &
				"######.##") + " " + &
			FORMAT$(A_BONUS + F_BONUS + N_BONUS, "######.##") + " " + &
			FORMAT$(A_ITC + F_ITC + N_ITC, "######.##") + " " + &
			FORMAT$(A_ITCREDUCE + F_ITCREDUCE + N_ITCREDUCE, &
				"######.##") + &
			SPACE$(34%) + &
			FORMAT$(A_ADJUST_BASIS + F_ADJUST_BASIS + N_ADJUST_BASIS, &
				"##,###,###.##") + "   " + &
			FORMAT$(A_AMOUNT_CTD + F_AMOUNT_CTD + N_AMOUNT_CTD, &
				"##,###,###.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "R          " + &
			FORMAT$(R_COST, "########.##") + " " + &
			FORMAT$(R_SALVAGE, "######.##") + " " + &
			FORMAT$(R_BONUS, "######.##") + " " + &
			FORMAT$(R_ITC, "######.##") + " " + &
			FORMAT$(R_ITCREDUCE, "######.##") + &
			"_____Retired" + &
			STRING$(22%, A"_"B) + &
			FORMAT$(R_ADJUST_BASIS, "##,###,###.##") + "   " + &
			FORMAT$(R_AMOUNT_CTD, "##,###,###.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Grand Total" + &
			FORMAT$(A_COST + F_COST + N_COST + R_COST, &
				"########.##") + " " + &
			FORMAT$(A_SALVAGE + F_SALVAGE + N_SALVAGE + R_SALVAGE, &
				"######.##") + " " + &
			FORMAT$(A_BONUS + F_BONUS + N_BONUS + R_BONUS, &
				"######.##") + " " + &
			FORMAT$(A_ITC + F_ITC + N_ITC + R_ITC, "######.##") + " " + &
			FORMAT$(A_ITCREDUCE + F_ITCREDUCE + N_ITCREDUCE + &
				R_ITCREDUCE, &
				"######.##") + " " + &
			STRING$(33%, A":"B) + &
			FORMAT$(A_ADJUST_BASIS + F_ADJUST_BASIS + N_ADJUST_BASIS + &
				R_ADJUST_BASIS, "##,###,###.##") + "   " + &
			FORMAT$(A_AMOUNT_CTD + F_AMOUNT_CTD + N_AMOUNT_CTD + &
				R_AMOUNT_CTD, "##,###,###.##") + " "
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

18000	!
	! Print subtotal
	!
	TEXT$ = "Total      " + &
		FORMAT$(SUBTOTAL_COST, "########.##") + " " + &
		FORMAT$(SUBTOTAL_SALVAGE, "######.##") + " " + &
		FORMAT$(SUBTOTAL_BONUS, "######.##") + " " + &
		FORMAT$(SUBTOTAL_ITC, "######.##") + " " + &
		FORMAT$(SUBTOTAL_ITCREDUCE, "######.##") + " " + &
		STRING$(33%, A":"B) + &
		FORMAT$(SUBTOTAL_ADJUST_BASIS, "##,###,###.##") + "   " + &
		FORMAT$(SUBTOTAL_AMOUNT_CTD, "##,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	PRINT_SUBTOTAL% = 0%

	SUBTOTAL_COST = 0.0
	SUBTOTAL_SALVAGE = 0.0
	SUBTOTAL_BONUS = 0.0
	SUBTOTAL_ITC = 0.0
	SUBTOTAL_ITCREDUCE = 0.0
	SUBTOTAL_ADJUST_BASIS = 0.0
	SUBTOTAL_AMOUNT_CTD = 0.0

	RETURN

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
