1	%TITLE "Depreciation Summary Ledger Report"
	%SBTTL "AD_RPRT_LEDSUMMARY"
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
	! ID:AD030
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Depreciation Summary Ledger Report\* prints a
	!	report containing the year to date depreciation information including the
	!	current period depreciation calculation. The report will be printed in order
	!	by asset number and contain the following fields:
	!	.b
	!	.lm 15
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
	!	Investment Tax Credit
	!	.le
	!	Location
	!	.le
	!	Investment Tax Credit Reduce
	!	.le
	!	Type
	!	.le
	!	Service Date
	!	.le
	!	Retirement Date
	!	.le
	!	Initial Units
	!	.le
	!	Adjusted Basis Period
	!	.le
	!	Depreciation Amount
	!	.le
	!	Depreciation Units
	!	.le
	!	Period Depreciation Amount
	!	.le
	!	Period Depreciation Units
	!	.le
	!	Total Depreciation Amount
	!	.le
	!	Total Depreciation Units
	!	.le
	!	Status
	!	.els
	!
	! Index:
	!	.x Summary Ledger>Report
	!	.x Report>Summary Ledger
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_LEDSUMMARY/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_LEDSUMMARY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_LEDSUMMARY.OBJ;*
	!
	! AUTHOR:
	!
	!	09/28/88 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	10/08/91 - Deborah K. Fries
	!		Cleaned source code so report would print
	!		Improved the error trapping
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in several
	!		places.
	!
	!	01/29/96 - Kevin Handy
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	08/27/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer.
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET) AD_35ASSET_CDD AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.HB"
	MAP (AD_DEPRECIATION) AD_DEPRECIATION_CDD AD_DEPRECIATION

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT) AD_OBJECT_CDD AD_OBJECT

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS) AD_DEPCLASS_CDD AD_DEPCLASS

	%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.HB"
	MAP (AD_ASSTYPE) AD_ASSTYPE_CDD AD_ASSTYPE

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP (AD_CONTROLOBJ) AD_CONTROLOBJ_CDD AD_CONTROLOBJ

	%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.HB"
	MAP (AD_CALCULATION) AD_CALCULATION_CDD AD_CALCULATION

	%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.HB"
	MAP (AD_BALANCE) AD_BALANCE_CDD AD_BALANCE

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
	!	The ^*Sort\* field prints the report
	!	in a selected order.
	!	.b
	!	Valid settings are:
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
	!	An entry is required in this field.
	!
	! Index:
	!	.x Sort>Depreciation Summary Ledger Report
	!	.x Depreciation Summary Ledger Report>Sort
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes
	!	the report to begin with the selected item.  The value must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank setting will cause the report to begin with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Summary Ledger Report
	!	.x Summary Ledger Report>From Item
	!	.x From>Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	report to end with the selected item.  The value must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Summary Ledger Report
	!	.x Summary Ledger Report>To Item
	!	.x To>Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting prints a report
	!	including selected items using the Wildcarding technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Summary Ledger Report
	!	.x Summary Ledger Report>Wildcard
	!
	!--

	OBJECT$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Object\*
	!	.b
	!	.lm +5
	!	The ^*Object\* field prints a report which
	!	will include only the selected object.
	!	.lm -5
	!
	! Index:
	!	.x Object>Summay Ledger Report
	!	.x Summary Ledger Report>Object
	!
	!--

	ONLY_TOTAL$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) Only Totals (Y,N)\*
	!	.b
	!	.lm +5
	!	The ^*Only Totals\* field prints the entire report by
	!	entering ^*N\* or only report totals by entering ^*Y\*.
	!	.lm -5
	!
	! Index:
	!	.x Only Totals>Summary Ledger Report
	!	.x Summary Ledger Report>Only Totals
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

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes
	!	the report to begin with the selected item.  The value must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank setting will cause the report to begin with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Summary Ledger Report
	!	.x Summary Ledger Report>From Item
	!	.x From>Item
	!	.x Item>From
	!
	!--

		TO_ITEM$ = DATE_STOREDATE( &
			EDIT$(UTL_REPORTX::OPTDEF(2%), 132%))

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	report to end with the selected item.  The value must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Summary Ledger Report
	!	.x Summary Ledger Report>To Item
	!	.x To>Item
	!	.x Item>To
	!
	!--

		WLDCRD$ = "*"
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
		%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "AD_CALCULATION"
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
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

380	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.OPN"
		GET #AD_CONTROLOBJ.CH%, KEY #0% EQ OBJECT$, REGARDLESS
	USE
		AD_CONTROLOBJ::ERA = &
			STRING$(LEN(AD_CONTROLOBJ::ERA), A"?"B)
		AD_CONTROLOBJ::LASTDEP = &
			STRING$(LEN(AD_CONTROLOBJ::LASTDEP), A"?"B)
		AD_CONTROLOBJ::LASTPER = &
			STRING$(LEN(AD_CONTROLOBJ::LASTPER), A"?"B)

		CONTINUE ReadPeriod IF ERR = 5% OR ERR = 155%
		FILENAME$ = "AD_CONTROLOBJ"
		CONTINUE HelpError
	END WHEN

 ReadPeriod:
	V% = READ_PERIOD("READ", AD_CONTROLOBJ::ERA, AD_CONTROLOBJ::LASTDEP, &
		PERIOD_DESC$, "", "", "", 0%)

	IF AD_CONTROLOBJ::LASTDEP <= AD_CONTROLOBJ::LASTPER OR &
		AD_CONTROLOBJ::LASTPER = ""
	THEN
		PERIOD$ = "  TO  PERIOD  " + AD_CONTROLOBJ::LASTDEP + " " + &
			TRM$(PERIOD_DESC$)
	ELSE
		PERIOD$ = "  FROM  PERIOD  " + AD_CONTROLOBJ::LASTPER + &
			"  TO  PERIOD  " + AD_CONTROLOBJ::LASTDEP + " " + &
			TRM$(PERIOD_DESC$)
	END IF

390	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "DEPRECIATION  SUMMARY  PERIOD  LEDGER  " + ADD_TITLE$
	TITLE$(2%) = "  FOR  OBJECT  " + OBJECT$ + " " + &
		TRM$(AD_OBJECT::DESCRIPTION) + PERIOD$
	TITLE$(3%) = "Asset Depreciation System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	IF ONLY_TOTAL$ <> "Y"
	THEN
		TITLE$(5%) = &
			"Asset#     Description                  " + &
			"            Loc  Tp    ServDate  " + &
			"     AdjBasis Period      DepAmt   PerDepAmt " + &
			"  TotDepAmt St"
		TITLE$(6%) = "                  Cost   Salvage   Sect179    ITCAmt " + &
			"ITCReduce RetDate       InitUnits           DepUnits " + &
			"PerDerUnits TotDepUnits"

		TITLE$(7%) = "."
	ELSE
		TITLE$(5%) = "                  Cost   Salvage   Sect179    ITCAmt " + &
			"ITCReduce               AdjBasis             DepAmt" + &
			"   PerDepAmt   TotDepAmt "
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
			FIND #AD_35ASSET.CH%, KEY #SORT_KEY% GE FROM_ITEM$, &
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

	!
	! Check current record
	!
17100	WHEN ERROR IN
		GET #AD_CALCULATION.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM + OBJECT$, &
			REGARDLESS
	USE
		AD_CALCULATION::DEP_STATUS	= "N"
		AD_CALCULATION::AMOUNT_CUR	= 0.0
		AD_CALCULATION::UNIT_CUR	= 0.0

		CONTINUE Calc IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_CALCULATION"
		CONTINUE HelpError
	END WHEN

 Calc:
	DEP_STATUS$ = AD_CALCULATION::DEP_STATUS

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
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B)
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
		PRNT_DATE(AD_35ASSET::SERVDATE, 8%) + " "

	TEXT2$ = SPACE$(LEN(AD_35ASSET::ASSET_NUM)) + " " + &
		FORMAT$(AD_35ASSET::COST, "########.##") + " " + &
		FORMAT$(AD_35ASSET::SALVAGE, "######.##") + " " + &
		FORMAT$(AD_35ASSET::BONUS, "######.##") + " " + &
		FORMAT$(AD_35ASSET::ITC, "######.##") + " " + &
		FORMAT$(AD_35ASSET::ITCREDUCE, "######.##") + " "

	SUBTOTAL_COST = SUBTOTAL_COST + AD_35ASSET::COST
	SUBTOTAL_SALVAGE = SUBTOTAL_SALVAGE + AD_35ASSET::SALVAGE
	SUBTOTAL_BONUS = SUBTOTAL_BONUS + AD_35ASSET::BONUS
	SUBTOTAL_ITC = SUBTOTAL_ITC+ AD_35ASSET::ITC
	SUBTOTAL_ITCREDUCE = SUBTOTAL_ITCREDUCE + AD_35ASSET::ITCREDUCE

	RET_DATE$ = SPACE$(LEN(AD_35ASSET::RET_DATE) + 2%)

	IF EDIT$(AD_35ASSET::RET_DATE, -1%) <> ""
	THEN
		RET_DATE$ = PRNT_DATE(AD_35ASSET::RET_DATE, 8%)
	END IF

17210	TEXT2$ = TEXT2$ + RET_DATE$ + " " + &
		FORMAT$(AD_35ASSET::UNITS, "<%>#######.#") + &
		SPACE$(8%)

	ADJUST_BASIS = AD_35ASSET::COST

	!
	! Read Depreciation file
	!
	WHEN ERROR IN
		GET #AD_DEPRECIATION.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM + OBJECT$, &
			REGARDLESS
	USE
		CONTINUE 17220 IF ERR = 155% OR ERR = 9%
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
		AD_DEPCLASS::SALVFACTOR = &
			STRING$(LEN(AD_DEPCLASS::SALVFACTOR), A"?"B)
		AD_DEPCLASS::BONUSFACTOR = &
			STRING$(LEN(AD_DEPCLASS::BONUSFACTOR), A"?"B)
		AD_DEPCLASS::ITCFACTOR = &
			STRING$(LEN(AD_DEPCLASS::ITCFACTOR), A"?"B)
		AD_DEPCLASS::DEPMETHOD = &
			STRING$(LEN(AD_DEPCLASS::DEPMETHOD), A"?"B)
		AD_DEPCLASS::YEARS = &
			STRING$(LEN(AD_DEPCLASS::YEARS), A"?"B)

		CONTINUE 17225 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::SALVAGE &
		IF AD_DEPCLASS::SALVFACTOR = "Y"
	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::BONUS &
		IF AD_DEPCLASS::BONUSFACTOR = "Y"
	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::ITCREDUCE &
		IF AD_DEPCLASS::ITCFACTOR = "Y"

	!
	! Read balance file
	!
17225	WHEN ERROR IN
		GET #AD_BALANCE.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM + OBJECT$, &
			REGARDLESS
	USE
		AD_BALANCE::DEP_STATUS	= " "
		AD_BALANCE::LASTPER	= ""
		AD_BALANCE::AMOUNT_CTD	= 0.0
		AD_BALANCE::UNIT_CTD	= 0.0

		CONTINUE 17230 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

	DEP_STATUS$ = AD_BALANCE::DEP_STATUS IF DEP_STATUS$ = "N"

17230	!
	! Read Balance File
	!
	TEXT$ = TEXT$ + " " + &
		FORMAT$(ADJUST_BASIS, "########.##") + " " + &
		AD_BALANCE::LASTPER + " " + &
		FORMAT$(AD_BALANCE::AMOUNT_CTD, "########.##") + " " + &
		FORMAT$(AD_CALCULATION::AMOUNT_CUR, "########.##") + " " + &
		FORMAT$(AD_BALANCE::AMOUNT_CTD + &
			AD_CALCULATION::AMOUNT_CUR, &
			"########.##") + " " + &
		DEP_STATUS$

	SUBTOTAL_ADJUST_BASIS = SUBTOTAL_ADJUST_BASIS + ADJUST_BASIS

	TEXT2$ = TEXT2$ + " " + &
		FORMAT$(AD_BALANCE::UNIT_CTD, "<%>#######.##") + " " + &
		FORMAT$(AD_CALCULATION::UNIT_CUR, "<%>#######.##") + " " + &
		FORMAT$(AD_CALCULATION::UNIT_CUR + &
			AD_BALANCE::UNIT_CTD, "<%>#######.##")

	SUBTOTAL_AMOUNT_CTD = SUBTOTAL_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD
	SUBTOTAL_AMOUNT_CUR = SUBTOTAL_AMOUNT_CUR + AD_CALCULATION::AMOUNT_CUR

17300	!
	! Print out
	!
	IF ONLY_TOTAL$ <> "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT2$, 0%)
	END IF
	PRINT_SUBTOTAL% = -1%

	SELECT DEP_STATUS$

	CASE "A"
		A_COST = A_COST + AD_35ASSET::COST
		A_SALVAGE = A_SALVAGE + AD_35ASSET::SALVAGE
		A_BONUS = A_BONUS + AD_35ASSET::BONUS
		A_ITC = A_ITC + AD_35ASSET::ITC
		A_ITCREDUCE = A_ITCREDUCE + AD_35ASSET::ITCREDUCE
		A_ADJUST_BASIS = A_ADJUST_BASIS + ADJUST_BASIS
		A_AMOUNT_CUR = A_AMOUNT_CUR + AD_CALCULATION::AMOUNT_CUR
		A_AMOUNT_CTD = A_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD

	CASE "R"
		R_COST = R_COST + AD_35ASSET::COST
		R_SALVAGE = R_SALVAGE + AD_35ASSET::SALVAGE
		R_BONUS = R_BONUS + AD_35ASSET::BONUS
		R_ITC = R_ITC + AD_35ASSET::ITC
		R_ITCREDUCE = R_ITCREDUCE + AD_35ASSET::ITCREDUCE
		R_ADJUST_BASIS = R_ADJUST_BASIS + ADJUST_BASIS
		R_AMOUNT_CUR = R_AMOUNT_CUR + AD_CALCULATION::AMOUNT_CUR
		R_AMOUNT_CTD = R_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD

	CASE "N"
		N_COST = N_COST + AD_35ASSET::COST
		N_SALVAGE = N_SALVAGE + AD_35ASSET::SALVAGE
		N_BONUS = N_BONUS + AD_35ASSET::BONUS
		N_ITC = N_ITC + AD_35ASSET::ITC
		N_ITCREDUCE = N_ITCREDUCE + AD_35ASSET::ITCREDUCE
		N_ADJUST_BASIS = N_ADJUST_BASIS + ADJUST_BASIS
		N_AMOUNT_CUR = N_AMOUNT_CUR + AD_CALCULATION::AMOUNT_CUR
		N_AMOUNT_CTD = N_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD

	CASE "F"
		F_COST = F_COST + AD_35ASSET::COST
		F_SALVAGE = F_SALVAGE + AD_35ASSET::SALVAGE
		F_BONUS = F_BONUS + AD_35ASSET::BONUS
		F_ITC = F_ITC + AD_35ASSET::ITC
		F_ITCREDUCE = F_ITCREDUCE + AD_35ASSET::ITCREDUCE
		F_ADJUST_BASIS = F_ADJUST_BASIS + ADJUST_BASIS
		F_AMOUNT_CUR = F_AMOUNT_CUR + AD_CALCULATION::AMOUNT_CUR
		F_AMOUNT_CTD = F_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD

	CASE "X"
		X_COST = X_COST + AD_35ASSET::COST
		X_SALVAGE = X_SALVAGE + AD_35ASSET::SALVAGE
		X_BONUS = X_BONUS + AD_35ASSET::BONUS
		X_ITC = X_ITC + AD_35ASSET::ITC
		X_ITCREDUCE = X_ITCREDUCE + AD_35ASSET::ITCREDUCE
		X_ADJUST_BASIS = X_ADJUST_BASIS + ADJUST_BASIS
		X_AMOUNT_CUR = X_AMOUNT_CUR + AD_CALCULATION::AMOUNT_CUR
		X_AMOUNT_CTD = X_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD

	END SELECT

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
		TEXT$ = ":::: Summary "
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 8%)
		TEXT$ = "Active     " + &
			FORMAT$(A_COST, "########.##") + " " + &
			FORMAT$(A_SALVAGE, "######.##") + " " + &
			FORMAT$(A_BONUS, "######.##") + " " + &
			FORMAT$(A_ITC, "######.##") + " " + &
			FORMAT$(A_ITCREDUCE, "######.##") + &
			SPACE$(12%) + &
			FORMAT$(A_ADJUST_BASIS, "########.##") + " " + &
			"        " + &
			FORMAT$(A_AMOUNT_CTD, "########.##") + " " + &
			FORMAT$(A_AMOUNT_CUR, "########.##") + " " + &
			FORMAT$(A_AMOUNT_CUR + A_AMOUNT_CTD, "########.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Fully Dep  " + &
			FORMAT$(F_COST, "########.##") + " " + &
			FORMAT$(F_SALVAGE, "######.##") + " " + &
			FORMAT$(F_BONUS, "######.##") + " " + &
			FORMAT$(F_ITC, "######.##") + " " + &
			FORMAT$(F_ITCREDUCE, "######.##") + &
			SPACE$(12%) + &
			FORMAT$(F_ADJUST_BASIS, "########.##") + " " + &
			"        " + &
			FORMAT$(F_AMOUNT_CTD, "########.##") + " " + &
			FORMAT$(F_AMOUNT_CUR, "########.##") + " " + &
			FORMAT$(F_AMOUNT_CUR + F_AMOUNT_CTD, "########.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "A + F      " + &
			FORMAT$(A_COST + F_COST, "########.##") + " " + &
			FORMAT$(A_SALVAGE + F_SALVAGE, "######.##") + " " + &
			FORMAT$(A_BONUS + F_BONUS, "######.##") + " " + &
			FORMAT$(A_ITC + F_ITC, "######.##") + " " + &
			FORMAT$(A_ITCREDUCE + F_ITCREDUCE, "######.##") + &
			SPACE$(12%) + &
			FORMAT$(A_ADJUST_BASIS + F_ADJUST_BASIS, &
				"########.##") + " " + &
			"        " + &
			FORMAT$(A_AMOUNT_CTD + F_AMOUNT_CTD, &
				"########.##") + " " + &
			FORMAT$(A_AMOUNT_CUR + F_AMOUNT_CUR, &
				"########.##") + " " + &
			FORMAT$(A_AMOUNT_CUR + A_AMOUNT_CTD + &
				F_AMOUNT_CUR + F_AMOUNT_CTD, &
				"########.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Retired    " + &
			FORMAT$(R_COST, "########.##") + " " + &
			FORMAT$(R_SALVAGE, "######.##") + " " + &
			FORMAT$(R_BONUS, "######.##") + " " + &
			FORMAT$(R_ITC, "######.##") + " " + &
			FORMAT$(R_ITCREDUCE, "######.##") + &
			SPACE$(12%) + &
			FORMAT$(R_ADJUST_BASIS, "########.##") + " " + &
			"        " + &
			FORMAT$(R_AMOUNT_CTD, "########.##") + " " + &
			FORMAT$(R_AMOUNT_CUR, "########.##") + " " + &
			FORMAT$(R_AMOUNT_CUR + R_AMOUNT_CTD, &
				"########.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Non Dep    " + &
			FORMAT$(N_COST, "########.##") + " " + &
			FORMAT$(N_SALVAGE, "######.##") + " " + &
			FORMAT$(N_BONUS, "######.##") + " " + &
			FORMAT$(N_ITC, "######.##") + " " + &
			FORMAT$(N_ITCREDUCE, "######.##") + &
			SPACE$(12%) + &
			FORMAT$(N_ADJUST_BASIS, "########.##") + " " + &
			"        " + &
			FORMAT$(N_AMOUNT_CTD, "########.##") + " " + &
			FORMAT$(N_AMOUNT_CUR, "########.##") + " " + &
			FORMAT$(N_AMOUNT_CUR + N_AMOUNT_CTD, "########.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Dep Undef  " + &
			FORMAT$(X_COST, "########.##") + " " + &
			FORMAT$(X_SALVAGE, "######.##") + " " + &
			FORMAT$(X_BONUS, "######.##") + " " + &
			FORMAT$(X_ITC, "######.##") + " " + &
			FORMAT$(X_ITCREDUCE, "######.##") + &
			SPACE$(12%) + &
			FORMAT$(X_ADJUST_BASIS, "########.##") + " " + &
			"        " + &
			FORMAT$(X_AMOUNT_CTD, "########.##") + " " + &
			FORMAT$(X_AMOUNT_CUR, "########.##") + " " + &
			FORMAT$(X_AMOUNT_CUR + X_AMOUNT_CTD, "########.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Grand Total" + &
			FORMAT$(A_COST + F_COST + R_COST + X_COST + N_COST, &
				"########.##") + " " + &
			FORMAT$(A_SALVAGE + F_SALVAGE + R_SALVAGE + &
				X_SALVAGE + N_SALVAGE, &
				"######.##") + " " + &
			FORMAT$(A_BONUS + F_BONUS + R_BONUS + X_BONUS + N_BONUS, &
				"######.##") + " " + &
			FORMAT$(A_ITC + F_ITC + R_ITC + X_ITC + N_ITC, &
				"######.##") + " " + &
			FORMAT$(A_ITCREDUCE + F_ITCREDUCE + R_ITCREDUCE + &
				X_ITCREDUCE + N_ITCREDUCE, "######.##") + " " + &
			STRING$(11%, A":"B) + &
			FORMAT$(A_ADJUST_BASIS + F_ADJUST_BASIS + &
				R_ADJUST_BASIS + X_ADJUST_BASIS + &
				N_ADJUST_BASIS, &
				"########.##") + " " + &
			"::::::: " + &
			FORMAT$(A_AMOUNT_CTD + F_AMOUNT_CTD + R_AMOUNT_CTD + &
				X_AMOUNT_CTD + N_AMOUNT_CTD, &
				"########.##") + " " + &
			FORMAT$(A_AMOUNT_CUR + F_AMOUNT_CUR + R_AMOUNT_CUR + &
				X_AMOUNT_CUR + N_AMOUNT_CUR, "########.##") + " " + &
			FORMAT$(A_AMOUNT_CTD + F_AMOUNT_CTD + R_AMOUNT_CTD + &
				X_AMOUNT_CTD + N_AMOUNT_CTD + &
				A_AMOUNT_CUR + F_AMOUNT_CUR + R_AMOUNT_CUR + &
				X_AMOUNT_CUR + N_AMOUNT_CUR, "########.##") + " "
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
		STRING$(12%, A":"B) + &
		FORMAT$(SUBTOTAL_ADJUST_BASIS, "########.##") + " " + &
		"       " + &
		FORMAT$(SUBTOTAL_AMOUNT_CTD, "########.##") + " " + &
		FORMAT$(SUBTOTAL_AMOUNT_CUR, "########.##") + " " + &
		FORMAT$(SUBTOTAL_AMOUNT_CUR + SUBTOTAL_AMOUNT_CTD, &
			"########.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	PRINT_TOTAL% = -1%
	PRINT_SUBTOTAL% = 0%

	SUBTOTAL_COST = 0.0
	SUBTOTAL_SALVAGE = 0.0
	SUBTOTAL_BONUS = 0.0
	SUBTOTAL_ITC = 0.0
	SUBTOTAL_ITCREDUCE = 0.0
	SUBTOTAL_ADJUST_BASIS = 0.0
	SUBTOTAL_AMOUNT_CUR = 0.0
	SUBTOTAL_AMOUNT_CTD = 0.0

	RETURN

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
