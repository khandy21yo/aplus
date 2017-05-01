1	%TITLE "Fixed Asset Report"
	%SBTTL "AD_RPRT_FIXED"
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
	! ID:AD007
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Fixed Assets\* report prints all of the records
	!	in the Asset Description File, including information
	!	concerning depreciation methods, balances, and retired date.
	!	.b
	!	The report contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Cost
	!	.le
	!	Salvage
	!	.le
	!	Section 179
	!	.le
	!	Investment Tax Credit Amount
	!	.le
	!	Investment Tax Credit Reduce
	!	.le
	!	Adjusted Basis
	!	.le
	!	Total Depreciation Amount
	!	.els
	!
	! Index:
	!	.x Report>Fixed Assets
	!	.x Fixed Assets>Report
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_FIXED/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_FIXED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_FIXED.OBJ;*
	!
	! AUTHOR:
	!
	!	01/22/88 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standrds.
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
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/28/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.HB"
	MAP (AD_BALANCE) AD_BALANCE_CDD AD_BALANCE

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
	!	.x Sort>Fixed Asset Report
	!	.x Fixed Asset Report>Sort
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*Item _#\* causes the printing
	!	to begin with the selected Item _#.  The value entered must
	!	be in agreement with the value in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Fixed Asset Report
	!	.x Fixed Asset Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* value causes
	!	the report to end with a selected Item _#.  The value entered must be
	!	in agreement with the value in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Fixed Asset Report
	!	.x Fixed Asset Report>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting prints a
	!	report including only selected assets using the wildcarding
	!	technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Fixed Asset Report
	!	.x Fixed Asset Report>Wildcard
	!
	! Required:
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
	!	.x Object>Fixed Asset Report
	!	.x Fixed Asset Report>Object
	!
	!--

	ONLY_TOTAL$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) Only Totals (Y,N)\*
	!	.b
	!	.lm +5
	!	The ^*Only Totals\* field prints the entire report by
	!	entering ^*N\* or print only report totals by entering a ^*Y\*.
	!	.lm -5
	!
	! Index:
	!	.x Only Totals>Fixed Asset Report
	!	.x Fixed Asset Report>Only Totals
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

		TO_ITEM$ = DATE_STOREDATE( &
			EDIT$(UTL_REPORTX::OPTDEF(2%), 132%))

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
		%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "AD_BALANCE"
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
	TITLE$(1%) = "FIXED  ASSET  REPORT  " + ADD_TITLE$ + &
		"  FOR  OBJECT  " + OBJECT$ + " " + &
		TRM$(AD_OBJECT::DESCRIPTION)
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	IF ONLY_TOTAL$ <> "Y"
	THEN
		TITLE$(4%) = &
			"Asset#     Description                  " + &
			"            Loc  Tp    ServDate  " + &
			" Class Mthd  RecPeriod      AdjBasis St    " + &
			"TotDepAmt Period"
		TITLE$(5%) = "                  Cost   Salvage   " + &
			"Sect179    ITCAmt " + &
			"ITCReduce RetDate                InitUnits" + &
			"                   TotDepUnits"

		TITLE$(6%) = "."
	ELSE
		TITLE$(4%) = "                  Cost   Salvage   " + &
			"Sect179    ITCAmt " + &
			"ITCReduce                                 " + &
			"      AdjBasis       TotDepAmt"

		TITLE$(5%) = "."
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

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
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

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
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

	IF EDIT$(AD_35ASSET::RET_DATE, -1%) <> ""
	THEN
		RET_DATE$ = PRNT_DATE(AD_35ASSET::RET_DATE, 8%)
		ASSET_STATUS$ = "R"
	ELSE
		ASSET_STATUS$ = "N"
	END IF

	TEXT$ = AD_35ASSET::ASSET_NUM + " " + &
		TRM$(AD_35ASSET::DESCRIPTION) + &
		STRING$(LEN(AD_35ASSET::DESCRIPTION) - &
			LEN(TRM$(AD_35ASSET::DESCRIPTION)), A"."B) + " " + &
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
	SUBTOTAL_ITC = SUBTOTAL_ITC+ AD_35ASSET::ITC
	SUBTOTAL_ITCREDUCE = SUBTOTAL_ITCREDUCE + AD_35ASSET::ITCREDUCE

	RET_DATE$ = SPACE$(LEN(AD_35ASSET::RET_DATE) + 2%)

17210	TEXT2$ = TEXT2$ + " " + &
		RET_DATE$ + &
		SPACE$(10%) + &
		FORMAT$(AD_35ASSET::UNITS, "<%>#,###,###.#")
	!
	! Read Depreciation file
	!
	WHEN ERROR IN
		GET #AD_DEPRECIATION.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM + OBJECT$, &
			REGARDLESS
	USE
		AD_DEPRECIATION::DEPCLASS = ""
		AD_DEPCLASS::DEPMETHOD = ""
		AD_DEPCLASS::YEARS = ""
		ADJUST_BASIS = AD_35ASSET::COST

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
	! Read Balance File
	!
	TEXT$ = TEXT$ + " " + &
		AD_DEPRECIATION::DEPCLASS + "  " + &
		AD_DEPCLASS::DEPMETHOD + "       " + &
		AD_DEPCLASS::YEARS + " " + &
		FORMAT$(ADJUST_BASIS, "##,###,###.##")

	SUBTOTAL_ADJUST_BASIS = SUBTOTAL_ADJUST_BASIS + ADJUST_BASIS

	WHEN ERROR IN
		GET #AD_BALANCE.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM + OBJECT$, &
			REGARDLESS
	USE
		AD_BALANCE::AMOUNT_CTD = 0.0
		AD_BALANCE::LASTPER = &
			STRING$(LEN(AD_BALANCE::LASTPER), A"?"B)

		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

	ASSET_STATUS$ = AD_BALANCE::DEP_STATUS IF ASSET_STATUS$ = "N"

	TEXT$ = TEXT$ + " " + &
		AD_BALANCE::DEP_STATUS + " " + &
		FORMAT$(AD_BALANCE::AMOUNT_CTD, "##,###,###.##") + " " + &
		AD_BALANCE::LASTPER

	TEXT2$ = TEXT2$ + SPACE$(17%) + &
		FORMAT$(AD_BALANCE::UNIT_CTD, "<%>#,###,###.##")

	SUBTOTAL_AMOUNT_CTD = SUBTOTAL_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD

17300	!
	! Print out
	!
	IF ONLY_TOTAL$ <> "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT2$, 0%)
	END IF
	PRINT_SUBTOTAL% = -1%

	SELECT ASSET_STATUS$

	CASE "N"
		N_COST = N_COST + AD_35ASSET::COST
		N_SALVAGE = N_SALVAGE + AD_35ASSET::SALVAGE
		N_BONUS = N_BONUS + AD_35ASSET::BONUS
		N_ITC = N_ITC + AD_35ASSET::ITC
		N_ITCREDUCE = N_ITCREDUCE + AD_35ASSET::ITCREDUCE
		N_ADJUST_BASIS = N_ADJUST_BASIS + ADJUST_BASIS
		N_AMOUNT_CTD = N_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD

	CASE "A"
		A_COST = A_COST + AD_35ASSET::COST
		A_SALVAGE = A_SALVAGE + AD_35ASSET::SALVAGE
		A_BONUS = A_BONUS + AD_35ASSET::BONUS
		A_ITC = A_ITC + AD_35ASSET::ITC
		A_ITCREDUCE = A_ITCREDUCE + AD_35ASSET::ITCREDUCE
		A_ADJUST_BASIS = A_ADJUST_BASIS + ADJUST_BASIS
		A_AMOUNT_CTD = A_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD

	CASE "R"
		R_COST = R_COST + AD_35ASSET::COST
		R_SALVAGE = R_SALVAGE + AD_35ASSET::SALVAGE
		R_BONUS = R_BONUS + AD_35ASSET::BONUS
		R_ITC = R_ITC + AD_35ASSET::ITC
		R_ITCREDUCE = R_ITCREDUCE + AD_35ASSET::ITCREDUCE
		R_ADJUST_BASIS = R_ADJUST_BASIS + ADJUST_BASIS
		R_AMOUNT_CTD = R_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD

	CASE "F"
		F_COST = F_COST + AD_35ASSET::COST
		F_SALVAGE = F_SALVAGE + AD_35ASSET::SALVAGE
		F_BONUS = F_BONUS + AD_35ASSET::BONUS
		F_ITC = F_ITC + AD_35ASSET::ITC
		F_ITCREDUCE = F_ITCREDUCE + AD_35ASSET::ITCREDUCE
		F_ADJUST_BASIS = F_ADJUST_BASIS + ADJUST_BASIS
		F_AMOUNT_CTD = F_AMOUNT_CTD + AD_BALANCE::AMOUNT_CTD
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
			FORMAT$(A_COST + F_COST + N_COST, "########.##") + &
				" " + &
			FORMAT$(A_SALVAGE + F_SALVAGE + N_SALVAGE, &
				"######.##") + " " + &
			FORMAT$(A_BONUS + F_BONUS + N_BONUS, "######.##") + &
				" " + &
			FORMAT$(A_ITC + F_ITC + N_ITC, "######.##") + " " + &
			FORMAT$(A_ITCREDUCE + F_ITCREDUCE + N_ITCREDUCE, &
				"######.##") + &
			SPACE$(34%) + &
			FORMAT$(A_ADJUST_BASIS + F_ADJUST_BASIS + &
				N_ADJUST_BASIS, "##,###,###.##") + "   " + &
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
			FORMAT$(A_ITC + F_ITC + N_ITC + R_ITC, &
				"######.##") + " " + &
			FORMAT$(A_ITCREDUCE + F_ITCREDUCE + N_ITCREDUCE + &
				R_ITCREDUCE, &
				"######.##") + " " + &
			STRING$(34%, A":"B) + &
			FORMAT$(A_ADJUST_BASIS + F_ADJUST_BASIS + &
				N_ADJUST_BASIS + R_ADJUST_BASIS, &
				"##,###,###.##") + "   " + &
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

	PRINT_TOTAL% = -1%
	PRINT_SUBTOTAL% = 0%

	SUBTOTAL_COST = 0.0
	SUBTOTAL_SALVAGE = 0.0
	SUBTOTAL_BONUS = 0.0
	SUBTOTAL_ITC = 0.0
	SUBTOTAL_ITCREDUCE = 0.0
	SUBTOTAL_ADJUST_BASIS = 0.0
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
