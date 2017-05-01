1	%TITLE "Fixed Asset by Depreciation Method"
	%SBTTL "AD_RPRT_FIXDEPMETHOD"
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
	! ID:AD008
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Fixed Asset by Depreciation Method\* prints a
	!	report of the asset depreciation in order of the depreciation method.
	!	.b
	!	The following fields are included:
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
	!	Investment Tax Credit Reduce
	!	.le
	!	Service Date
	!	.le
	!	Retirement Date
	!	.le
	!	Class
	!	.le
	!	Method
	!	.le
	!	Recovery Date
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
	!	.lm -5
	!
	! Index:
	!	.x Report>Fixed Asset by Depreciation Method
	!	.x Fixed Asset by Depreciation Method>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_FIXDEPMETHOD/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_FIXDEPMETHOD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_FIXDEPMETHOD.OBJ;*
	!
	! AUTHOR:
	!
	!	09/25/88 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	02/25/91 - Craig Tanner
	!		Modified to use AD_35ASSET.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 source code standards.
	!		Change STAT$ to STAT% in call to ASSG_CHANNEL.
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	10/03/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code.
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/25/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.HB"
	MAP (AD_METHOD) AD_METHOD_CDD AD_METHOD

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION) UTL_LOCATION_CDD UTL_LOCATION

	MAP (AD_TEMP) &
		AD_TEMP.PROPTYPE$ = 2%, &
		AD_TEMP.DEPCLASS$ = 4%, &
		AD_TEMP.DEPMETHOD$ = 4%, &
		AD_TEMP.YEARS$ = 4%, &
		AD_TEMP.SALVFACTOR$ = 1%, &
		AD_TEMP.BONUSFACTOR$ = 1%, &
		AD_TEMP.ITCFACTOR$ = 1%, &
		AD_TEMP.ASSETNUM$ = 10%

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Depreciation Method\*
	!	.b
	!	.lm +5
	!	The ^*Depreciation Method\* causes
	!	the report to begin with the selected method.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	method in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Depreciation Method>Fixed Asset>By Depreciation Method
	!	.x Fixed Asset>By Depreciation Method>From Depreciation Method
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Depreciation Method\*
	!	.b
	!	.lm +5
	!	The ^*Depreciation Method\* causes
	!	the report to end with the selected method.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	method in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Depreciation Method>Fixed Asset>By Depreciation Method
	!	.x Fixed Asset>By Depreciation Method>To Depreciation Method
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting prints a report
	!	including selected depreciation methods using the wildcarding
	!	technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Fixed Asset>By Depreication Method
	!	.x Fixed Asset>By Depreciation Method>Wildcard
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
	!	.x Object>Fixed Asset>By Depreciation Method
	!	.x Fixed Asset>By Depreciation Method>Object
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
	!	.x Only Totals>Fixed Asset>By Depreciation Method
	!	.x Fixed Asset>By Depreciation Method>Only Totals
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(8%), 132%))

	!++
	! Abstract:FLD09
	!	^*(09) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field causes the
	!	report to begin with the selected date.
	!	.b
	!	A blank setting will cause the report to begin with the
	!	first date in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Date>Fixed Asset>By Depreciation Type
	!	.x Fixed Asset>By Depreciation Type>From Date
	!
	!--

	TO_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(9%), 132%))

	!++
	! Abstract:FLD10
	!	^*(10) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field causes the
	!	report to end with the selected date.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last date in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Date>Fixed Asset>By Depreciation Method
	!	.x Fixed Asset>By Depreciation Method>To Date
	!
	!--

	TIME_PERIOD$ = ""
	TIME_PERIOD$ = "  FROM " + PRNT_DATE(FROM_DATE$, 8%) &
		IF FROM_DATE$ <> "" AND TO_DATE$ = ""
	TIME_PERIOD$ = "  TO " + PRNT_DATE(TO_DATE$, 8%) &
		IF FROM_DATE$ = "" AND TO_DATE$ <> ""
	TIME_PERIOD$ = "  FROM " + PRNT_DATE(FROM_DATE$, 8%) + " TO " + &
		PRNT_DATE(TO_DATE$, 8%) &
		IF FROM_DATE$ <> "" AND TO_DATE$ <> ""

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
	USE
		CONTINUE 310 IF ERR = 5%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.OPN"
	USE
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
		%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "AD_METHOD"
		CONTINUE HelpError
	END WHEN

370	CALL ASSG_CHANNEL(AD_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "AD_TEMP.TMP" FOR OUTPUT &
			AS FILE #AD_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AD_TEMP, &
			PRIMARY KEY (AD_TEMP.DEPMETHOD$) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		CONTINUE ReportTitle  IF ERR = 5%
		FILENAME$ = "AD_TEMP"
		CONTINUE HelpError
	END WHEN

400	WHEN ERROR IN
		FIND #AD_DEPRECIATION.CH%, KEY #1% GE OBJECT$, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Creating Temporary File.", 1% + 16%)

 GetNextDep:
410	WHEN ERROR IN
		GET #AD_DEPRECIATION.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

	GOTO ReportTitle IF AD_DEPRECIATION::DEP_OBJECT <> OBJECT$

420	WHEN ERROR IN
		GET #AD_DEPCLASS.CH%, KEY #0% EQ AD_DEPRECIATION::DEPCLASS, &
			REGARDLESS
	USE
		AD_DEPCLASS::PROPTYPE = &
			STRING$(LEN(AD_DEPCLASS::PROPTYPE), A"?"B)
		AD_DEPCLASS::DEPMETHOD = &
			STRING$(LEN(AD_DEPCLASS::DEPMETHOD), A"?"B)
		AD_DEPCLASS::YEARS= &
			STRING$(LEN(AD_DEPCLASS::YEARS), A"?"B)
		AD_DEPCLASS::SALVFACTOR = &
			STRING$(LEN(AD_DEPCLASS::SALVFACTOR), A"?"B)
		AD_DEPCLASS::BONUSFACTOR = &
			STRING$(LEN(AD_DEPCLASS::BONUSFACTOR), A"?"B)
		AD_DEPCLASS::ITCFACTOR = &
			STRING$(LEN(AD_DEPCLASS::ITCFACTOR), A"?"B)

		CONTINUE AddTemp IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

 AddTemp:

	AD_TEMP.PROPTYPE$ = AD_DEPCLASS::PROPTYPE
	AD_TEMP.DEPCLASS$ = AD_DEPRECIATION::DEPCLASS
	AD_TEMP.DEPMETHOD$ = AD_DEPCLASS::DEPMETHOD
	AD_TEMP.YEARS$ = AD_DEPCLASS::YEARS
	AD_TEMP.SALVFACTOR$ = AD_DEPCLASS::SALVFACTOR
	AD_TEMP.BONUSFACTOR$ = AD_DEPCLASS::BONUSFACTOR
	AD_TEMP.ITCFACTOR$ = AD_DEPCLASS::ITCFACTOR
	AD_TEMP.ASSETNUM$ = AD_DEPRECIATION::ASSET_NUM

	GOTO GetNextDep IF WLDCRD$ <> "" AND &
		COMP_STRING(EDIT$(AD_TEMP.DEPMETHOD$, -1%), WLDCRD$) = 0%

430	PUT #AD_TEMP.CH%

	GOTO GetNextDep

 ReportTitle:
	TITLE$(1%) = "DEPRECIATION  ASSET  BY  DEPRECIATION  METHOD"
	TITLE$(2%) = "FOR  OBJECT  " + OBJECT$ + " " + &
		TRM$(AD_OBJECT::DESCRIPTION) + &
		TIME_PERIOD$
	TITLE$(3%) = "Asset Depreciation System"
	TITLE$(4%) = ""

	IF ONLY_TOTAL$ <> "Y"
	THEN
		TITLE$(5%) = &
			"Asset#     Description                  " + &
			"            Loc  Tp    ServDate  " + &
			" Class Mthd  RecPeriod      AdjBasis St    " + &
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
			RESET #AD_TEMP.CH%
		ELSE
			FIND #AD_TEMP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AD_TEMP"
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
		GET #AD_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal  IF (AD_TEMP.DEPMETHOD$ > TO_ITEM$) AND TO_ITEM$ <> ""

17120	RET_DATE$ = SPACE$(LEN(AD_35ASSET::RET_DATE) + 2%)

	WHEN ERROR IN
		GET #AD_35ASSET.CH%, KEY #0% EQ AD_TEMP.ASSETNUM$, REGARDLESS
	USE
		AD_35ASSET::DESCRIPTION = &
			STRING$(LEN(AD_35ASSET::DESCRIPTION), A"?"B)
		AD_35ASSET::LOCATION = &
			STRING$(LEN(AD_35ASSET::LOCATION), A"?"B)
		AD_35ASSET::ASSET_TYPE= &
			STRING$(LEN(AD_35ASSET::ASSET_TYPE), A"?"B)
		AD_35ASSET::SERVDATE = &
			STRING$(LEN(AD_35ASSET::SERVDATE), A"?"B)
		AD_35ASSET::COST = 0.0
		AD_35ASSET::SALVAGE = 0.0
		AD_35ASSET::ITC = 0.0
		AD_35ASSET::ITCREDUCE = 0.0
		AD_35ASSET::UNITS = 0.0

		CONTINUE CheckDate IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

	IF EDIT$(AD_35ASSET::RET_DATE, -1%) <> ""
	THEN
		RET_DATE$ = PRNT_DATE(AD_35ASSET::RET_DATE, 8%)
		ASSET_STATUS$ = "R"
	END IF

 CheckDate:
	GOTO GetNextRec &
		IF (AD_35ASSET::SERVDATE < FROM_DATE$) AND FROM_DATE$ <> ""
	GOTO GetNextRec IF (AD_35ASSET::SERVDATE > TO_DATE$) AND TO_DATE$ <> ""

	GOTO TestType IF TEST_STRING$ = AD_TEMP.DEPMETHOD$

	GOSUB 18000 IF TEST_STRING$ <> ""

17130	WHEN ERROR IN
		GET #AD_METHOD.CH%, KEY #0% EQ AD_TEMP.DEPMETHOD$, REGARDLESS
	USE
		AD_METHOD::DESCRIPTION = &
			STRING$(LEN(AD_METHOD::DESCRIPTION), A"?"B)

		CONTINUE 17135 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_METHOD"
		CONTINUE HelpError
	END WHEN

17135	TEXT$ = ":::: " + &
		TRM$(AD_TEMP.DEPMETHOD$) + " " + &
		TRM$(AD_METHOD::DESCRIPTION) + " "
	TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
 TestType:

	TEST_STRING$ = AD_TEMP.DEPMETHOD$

	ADJUST_BASIS = AD_35ASSET::COST
	ASSET_STATUS$ = "N"
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

17210	TEXT2$ = TEXT2$ + " " + &
		RET_DATE$ + &
		SPACE$(10%) + &
		FORMAT$(AD_35ASSET::UNITS, "<%>#,###,###.#")

	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::SALVAGE &
		IF AD_TEMP.SALVFACTOR$ = "Y"
	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::BONUS &
		IF AD_TEMP.BONUSFACTOR$ = "Y"
	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::ITCREDUCE &
		IF AD_TEMP.ITCFACTOR$ = "Y"

17230	!
	! Read Balance File
	!
	TEXT$ = TEXT$	+ " " + &
		AD_TEMP.DEPCLASS$ + "  " + &
		AD_TEMP.DEPMETHOD$ + "       " + &
		AD_TEMP.YEARS$ + " " + &
		FORMAT$(ADJUST_BASIS, "##,###,###.##")

	SUBTOTAL_ADJUST_BASIS = SUBTOTAL_ADJUST_BASIS + ADJUST_BASIS

	WHEN ERROR IN
		GET #AD_BALANCE.CH%, &
			KEY #0% EQ AD_TEMP.ASSETNUM$ + OBJECT$, &
			REGARDLESS
	USE
		AD_BALANCE::AMOUNT_CTD	= 0.0
		AD_BALANCE::LASTPER	= &
			STRING$(LEN(AD_BALANCE::LASTPER), A"?"B)

		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

	TEXT$ = TEXT$ + " " + &
		AD_BALANCE::DEP_STATUS + " " + &
		FORMAT$(AD_BALANCE::AMOUNT_CTD, "##,###,###.##") + " " + &
		AD_BALANCE::LASTPER

	TEXT2$ = TEXT2$ + SPACE$(17%) + &
		FORMAT$(AD_BALANCE::UNIT_CTD, "<%>#,###,###.##")

	ASSET_STATUS$ = AD_BALANCE::DEP_STATUS IF ASSET_STATUS$ = "N"
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
		TEXT$ = ":::: Summary "
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 7%)
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
			FORMAT$(A_COST+F_COST+N_COST, "########.##") + " " + &
			FORMAT$(A_SALVAGE + F_SALVAGE + N_SALVAGE, &
				"######.##") + " " + &
			FORMAT$(A_BONUS + F_BONUS + N_BONUS, "######.##") + " " + &
			FORMAT$(A_ITC + F_ITC + N_ITC, "######.##") + " " + &
			FORMAT$(A_ITCREDUCE + F_ITCREDUCE+N_ITCREDUCE, &
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
			FORMAT$(A_ITC + F_ITC + N_ITC + R_ITC, &
				"######.##") + " " + &
			FORMAT$(A_ITCREDUCE + F_ITCREDUCE + N_ITCREDUCE + &
				R_ITCREDUCE, &
				"######.##") + " " + &
			STRING$(32%, A":"B) + " " + &
			FORMAT$(A_ADJUST_BASIS + F_ADJUST_BASIS + N_ADJUST_BASIS+ &
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
