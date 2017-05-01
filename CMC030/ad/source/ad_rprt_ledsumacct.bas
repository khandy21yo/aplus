1	%TITLE "Summary Period Ledger by General Ledger Account"
	%SBTTL "AD_RPRT_LEDSUMACCT"
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
	! ID:AD023
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints out a report containing the year to date depreciation
	!	including the current period calculations from the Archive file. The report
	!	is printed in General Ledger Account Number order and contains the following
	!	fields:
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
	!	Investment Tax Credit Amount
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
	!	Status
	!	.le
	!	Period Depreciation Amount
	!	.le
	!	Period Depreciation Units
	!	.le
	!	Total Depreciation Amount
	!	.le
	!	Total Depreciation Units
	!	.els
	!
	! Index:
	!	.x Summary Ledger by General Ledger Account
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_LEDSUMACCT/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_LEDSUMACCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_LEDSUMACCT.OBJ;*
	!
	! AUTHOR:
	!
	!	09/29/88 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update source code to V3.6 Calico Standards.
	!		Change STAT$ to STAT% in call.
	!
	!	08/21/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	08/23/95 - Kevin Handy
	!		Reformat source so it is easier to read.
	!		Add comments to make program more understandable.
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source.
	!		Change STRING$(...,ASCII(" ")) to "" in several
	!		places.
	!
	!	01/29/96 - Kevin Handy
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	10/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/12/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.HB"
	MAP (AD_CALCULATION) AD_CALCULATION_CDD AD_CALCULATION

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROL.HB"
	MAP (AD_CONTROL) AD_CONTROL_CDD AD_CONTROL

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP (AD_CONTROLOBJ) AD_CONTROLOBJ_CDD AD_CONTROLOBJ

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS) AD_DEPCLASS_CDD AD_DEPCLASS

	%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.HB"
	MAP (AD_ACCOUNT) AD_ACCOUNT_CDD AD_ACCOUNT

	%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.HB"
	MAP (AD_BALANCE) AD_BALANCE_CDD AD_BALANCE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART) GL_CHART_CDD GL_CHART

	RECORD AD_TEMP_CDD
		STRING DEPACCT = 18%
		STRING EXPACCT = 18%
		STRING ASSACCT = 18%
		STRING ASSET = 10%
	END RECORD

	MAP (AD_TEMP) AD_TEMP_CDD AD_TEMP

	RECORD	ACCOUNT_RECORD
		STRING	NUMBER = 18%
		STRING	DESCR = 40%
		REAL	CREDIT_CUR
		REAL	DEBIT_CUR
		REAL	CREDIT_CTD
		REAL	DEBIT_CTD
	END RECORD

	DIM ACCOUNT_RECORD ACCOUNT(1000%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)
	!++
	! Abstract:FLD01
	!	^*(01) Account Numbers\*
	!	.b
	!	.lm +5
	!	The ^*Account Numbers\* field causes the report to
	!	print only selected General Ledger Account Numbers.
	!	.b
	!	Wildcarding techniques may be used.
	!	.lm -5
	!
	! Index:
	!	.x Account Numbers>Summary Ledger by General Ledger Account
	!	.x Summary Ledger by General Ledger Account>Account Numbers
	!
	!--

	ONLY_TOTAL$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) Only Totals (Y,N)\*
	!	.b
	!	.lm +5
	!	The ^*Only Totals\* field prints the entire report by
	!	entering ^*N\* or only report totals by entering ^*Y\*.
	!	.lm -5
	!
	! Index:
	!	.x Only Totals>Summary Ledger by General Ledger Account
	!	.x Summary Ledger by General Ledger Account>Only Totals
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	SELECT SORTBY$
	CASE "A"
		SORTTEXT$ = "By Asset Acct"
	CASE "AE"
		SORTTEXT$ = "By Asset/Expense Account"
	CASE "D"
		SORTTEXT$ = "By Depreciation Account"
	CASE "E"
		SORTTEXT$ = "By Expense Account"
	CASE ELSE
		SORTBY$ = "DE"
		SORTTEXT$ = "By Depreciation/Expense Account"
	END SELECT

	!++
	! Abstract:FLD03
	!	^*(03) Sort By\*
	!	.b
	!	.lm +5
	!	Determines the sortation of the report.
	!	.list 0,"*"
	!	.le
	!	A - Asset
	!	.le
	!	AE - Asset/Expense
	!	.le
	!	D - Depreciation
	!	.le
	!	DE - Depreciation/Expense
	!	.le
	!	E - Expense
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Only Totals>Summary Ledger by General Ledger Account
	!	.x Summary Ledger by General Ledger Account>Only Totals
	!
	!--

300	!
	! Asset
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
	USE
		CONTINUE 310 IF ERR = 5%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

310	!
	! Depreciation
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.OPN"
	USE
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

320	!
	! Calculation
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "AD_CALCULATION"
		CONTINUE HelpError
	END WHEN

340	!
	! Control
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROL.OPN"

		GET #AD_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		AD_CONTROL::DEP_OBJECT = &
			STRING$(LEN(AD_CONTROL::DEP_OBJECT), A"?"B)

		CONTINUE 350 IF ERR = 5% OR ERR = 155%
		FILENAME$ = "AD_CONTROL"
		CONTINUE HelpError
	END WHEN

350	!
	! DepClass
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

360	!
	! Chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

370	!
	! AD Account
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.OPN"
	USE
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

380	!
	! AD ControlObj
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.OPN"

		GET #AD_CONTROLOBJ.CH%, &
			KEY #0% EQ AD_CONTROL::DEP_OBJECT, &
			REGARDLESS
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

	IF AD_CONTROLOBJ::LASTDEP <= AD_CONTROLOBJ::LASTPER
	THEN
		PERIOD$ ="  TO  PERIOD  " + AD_CONTROLOBJ::LASTDEP + " " + &
			TRM$(PERIOD_DESC$)
	ELSE
		PERIOD$ ="  FROM  PERIOD  " + AD_CONTROLOBJ::LASTPER + &
			"  TO  PERIOD  " + AD_CONTROLOBJ::LASTDEP + " " + &
			TRM$(PERIOD_DESC$)
	END IF

390	!
	! AD Balance
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.OPN"
	USE
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

395	!
	! Create work file
	!
	CALL ASSG_CHANNEL(AD_TEMP.CH%,	STAT%)
	CALL  READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)
	WHEN ERROR IN
		SELECT SORTBY$
		CASE "A"
			OPEN UTL_WORK.DEV$ + "AD_TEMP.TMP" FOR OUTPUT AS FILE #AD_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				BUFFER 32%, &
				MAP AD_TEMP, &
				PRIMARY KEY (AD_TEMP::ASSACCT, AD_TEMP::ASSET) DUPLICATES, &
				ACCESS MODIFY, ALLOW NONE, TEMPORARY
		CASE "AE"
			OPEN UTL_WORK.DEV$ + "AD_TEMP.TMP" FOR OUTPUT AS FILE #AD_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				BUFFER 32%, &
				MAP AD_TEMP, &
				PRIMARY KEY (AD_TEMP::ASSACCT, AD_TEMP::EXPACCT) DUPLICATES, &
				ACCESS MODIFY, ALLOW NONE, TEMPORARY
		CASE "D"
			OPEN UTL_WORK.DEV$ + "AD_TEMP.TMP" FOR OUTPUT AS FILE #AD_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				BUFFER 32%, &
				MAP AD_TEMP, &
				PRIMARY KEY (AD_TEMP::DEPACCT, AD_TEMP::ASSET) DUPLICATES, &
				ACCESS MODIFY, ALLOW NONE, TEMPORARY
		CASE "E"
			OPEN UTL_WORK.DEV$ + "AD_TEMP.TMP" FOR OUTPUT AS FILE #AD_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				BUFFER 32%, &
				MAP AD_TEMP, &
				PRIMARY KEY (AD_TEMP::EXPACCT, AD_TEMP::ASSET) DUPLICATES, &
				ACCESS MODIFY, ALLOW NONE, TEMPORARY
		CASE ELSE
			OPEN UTL_WORK.DEV$ + "AD_TEMP.TMP" FOR OUTPUT AS FILE #AD_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				BUFFER 32%, &
				MAP AD_TEMP, &
				PRIMARY KEY (AD_TEMP::DEPACCT, AD_TEMP::EXPACCT) DUPLICATES, &
				ACCESS MODIFY, ALLOW NONE, TEMPORARY
		END SELECT
	USE
		FILENAME$ = "AD_TEMP"
		CONTINUE HelpError
	END WHEN

400	WHEN ERROR IN
		RESET #AD_35ASSET.CH%
	USE
		CONTINUE ReportTitle IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Creating Temporary File.", 1% + 16%)

	!
	! Copy info into work file from AD_ASSET
	!
 GetNextTemp:
410	WHEN ERROR IN
		GET #AD_35ASSET.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

420	WHEN ERROR IN
		GET #AD_ACCOUNT.CH%, &
			KEY #0% EQ AD_35ASSET::LOCATION + &
			AD_35ASSET::ASSET_TYPE, &
			REGARDLESS
	USE
		AD_ACCOUNT::DEP_ACCT = &
			STRING$(LEN(AD_ACCOUNT::DEP_ACCT), A"?"B)
		AD_ACCOUNT::EXP_ACCT = &
			STRING$(LEN(AD_ACCOUNT::DEP_ACCT), A"?"B)

		CONTINUE AddTemp IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 AddTemp:

	AD_TEMP::DEPACCT = AD_ACCOUNT::DEP_ACCT
	AD_TEMP::EXPACCT = AD_ACCOUNT::EXP_ACCT
	AD_TEMP::ASSACCT = AD_ACCOUNT::ASS_ACCT
	AD_TEMP::ASSET = AD_35ASSET::ASSET_NUM

	SELECT SORTBY$
	CASE "A"
		GOTO GetNextTemp IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_TEMP::ASSACCT, -1%), WLDCRD$) = 0%
	CASE "AE"
		GOTO GetNextTemp IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_TEMP::ASSACCT, -1%), WLDCRD$) = 0% AND &
			COMP_STRING(EDIT$(AD_TEMP::EXPACCT, -1%), WLDCRD$) = 0%
	CASE "D"
		GOTO GetNextTemp IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_TEMP::DEPACCT, -1%), WLDCRD$) = 0%
	CASE "E"
		GOTO GetNextTemp IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_TEMP::EXPACCT, -1%), WLDCRD$) = 0%
	CASE ELSE
		GOTO GetNextTemp IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_TEMP::DEPACCT, -1%), WLDCRD$) = 0% AND &
			COMP_STRING(EDIT$(AD_TEMP::EXPACCT, -1%), WLDCRD$) = 0%
	END SELECT

430	PUT #AD_TEMP.CH%

	GOTO GetNextTemp

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "SUMMARY  PERIOD  LEDGER  BY  ACCOUNT  NUMBER"
	TITLE$(2%) = PERIOD$
	TITLE$(3%) = "Asset Depreciation System " + TEXTSOURCE$
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
		RESET #AD_TEMP.CH%
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

17100	!
	! Read Calculation record
	!
	WHEN ERROR IN
		GET #AD_CALCULATION.CH%, &
			KEY #0% EQ AD_TEMP::ASSET + AD_CONTROL::DEP_OBJECT, &
			REGARDLESS
	USE
		AD_CALCULATION::DEP_STATUS = " "
		AD_CALCULATION::AMOUNT_CUR = 0.0
		AD_CALCULATION::UNIT_CUR = 0.0

		CONTINUE 17120 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_CALCULATION"
		CONTINUE HelpError
	END WHEN

17120	DEP_STATUS$ = AD_CALCULATION::DEP_STATUS

	!
	! Read asset record
	!
	WHEN ERROR IN
		GET #AD_35ASSET.CH%, KEY #0% EQ AD_TEMP::ASSET, REGARDLESS
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

		CONTINUE 17130 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

17130	SELECT SORTBY$
	CASE "A"
		GOTO TestType IF TEST_STRING$ = AD_TEMP::ASSACCT
	CASE "AE"
		GOTO TestType IF TEST_STRING$ = AD_TEMP::ASSACCT + &
			AD_TEMP::EXPACCT
	CASE "D"
		GOTO TestType IF TEST_STRING$ = AD_TEMP::DEPACCT
	CASE "E"
		GOTO TestType IF TEST_STRING$ = AD_TEMP::EXPACCT
	CASE ELSE
		GOTO TestType IF TEST_STRING$ = AD_TEMP::DEPACCT + &
			AD_TEMP::EXPACCT
	END SELECT

	!
	! Subtotal if necessary
	!
	GOSUB PrintSubtotal IF TEST_STRING$ <> ""

	!
	! Read chart for DepAcct
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%, &
			KEY #0% EQ AD_TEMP::ASSACCT, &
			REGARDLESS
	USE
		GL_CHART::DESCR	= STRING$(LEN(GL_CHART::DESCR), A"?"B)

		CONTINUE 17132 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

17132	!
	! Print credit line
	!
	IF SORTBY$ = "AE" OR SORTBY$ = "A"
	THEN
		TEXT$ = ":::: Asset  - " + &
			TRM$(AD_TEMP::ASSACCT) + " " + &
			TRM$(GL_CHART::DESCR) + " "
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B) &
			IF SORTBY$ = "A" OR SORTBY$ = "AE"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)
	END IF

17133	!
	! Read chart for DepAcct
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%, &
			KEY #0% EQ AD_TEMP::DEPACCT, &
			REGARDLESS
	USE
		GL_CHART::DESCR	= STRING$(LEN(GL_CHART::DESCR), A"?"B)

		CONTINUE 17135 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

17135	!
	! Print credit line
	!
	IF SORTBY$ = "DE" OR SORTBY$ = "D"
	THEN
		TEXT$ = ":::: Credit - " + &
			TRM$(AD_TEMP::DEPACCT) + " " + &
			TRM$(GL_CHART::DESCR) + " "
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B) &
			IF SORTBY$ = "D" OR SORTBY$ = "DE"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)
	END IF

	DEPACCT_DESC$ = GL_CHART::DESCR

17140	!
	! Read chart for ExpAccount
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%, &
			KEY #0% EQ AD_TEMP::EXPACCT, &
			REGARDLESS
	USE
		GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), A"?"B)

		CONTINUE 17145 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

17145	!
	! Print debit line
	!
	IF SORTBY$ = "DE" OR SORTBY$ = "AE" OR SORTBY$ = "E"
	THEN
		TEXT$ = "     Debit  - " + &
			TRM$(AD_TEMP::EXPACCT) + " " + &
			TRM$(GL_CHART::DESCR)
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B) &
			IF SORTBY$ = "E"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)
	END IF

17150	EXPACCT_DESC$ = GL_CHART::DESCR

 TestType:
	SELECT SORTBY$
	CASE "A"
		TEST_STRING$ = AD_TEMP::ASSACCT
	CASE "AE"
		TEST_STRING$ = AD_TEMP::ASSACCT + AD_TEMP::EXPACCT
	CASE "D"
		TEST_STRING$ = AD_TEMP::DEPACCT
	CASE "E"
		TEST_STRING$ = AD_TEMP::EXPACCT
	CASE ELSE
		TEST_STRING$ = AD_TEMP::DEPACCT + AD_TEMP::EXPACCT
	END SELECT

	ADJUST_BASIS = AD_35ASSET::COST

	!
	! Create title for this asset
	!
	TEXT$ = AD_35ASSET::ASSET_NUM + " " + &
		TRM$(AD_35ASSET::DESCRIPTION) + &
		STRING$(LEN(AD_35ASSET::DESCRIPTION) - &
			LEN(TRM$(AD_35ASSET::DESCRIPTION)), A"."B) + &
			" " + &
		AD_35ASSET::LOCATION + " " + &
		AD_35ASSET::ASSET_TYPE + "    " + &
		PRNT_DATE(AD_35ASSET::SERVDATE, 8%)

	TEXT2$ = SPACE$(LEN(AD_35ASSET::ASSET_NUM)) + " " + &
		FORMAT$(AD_35ASSET::COST, "########.##") + " " + &
		FORMAT$(AD_35ASSET::SALVAGE, "######.##") + " " + &
		FORMAT$(AD_35ASSET::BONUS, "######.##") + " " + &
		FORMAT$(AD_35ASSET::ITC, "######.##") + " " + &
		FORMAT$(AD_35ASSET::ITCREDUCE, "######.##")

	SUBTOTAL_COST = SUBTOTAL_COST	+ AD_35ASSET::COST
	SUBTOTAL_SALVAGE = SUBTOTAL_SALVAGE + AD_35ASSET::SALVAGE
	SUBTOTAL_BONUS = SUBTOTAL_BONUS + AD_35ASSET::BONUS
	SUBTOTAL_ITC = SUBTOTAL_ITC+ AD_35ASSET::ITC
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
	AD_DEPCLASS::DEPMETHOD = ""
	AD_DEPCLASS::YEARS = ""
	ADJUST_BASIS = AD_35ASSET::COST

	WHEN ERROR IN
		GET #AD_DEPRECIATION.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM + AD_CONTROL::DEP_OBJECT, &
			REGARDLESS
	USE
		AD_DEPRECIATION::DEPCLASS = ""

		CONTINUE 17220 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

17220	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::SALVAGE &
		IF AD_DEPCLASS::SALVFACTOR = "Y"
	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::BONUS &
		IF AD_DEPCLASS::BONUSFACTOR = "Y"
	ADJUST_BASIS = ADJUST_BASIS - AD_35ASSET::ITCREDUCE &
		IF AD_DEPCLASS::ITCFACTOR = "Y"

	!
	! Read balance file
	!
	WHEN ERROR IN
		GET #AD_BALANCE.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM + AD_CONTROL::DEP_OBJECT, &
			REGARDLESS
	USE
		AD_BALANCE::DEP_STATUS = " "
		AD_BALANCE::LASTPER = ""
		AD_BALANCE::AMOUNT_CTD = 0.0
		AD_BALANCE::UNIT_CTD = 0.0

		CONTINUE 17230 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

	DEP_STATUS$ = AD_BALANCE::DEP_STATUS &
		IF DEP_STATUS$ = " "

17230	!
	! Read Balance File
	!
	TEXT$ = TEXT$ + "  " + &
		FORMAT$(ADJUST_BASIS, "########.##") + " " + &
		AD_BALANCE::LASTPER + " " + &
		FORMAT$(AD_BALANCE::AMOUNT_CTD, "########.##") + " " + &
		FORMAT$(AD_CALCULATION::AMOUNT_CUR, "########.##") + " " + &
		FORMAT$(AD_BALANCE::AMOUNT_CTD + AD_CALCULATION::AMOUNT_CUR, &
			"########.##") + " " + &
		DEP_STATUS$

	SUBTOTAL_ADJUST_BASIS = SUBTOTAL_ADJUST_BASIS + ADJUST_BASIS

	TEXT2$ = TEXT2$ + "  " + &
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

	FOR LOOP% = 1% TO LOOP_ACCT%
		GOTO AddCredit IF ACCOUNT(LOOP%)::NUMBER = AD_TEMP::DEPACCT
	NEXT LOOP%

	LOOP%, LOOP_ACCT% = LOOP_ACCT% + 1%
	ACCOUNT(LOOP%)::NUMBER		= AD_TEMP::DEPACCT
	ACCOUNT(LOOP%)::DESCR		= DEPACCT_DESC$
	ACCOUNT(LOOP%)::CREDIT_CUR	= 0.0
	ACCOUNT(LOOP%)::DEBIT_CUR	= 0.0
	ACCOUNT(LOOP%)::CREDIT_CTD	= 0.0
	ACCOUNT(LOOP%)::DEBIT_CTD	= 0.0

 AddCredit:
	ACCOUNT(LOOP%)::CREDIT_CUR = ACCOUNT(LOOP%)::CREDIT_CUR - &
		AD_CALCULATION::AMOUNT_CUR
	ACCOUNT(LOOP%)::CREDIT_CTD = ACCOUNT(LOOP%)::CREDIT_CTD - &
		AD_BALANCE::AMOUNT_CTD

	FOR LOOP% = 1% TO LOOP_ACCT%
		GOTO AddDebit IF ACCOUNT(LOOP%)::NUMBER = AD_TEMP::EXPACCT
	NEXT LOOP%

	LOOP%, LOOP_ACCT% = LOOP_ACCT% + 1%
	ACCOUNT(LOOP%)::NUMBER		= AD_TEMP::EXPACCT
	ACCOUNT(LOOP%)::DESCR		= EXPACCT_DESC$
	ACCOUNT(LOOP%)::CREDIT_CUR	= 0.0
	ACCOUNT(LOOP%)::DEBIT_CUR	= 0.0
	ACCOUNT(LOOP%)::CREDIT_CTD	= 0.0
	ACCOUNT(LOOP%)::DEBIT_CTD	= 0.0

 AddDebit:
	ACCOUNT(LOOP%)::DEBIT_CUR = ACCOUNT(LOOP%)::DEBIT_CUR + &
		AD_CALCULATION::AMOUNT_CUR
	ACCOUNT(LOOP%)::DEBIT_CTD = ACCOUNT(LOOP%)::DEBIT_CTD + &
		AD_BALANCE::AMOUNT_CTD

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	GOSUB PrintSubtotal IF PRINT_SUBTOTAL%

	IF PRINT_TOTAL%
	THEN
		!
		! Print final total
		!
		!
		! Sort array
		!
		FOR LOOP% = 1% TO LOOP_ACCT% - 1%
			ACCOUNT(0%) = ACCOUNT(LOOP%)
			INDEX% = LOOP%
			FOR I% = LOOP% + 1% TO LOOP_ACCT%
				IF ACCOUNT(I%)::NUMBER<ACCOUNT(0%)::NUMBER
				THEN
					ACCOUNT(0%) = ACCOUNT(I%)
					INDEX% = I%
				END IF
			NEXT I%
			ACCOUNT(INDEX%) = ACCOUNT(LOOP%)
			ACCOUNT(LOOP%) = ACCOUNT(0%)
		NEXT LOOP%

		TITLE$(5%) = "AccountNumber      Description         " + &
			"                           DebitCTD    " + &
			" CreditCTD      DebitCUR     CreditCUR       Balance"
		TITLE$(6%) = "."
		TITLE$(7%) = ""

		ACCOUNT(0%)::NUMBER	= "Grand Total ::::::"
		ACCOUNT(0%)::DESCR	= STRING$(40%, A":"B)
		ACCOUNT(0%)::CREDIT_CUR	= 0.0
		ACCOUNT(0%)::DEBIT_CUR	= 0.0
		ACCOUNT(0%)::CREDIT_CTD	= 0.0
		ACCOUNT(0%)::DEBIT_CTD	= 0.0

		LIN% = 999%

		FOR LOOP% = 1% TO LOOP_ACCT%
			TEXT$ = ACCOUNT(LOOP%)::NUMBER + " " + &
				ACCOUNT(LOOP%)::DESCR + " " + &
				FORMAT$(ACCOUNT(LOOP%)::DEBIT_CTD, &
					"<%>##,###,###.##") + &
				FORMAT$(ACCOUNT(LOOP%)::CREDIT_CTD, &
					"<%>##,###,###.##") + &
				FORMAT$(ACCOUNT(LOOP%)::DEBIT_CUR, &
					"<%>##,###,###.##") + &
				FORMAT$(ACCOUNT(LOOP%)::CREDIT_CUR, &
					"<%>##,###,###.##") + &
				FORMAT$(ACCOUNT(LOOP%)::DEBIT_CUR + &
					ACCOUNT(LOOP%)::DEBIT_CTD + &
					ACCOUNT(LOOP%)::CREDIT_CUR + &
					ACCOUNT(LOOP%)::CREDIT_CTD, &
					"###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

			LIN% = 0%
			ACCOUNT(0%)::CREDIT_CTD=ACCOUNT(0%)::CREDIT_CTD + &
				ACCOUNT(LOOP%)::CREDIT_CTD
			ACCOUNT(0%)::DEBIT_CTD=ACCOUNT(0%)::DEBIT_CTD+ &
				ACCOUNT(LOOP%)::DEBIT_CTD
			ACCOUNT(0%)::CREDIT_CUR=ACCOUNT(0%)::CREDIT_CUR + &
				ACCOUNT(LOOP%)::CREDIT_CUR
			ACCOUNT(0%)::DEBIT_CUR=ACCOUNT(0%)::DEBIT_CUR+ &
				ACCOUNT(LOOP%)::DEBIT_CUR
		NEXT LOOP%

		TEXT$ = ACCOUNT(0%)::NUMBER + ":" + &
			ACCOUNT(0%)::DESCR + " " + &
			FORMAT$(ACCOUNT(0%)::DEBIT_CTD, "<%>##,###,###.##") + &
			FORMAT$(ACCOUNT(0%)::CREDIT_CTD, "<%>##,###,###.##") + &
			FORMAT$(ACCOUNT(0%)::DEBIT_CUR, "<%>##,###,###.##") + &
			FORMAT$(ACCOUNT(0%)::CREDIT_CUR, "<%>##,###,###.##") + &
			FORMAT$(ACCOUNT(0%)::DEBIT_CUR + &
				ACCOUNT(0%)::DEBIT_CTD + &
				ACCOUNT(0%)::CREDIT_CUR + &
				ACCOUNT(0%)::CREDIT_CTD, "###,###,###.##")
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

 PrintSubtotal:
18000	!
	! Print subtotal
	!
	TEXT$ = "Total      " + &
		FORMAT$(SUBTOTAL_COST, "########.##") + " " + &
		FORMAT$(SUBTOTAL_SALVAGE, "######.##") + " " + &
		FORMAT$(SUBTOTAL_BONUS, "######.##") + " " + &
		FORMAT$(SUBTOTAL_ITC, "######.##") + " " + &
		FORMAT$(SUBTOTAL_ITCREDUCE, "######.##") + " " + &
		STRING$(11%, A":"B) + &
		FORMAT$(SUBTOTAL_ADJUST_BASIS, "########.##") + " " + &
		"       " + &
		FORMAT$(SUBTOTAL_AMOUNT_CTD, "########.##") + " " + &
		FORMAT$(SUBTOTAL_AMOUNT_CUR, "########.##") + " " + &
		FORMAT$(SUBTOTAL_AMOUNT_CUR+SUBTOTAL_AMOUNT_CTD, &
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

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
