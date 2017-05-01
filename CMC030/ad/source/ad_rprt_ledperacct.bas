1	%TITLE "Period Ledger by General Ledger Account"
	%SBTTL "AD_RPRT_LEDPERACCT"
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
	! ID:AD022
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Period Ledger by General Ledger Account\* prints
	!	a report containing the calculated depreciation for the current period. All
	!	information is retrieved from the Archive file and is printed in the order
	!	of the General Ledger Account Numbers. The following fields are included in
	!	this report:
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
	!	Class
	!	.le
	!	Method
	!	.le
	!	Recovery Period
	!	.le
	!	Initial Units
	!	.le
	!	Adjuster Basis Status
	!	.le
	!	Total Depreciated Amount
	!	.le
	!	Total Depreciated Units
	!	.els
	!
	! Index:
	!	.x Report>Period Ledger by General Ledger Number
	!	.x Period Ledger by General Ledger Number>Report
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_LEDPERACCT/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_LEDPERACCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_LEDPERACCT.OBJ;*
	!
	! AUTHOR:
	!
	!	09/27/88 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 source standards.
	!		Change STAT$ to STAT%
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
	!	10/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	07/30/97 - Kevin Handy
	!		Make XAGE parameter of READ_PERIOD an integer.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/24/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	02/13/2000 - Kevin Handy
	!		Add layout information
	!
	!	02/14/2000 - Kevin Handy
	!		Stuff titles into spreadsheet files
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART) GL_CHART_CDD GL_CHART

	MAP (AD_TEMP) &
		AD_TEMP.DEPACCT$ = 18%, &
		AD_TEMP.EXPACCT$ = 18%, &
		AD_TEMP.ASSET$ = 10%

	RECORD	ACCOUNT_RECORD
		STRING	NUMBER = 18%
		STRING	DESCR = 40%
		REAL	CREDIT
		REAL	DEBIT
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
	!	.x Account Numbers>Period Ledger by General Ledger Number
	!	.x Period Ledger by General Ledger Number>Account Numbers
	!
	!--

	ONLY.TOTAL$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

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
	!	.x Only Totals>Period Ledger by General Ledger Account
	!	.x Period Ledger by General Ledger Account>Only Totals
	!
	!--


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
		%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "AD_CALCULATION"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROL.OPN"
		GET #AD_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		AD_CONTROL::DEP_OBJECT = &
			STRING$(LEN(AD_CONTROL::DEP_OBJECT), A"?"B)

		CONTINUE 350 IF ERR = 5% OR ERR = 155%
		FILENAME$ = "AD_CONTROL"
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
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

370	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.OPN"
	USE
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

380	WHEN ERROR IN
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
		PERIOD.DESC$, "", "", "", 0%)

	IF AD_CONTROLOBJ::LASTDEP <= AD_CONTROLOBJ::LASTPER
	THEN
		PERIOD$ = "  TO  PERIOD  " + AD_CONTROLOBJ::LASTDEP + " " + &
			TRM$(PERIOD.DESC$)
	ELSE
		PERIOD$ = "  FROM  PERIOD  " + AD_CONTROLOBJ::LASTPER + &
			"  TO  PERIOD  " + AD_CONTROLOBJ::LASTDEP + " " + &
			TRM$(PERIOD.DESC$)
	END IF

390	CALL ASSG_CHANNEL(AD_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "AD_TEMP.TMP" FOR OUTPUT AS FILE #AD_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			BUFFER 32%, &
			MAP AD_TEMP, &
			PRIMARY KEY (AD_TEMP.DEPACCT$, AD_TEMP.EXPACCT$) DUPLICATES, &
			ACCESS MODIFY, &
			ALLOW NONE, &
			TEMPORARY
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
			KEY #0% EQ AD_35ASSET::LOCATION + AD_35ASSET::ASSET_TYPE, &
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

	AD_TEMP.DEPACCT$ = AD_ACCOUNT::DEP_ACCT
	AD_TEMP.EXPACCT$ = AD_ACCOUNT::EXP_ACCT
	AD_TEMP.ASSET$ = AD_35ASSET::ASSET_NUM

	GOTO GetNextTemp IF WLDCRD$ <> "" AND &
		COMP_STRING(EDIT$(AD_TEMP.DEPACCT$, -1%), WLDCRD$) = 0% AND &
		COMP_STRING(EDIT$(AD_TEMP.EXPACCT$, -1%), WLDCRD$) = 0%

430	PUT #AD_TEMP.CH%

	GOTO GetNextTemp

 ReportTitle:
	LAYOUT_1$ = "$Colon:5,$Title:132"
	LAYOUT_2$ = "$Account:19,$Description:59,VDebit:74,VCredit:132"
	LAYOUT_2A$ = "$Account:19,$Description:59,$Debit:74,$Credit:132"
	LAYOUT_3$ = "$Title:11,VCost:22,VSalvage:32,VBonus:42,VItc:52," + &
		"VItcReduce:63,$Colon:96,VAdjust:109,VCur:132"
	LAYOUT_3A$ = "$Title:11,$Cost:22,$Salvage:32,$Bonus:42,$Itc:52," + &
		"$ItcReduce:63,$Colon:96,$Adjust:109,$Cur:132"
	LAYOUT_4$ = "$Asset:11,$Description:52,$Location:57,$Type:63," + &
		"$Date:74,$Class:80,$Method:91,$Years:96,VBasis:110," + &
		"$Status:112,VCurrent:132"
	LAYOUT_4A$ = "$Asset:11,$Description:52,$Location:57,$Type:63," + &
		"$Date:74,$Class:80,$Method:91,$Years:96,$Basis:110," + &
		"$Status:112,$Current:132"

	!
	! Title
	!
	TITLE$(1%) = "PERIOD  LEDGER  BY  ACCOUNT  NUMBER"
	TITLE$(2%) = PERIOD$
	TITLE$(3%) = "Asset Depreciation System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	IF ONLY.TOTAL$<>"Y"
	THEN
		TITLE$(5%) = &
			"Asset#     Description                  " + &
			"            Loc  Tp    ServDate  " + &
			" Class Mthd  RecPeriod      AdjBasis St    " + &
			"TotDepAmt"
		TITLE$(6%) = "                  Cost   Salvage   Sect179    ITCAmt " + &
			"ITCReduce RetDate                InitUnits" + &
			"                   TotDepUnits"

		TITLE$(7%) = "."

		IF UTL_REPORTX::PRINTTO = OUTP_TO2020 OR &
			UTL_REPORTX::PRINTTO = OUTP_TOPL
		THEN
			CALL OUTP_LINE(LAYOUT_4A$, UTL_REPORTX, TITLE$(), &
				TITLE$(5%), 0%)
			CALL OUTP_LINE(LAYOUT_3A$, UTL_REPORTX, TITLE$(), &
				TITLE$(6%), 0%)

		END IF
	ELSE
		TITLE$(5%) = "                  Cost   Salvage   Sect179    ITCAmt " + &
			"ITCReduce                                 " + &
			"      AdjBasis       TotDepAmt"

		TITLE$(6%) = "."

		IF UTL_REPORTX::PRINTTO = OUTP_TO2020 OR &
			UTL_REPORTX::PRINTTO = OUTP_TOPL
		THEN
			CALL OUTP_LINE(LAYOUT_4A$, UTL_REPORTX, TITLE$(), &
				TITLE$(5%), 0%)
		END IF
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

17100	WHEN ERROR IN
		GET #AD_CALCULATION.CH%, &
			KEY #0% EQ AD_TEMP.ASSET$ + AD_CONTROL::DEP_OBJECT, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_CALCULATION"
		CONTINUE HelpError
	END WHEN

17120	WHEN ERROR IN
		GET #AD_35ASSET.CH%, KEY #0% EQ AD_TEMP.ASSET$, REGARDLESS
	USE
		AD_35ASSET::DESCRIPTION = &
			STRING$(LEN(AD_35ASSET::DESCRIPTION), A"?"B)
		AD_35ASSET::LOCATION = &
			STRING$(LEN(AD_35ASSET::LOCATION), A"?"B)
		AD_35ASSET::ASSET_TYPE = &
			STRING$(LEN(AD_35ASSET::ASSET_TYPE), A"?"B)
		AD_35ASSET::SERVDATE = &
			STRING$(LEN(AD_35ASSET::SERVDATE), A"?"B)
		AD_35ASSET::COST	= 0.0
		AD_35ASSET::SALVAGE	= 0.0
		AD_35ASSET::ITC		= 0.0
		AD_35ASSET::ITCREDUCE	= 0.0
		AD_35ASSET::UNITS	= 0.0

		CONTINUE 17130 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

17130	GOTO TestType IF TEST_STRING$ = AD_TEMP.DEPACCT$ + AD_TEMP.EXPACCT$

	GOSUB 18000 IF TEST_STRING$ <> ""

	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ AD_TEMP.DEPACCT$, REGARDLESS
	USE
		GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), A"?"B)

		CONTINUE 17135 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

17135	TEXT$ = ":::: Credit - " + &
		TRM$(AD_TEMP.DEPACCT$) + " " + &
		TRM$(GL_CHART::DESCR) + " "
	TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B)
	CALL OUTP_LINE(LAYOUT_1$, UTL_REPORTX, TITLE$(), TEXT$, 4%)

	DEPACCT.DESC$ = GL_CHART::DESCR

17140	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ AD_TEMP.EXPACCT$, REGARDLESS
	USE
		GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), A"?"B)

		CONTINUE 17145 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

17145	TEXT$ = "     Debit  - " + &
		TRM$(AD_TEMP.EXPACCT$) + " " + &
		TRM$(GL_CHART::DESCR)
	CALL OUTP_LINE(LAYOUT_1$, UTL_REPORTX, TITLE$(), TEXT$, 4%)

	EXPACCT.DESC$ = GL_CHART::DESCR

 TestType:

	TEST_STRING$ = AD_TEMP.DEPACCT$ + AD_TEMP.EXPACCT$

	ADJUST_BASIS = AD_35ASSET::COST
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
		GET #AD_DEPRECIATION.CH%, KEY #0% EQ AD_35ASSET::ASSET_NUM + &
			AD_CONTROL::DEP_OBJECT, REGARDLESS
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
	! Read Ledger File
	!
	TEXT$ = TEXT$ + " " + &
		AD_DEPRECIATION::DEPCLASS + "  " + &
		AD_DEPCLASS::DEPMETHOD + "       " + &
		AD_DEPCLASS::YEARS + " " + &
		FORMAT$(ADJUST_BASIS, "##,###,###.##")

	SUBTOTAL_ADJUST_BASIS = SUBTOTAL_ADJUST_BASIS + ADJUST_BASIS

	TEXT$ = TEXT$ + " " + &
		AD_CALCULATION::DEP_STATUS + " " + &
		FORMAT$(AD_CALCULATION::AMOUNT_CUR, "##,###,###.##")

	TEXT2$ = TEXT2$ + SPACE$(17%) + &
		FORMAT$(AD_CALCULATION::UNIT_CUR, "<%>#,###,###.##")

	SUBTOTAL_AMOUNT_CUR = SUBTOTAL_AMOUNT_CUR + AD_CALCULATION::AMOUNT_CUR

17300	!
	! Print out
	!
	IF ONLY.TOTAL$ <> "Y"
	THEN
		CALL OUTP_LINE(LAYOUT_4$, UTL_REPORTX, TITLE$(), TEXT$, 1%)
		CALL OUTP_LINE(LAYOUT_3$, UTL_REPORTX, TITLE$(), TEXT2$, 0%)
	END IF
	PRINT_SUBTOTAL% = -1%

	FOR LOOP% = 1% TO LOOP_ACCT%
		GOTO AddCredit IF ACCOUNT(LOOP%)::NUMBER = AD_TEMP.DEPACCT$
	NEXT LOOP%

	LOOP%,LOOP_ACCT% = LOOP_ACCT% + 1%
	ACCOUNT(LOOP%)::NUMBER	= AD_TEMP.DEPACCT$
	ACCOUNT(LOOP%)::DESCR	= DEPACCT.DESC$
	ACCOUNT(LOOP%)::CREDIT	= 0.0
	ACCOUNT(LOOP%)::DEBIT	= 0.0

 AddCredit:
	ACCOUNT(LOOP%)::CREDIT = ACCOUNT(LOOP%)::CREDIT - &
		AD_CALCULATION::AMOUNT_CUR

	FOR LOOP% = 1% TO LOOP_ACCT%
		GOTO AddDebit IF ACCOUNT(LOOP%)::NUMBER = AD_TEMP.EXPACCT$
	NEXT LOOP%

	LOOP%,LOOP_ACCT% = LOOP_ACCT% + 1%
	ACCOUNT(LOOP%)::NUMBER	= AD_TEMP.EXPACCT$
	ACCOUNT(LOOP%)::DESCR	= EXPACCT.DESC$
	ACCOUNT(LOOP%)::CREDIT	= 0.0
	ACCOUNT(LOOP%)::DEBIT	= 0.0

 AddDebit:
	ACCOUNT(LOOP%)::DEBIT = ACCOUNT(LOOP%)::DEBIT + &
		AD_CALCULATION::AMOUNT_CUR


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
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)

		!
		! Sort array
		!
		FOR LOOP% = 1% TO LOOP_ACCT% - 1%
			ACCOUNT(0%) = ACCOUNT(LOOP%)
			INDEX% = LOOP%
			FOR I% = LOOP% + 1% TO LOOP_ACCT%
				IF ACCOUNT(I%)::NUMBER < ACCOUNT(0%)::NUMBER
				THEN
					ACCOUNT(0%) = ACCOUNT(I%)
					INDEX% = I%
				END IF
			NEXT I%
			ACCOUNT(INDEX%) = ACCOUNT(LOOP%)
			ACCOUNT(LOOP%) = ACCOUNT(0%)
		NEXT LOOP%

		TEXT$ = "AccountNumber      Description         " + &
			"                             Debit      " + &
			" Credit"
		CALL OUTP_LINE(LAYOUT_2A$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		ACCOUNT(0%)::NUMBER	= "Grand Total ::::::"
		ACCOUNT(0%)::DESCR	= STRING$(40%, A":"B)
		ACCOUNT(0%)::CREDIT	= 0.0
		ACCOUNT(0%)::DEBIT	= 0.0

		FOR LOOP% = 1% TO LOOP_ACCT%
			TEXT$ = ACCOUNT(LOOP%)::NUMBER + " " + &
				ACCOUNT(LOOP%)::DESCR + " " + &
				FORMAT$(ACCOUNT(LOOP%)::DEBIT, &
				"<%>#,###,###.##") + &
				FORMAT$(ACCOUNT(LOOP%)::CREDIT, &
				"<%>#,###,###.##")
			CALL OUTP_LINE(LAYOUT_2$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

			ACCOUNT(0%)::CREDIT = ACCOUNT(0%)::CREDIT + &
				ACCOUNT(LOOP%)::CREDIT
			ACCOUNT(0%)::DEBIT = ACCOUNT(0%)::DEBIT + &
				ACCOUNT(LOOP%)::DEBIT
		NEXT LOOP%

		TEXT$ = ACCOUNT(0%)::NUMBER + ":" + &
			ACCOUNT(0%)::DESCR + " " + &
			FORMAT$(ACCOUNT(0%)::DEBIT, "<%>#,###,###.##") + &
			FORMAT$(ACCOUNT(0%)::CREDIT, "<%>#,###,###.##") + " "
		TEXT$ = TEXT$ + STRING$(132% - LEN(TEXT$), A":"B)
		CALL OUTP_LINE(LAYOUT_2$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
		FORMAT$(SUBTOTAL_AMOUNT_CUR, "##,###,###.##")

	CALL OUTP_LINE(LAYOUT_3$, UTL_REPORTX, TITLE$(), TEXT$, -2%)
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
