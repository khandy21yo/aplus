1	%TITLE "Fixed Asset by General Ledger Account"
	%SBTTL "AD_RPRT_FIXACCT"
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
	! ID:AD029
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Fixed Asset by General Ledger Account\* prints
	!	a report containing the year to date depreciation not including the current
	!	period calculations. The report is printed in Account Number order and may
	!	include all assets or just assets completing specified conditions.
	!	Included in this report are the following fields:
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
	!	.lm -5
	!
	! Index:
	!	.x Report>Fixed Asset by GL Account
	!	.x Fixed Asset by GL Account>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_FIXACCT/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_FIXACCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_FIXACCT.OBJ;*
	!
	! AUTHOR:
	!
	!	09/26/88 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/18/92 - Frank F. Starman
	!		Trap error 155 on line 17100 to 17120
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update source code to V3.6 standards.
	!		Change STAT$ to STAT% in ASSG_CHANNEL call.
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change string(...,ascii(" ")) to "" in several
	!		places.
	!
	!	01/29/96 - Kevin Handy
	!		Change STRING(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	10/03/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/20/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROL.HB"
	MAP (AD_CONTROL) AD_CONTROL_CDD AD_CONTROL

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

	RECORD ACCOUNT_RECORD
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
	!	^*(01) Account (Wildcard)\*
	!	.b
	!	.lm +5
	!	The ^*Account (Wildcard)\* field causes the report to
	!	print only selected General Ledger Account Numbers.
	!	.b
	!	Wildcarding techniques may be used.
	!	.lm -5
	!
	! Index:
	!	.x Account>Fixed Assets by General Ledger Account
	!	.x Fixed Assets by General Ledger Account>Account
	!
	!--

	ONLY_TOTAL$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) Only Totals (Y,N)\*
	!	.b
	!	.lm +5
	!	The ^*Only Totals\* field prints the entire report by
	!	entering ^*N\* or print only report totals by entering a ^*Y\*.
	!	.lm -5
	!
	! Index:
	!	.x Only Totals>Fixed Asset by General Ledger Account
	!	.x Fixed Asset by General Ledger Account>Only Totals
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
		%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "AD_BALANCE"
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

380	CALL ASSG_CHANNEL(AD_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "AD_TEMP.TMP" FOR OUTPUT &
			AS FILE #AD_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AD_TEMP, &
			PRIMARY KEY (AD_TEMP.DEPACCT$, AD_TEMP.EXPACCT$) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
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
	!
	! Title
	!
	TITLE$(1%) = "FIXED  ASSET  BY  ACCOUNT  NUMBER"
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	IF ONLY_TOTAL$<>"Y"
	THEN
		TITLE$(4%) = &
			"Asset#     Description                  " + &
			"            Loc  Tp    ServDate  " + &
			" Class Mthd  RecPeriod      AdjBasis St    " + &
			"TotDepAmt Period"
		TITLE$(5%) = "                  Cost   Salvage   Sect179    ITCAmt " + &
			"ITCReduce RetDate                InitUnits" + &
			"                   TotDepUnits"

		TITLE$(6%) = "."
	ELSE
		TITLE$(4%) = "                  Cost   Salvage   Sect179    ITCAmt " + &
			"ITCReduce                                 " + &
			"      AdjBasis       TotDepAmt"

		TITLE$(5%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #AD_TEMP.CH%
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
		GET #AD_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_TEMP"
		CONTINUE HelpError
	END WHEN

	AD_BALANCE::DEP_STATUS = "A"
	AD_BALANCE::AMOUNT_CTD = 0.0
	AD_BALANCE::LASTPER = ""
	AD_BALANCE::UNIT_CTD = 0.0

17100	WHEN ERROR IN
		GET #AD_BALANCE.CH%, KEY #0% EQ AD_TEMP.ASSET$ + &
			AD_CONTROL::DEP_OBJECT, &
			REGARDLESS
	USE
		CONTINUE 17120 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_BALANCE"
		CONTINUE HelpError
	END WHEN

17120	AD_35ASSET::DESCRIPTION = &
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
	RET_DATE$ = SPACE$(LEN(AD_35ASSET::RET_DATE) + 2%)

	GET #AD_35ASSET.CH%, KEY #0% EQ AD_TEMP.ASSET$, REGARDLESS

	IF EDIT$(AD_35ASSET::RET_DATE, -1%) <> ""
	THEN
		RET_DATE$ = PRNT_DATE(AD_35ASSET::RET_DATE, 8%)
	END IF

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
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)

	DEPACCT_DESC$ = GL_CHART::DESCR

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
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)

	EXPACCT_DESC$ = GL_CHART::DESCR

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
	SUBTOTAL_ITC = SUBTOTAL_ITC+ AD_35ASSET::ITC
	SUBTOTAL_ITCREDUCE = SUBTOTAL_ITCREDUCE + AD_35ASSET::ITCREDUCE


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
			KEY #0% EQ AD_35ASSET::ASSET_NUM + &
			AD_CONTROL::DEP_OBJECT, &
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
	! Read Balance File
	!
	TEXT$ = TEXT$ + " " + &
		AD_DEPRECIATION::DEPCLASS + "  " + &
		AD_DEPCLASS::DEPMETHOD + "       " + &
		AD_DEPCLASS::YEARS + " " + &
		FORMAT$(ADJUST_BASIS, "##,###,###.##")

	SUBTOTAL_ADJUST_BASIS = SUBTOTAL_ADJUST_BASIS + ADJUST_BASIS

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

	FOR LOOP% = 1% TO LOOP_ACCT%
		GOTO AddCredit IF ACCOUNT(LOOP%)::NUMBER = AD_TEMP.DEPACCT$
	NEXT LOOP%

	LOOP%, LOOP_ACCT% = LOOP_ACCT% + 1%
	ACCOUNT(LOOP%)::NUMBER	= AD_TEMP.DEPACCT$
	ACCOUNT(LOOP%)::DESCR	= DEPACCT_DESC$
	ACCOUNT(LOOP%)::CREDIT	= 0.0
	ACCOUNT(LOOP%)::DEBIT	= 0.0

 AddCredit:
	ACCOUNT(LOOP%)::CREDIT	= ACCOUNT(LOOP%)::CREDIT - &
		AD_BALANCE::AMOUNT_CTD

	FOR LOOP% = 1% TO LOOP_ACCT%
		GOTO AddDebit IF ACCOUNT(LOOP%)::NUMBER = AD_TEMP.EXPACCT$
	NEXT LOOP%

	LOOP%,LOOP_ACCT% = LOOP_ACCT% + 1%
	ACCOUNT(LOOP%)::NUMBER	= AD_TEMP.EXPACCT$
	ACCOUNT(LOOP%)::DESCR	= EXPACCT_DESC$
	ACCOUNT(LOOP%)::CREDIT	= 0.0
	ACCOUNT(LOOP%)::DEBIT	= 0.0

 AddDebit:
	ACCOUNT(LOOP%)::DEBIT	= ACCOUNT(LOOP%)::DEBIT + AD_BALANCE::AMOUNT_CTD


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
			"                            Credit      " + &
			"  Debit"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		ACCOUNT(0%)::NUMBER	= "Grand Total ::::::"
		ACCOUNT(0%)::DESCR	= STRING$(40%, A":"B)
		ACCOUNT(0%)::CREDIT	= 0.0
		ACCOUNT(0%)::DEBIT	= 0.0

		FOR LOOP% = 1% TO LOOP_ACCT%
			TEXT$ = ACCOUNT(LOOP%)::NUMBER + " " + &
				ACCOUNT(LOOP%)::DESCR + " " + &
				FORMAT$(ACCOUNT(LOOP%)::CREDIT, "<%>#,###,###.##") + &
				FORMAT$(ACCOUNT(LOOP%)::DEBIT, "<%>#,###,###.##")
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			ACCOUNT(0%)::CREDIT = ACCOUNT(0%)::CREDIT + &
				ACCOUNT(LOOP%)::CREDIT
			ACCOUNT(0%)::DEBIT = ACCOUNT(0%)::DEBIT + &
				ACCOUNT(LOOP%)::DEBIT
		NEXT LOOP%

		TEXT$ = ACCOUNT(0%)::NUMBER + ":" + &
			ACCOUNT(0%)::DESCR + " " + &
			FORMAT$(ACCOUNT(0%)::CREDIT, "<%>#,###,###.##") + &
			FORMAT$(ACCOUNT(0%)::DEBIT, "<%>#,###,###.##") + " "
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
