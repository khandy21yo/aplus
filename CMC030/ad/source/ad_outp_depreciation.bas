1	%TITLE "Projected Depreciation"
	%SBTTL "AD_OUTP_DEPRECIATION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_OUTP_DEPRECIATION(AD_35ASSET_CDD AD_35ASSET)

	!
	! COPYRIGHT (C) 1986, 1988 BY
	!
	! Computer Management Center, Inc.
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
	! ID:AD050
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints Projected depreciation.
	!	.lm -5
	!
	! Index:
	!	.x Projected Deprecation
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_OUTP_DEPRECIATION/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AD_OUTP_DEPRECIATION
	!	$ DELETE AD_OUTP_DEPRECIATION.OBJ;*
	!
	! Author:
	!
	!	09/15/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to v3.6 coding standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	09/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	07/30/97 - Kevin Handy
	!		Make XAGE parameter of READ_PERIOD an integer.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/27/2000 - Kevin Handy
	!		Use A"X"B instead of ASCII("X")
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS)	AD_DEPCLASS_CDD	AD_DEPCLASS

	%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.HB"
	MAP (AD_METHOD)	AD_METHOD_CDD	AD_METHOD

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLE.HB"
	MAP (AD_TABLE)	AD_TABLE_CDD	AD_TABLE

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLEONE.HB"
	MAP (AD_TABLEONE)	AD_TABLEONE_CDD	AD_TABLEONE

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLETWO.HB"
	MAP (AD_TABLETWO)	AD_TABLETWO_CDD	AD_TABLETWO

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILING.HB"
	MAP (AD_CEILING)	AD_CEILING_CDD	AD_CEILING

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGONE.HB"
	MAP (AD_CEILINGONE)	AD_CEILINGONE_CDD	AD_CEILINGONE

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGTWO.HB"
	MAP (AD_CEILINGTWO)	AD_CEILINGTWO_CDD	AD_CEILINGTWO

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_INITFORM
	EXTERNAL REAL	FUNCTION AD_FUNC_CONVENTION
	EXTERNAL REAL	FUNCTION AD_FUNC_METHOD

	%PAGE

	ON ERROR GOTO 19000

	AD_OUTP_DEPRECIATION = 0%
	REPORT$ = "AD050"

400	!

 SetScreen:
	!******************************************************************
	! Ask user to change settings
	!******************************************************************
	GOTO ExitFunction &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!
	! Set up from user input
	!
	ERA$(1%) = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Era Code 1\*
	!	.b
	!	.lm +5
	!	The ^*Era Code\* consists of an accounting time frame which contains a group
	!	of accounting periods and figures depreciation.
	!	The use of Era Codes allows for comparison between types of depreciation.
	!	.lm -5
	!
	! Index:
	!	.x Era Code>Projected Depreciation
	!	.x Projected Depreciation>Era Code
	!
	!--
	ERA$(2%) = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) Era Code 2\*
	!	.B
	!	.LM +5
	!	The ^*Era Code\* consists of an accounting time frame which contains a group
	!	of accounting periods and figures depreciation.
	!	The use of Era Codes allows for comparison between types of depreciation.
	!	.LM -5
	!
	! Index:
	!	.x Era Code>Projected Depreciation
	!	.x Projected Depreciation>Era Code
	!
	!--
	ERA$(3%) = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Era Code 3\*
	!	.B
	!	.LM +5
	!	The ^*Era Code\* consists of an accounting time frame which contains a group
	!	of accounting periods and figures depreciation.
	!	The use of Era Codes allows for comparison between types of depreciation.
	!	.LM -5
	!
	! Index:
	!	.x Era Code>Projected Depreciation
	!	.x Projected Depreciation>Era Code
	!
	!--
	ERA$(4%) = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Era Code 4\*
	!	.B
	!	.LM +5
	!	The ^*Era Code\* consists of an accounting time frame which contains a group
	!	of accounting periods and figures depreciation.
	!	The use of Era Codes allows for comparison between types of depreciation.
	!	.LM -5
	!
	! Index:
	!	.x Era Code>Projected Depreciation
	!	.x Projected Depreciation>Era Code
	!
	!--
	DEPCLASS$(1%) = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Depreciation Class 1\*
	!	.B
	!	.LM +5
	!	The ^*Depreciation Class\* field contains a summary of information on how
	!	to depreciate the desired object in a particular manner.
	!	.LM -5
	!
	! Index:
	!	.x Deprecation Class>Projected Depreciation
	!	.x Projected Deprecation>Depreciation Class
	!
	!--
	DEPCLASS$(2%) = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) Depreciation Class 2\*
	!	.B
	!	.LM +5
	!	The ^*Depreciation Class\* field contains a summary of information on how
	!	to depreciate the desired object in a particular manner.
	!	.LM -5
	!
	! Index:
	!	.x Depreciation Class>Projected Depreciation
	!	.x Projected Depreciation>Depreciation Class
	!
	!--
	DEPCLASS$(3%) = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	^*(08) Depreciation Class 3\*
	!	.B
	!	.LM +5
	!	The ^*Depreciation Class\* field contains a summary of information on how
	!	to depreciate the desired object in a particular manner.
	!	.LM -5
	!
	! Index:
	!	.x Depreciation Class>Projected Depreciation
	!	.x Projected Depreciation>Depreciation Class
	!
	!--
	DEPCLASS$(4%) = EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)

	!++
	! Abstract:FLD09
	!	^*(09) Depreciation Class 4\*
	!	.B
	!	.LM +5
	!	The ^*Depreciation Class\* field contains a summary of information on how
	!	to depreciate the desired object in a particular manner.
	!	.LM -5
	!
	! Index:
	!
	!--
	DISP_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(9%), 132%))

	!++
	! Abstract:FLD10
	!	^*(10) Disposal Date\*
	!	.B
	!	.LM +5
	!	The ^*Disposal Date\* enters the date when all depreciation
	!	on the object will be complete.
	!	.B
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Disposal Date>Projected Depreciation
	!	.x Projected Depreciation>Disposal Date
	!
	!--

510	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.OPN"
	USE
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

520	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_TABLEONE.OPN"
	USE
		CONTINUE 530 IF ERR = 5%
		FILENAME$ = "AD_TABLEONE"
		CONTINUE HelpError
	END WHEN

530	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_TABLETWO.OPN"
	USE
		CONTINUE 540 IF ERR = 5%
		FILENAME$ = "AD_TABLETWO"
		CONTINUE HelpError
	END WHEN

540	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_TABLE.OPN"
	USE
		CONTINUE 550 IF ERR = 5%
		FILENAME$ = "AD_TABLE"
		CONTINUE HelpError
	END WHEN

550	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILING.OPN"
	USE
		CONTINUE 560 IF ERR = 5%
		FILENAME$ = "AD_CEILING"
		CONTINUE HelpError
	END WHEN

560	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGONE.OPN"
	USE
		CONTINUE 570 IF ERR = 5%
		FILENAME$ = "AD_CEILINGONE"
		CONTINUE HelpError
	END WHEN

570	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGTWO.OPN"
	USE
		CONTINUE 580 IF ERR = 5%
		FILENAME$ = "AD_CEILINGTWO"
		CONTINUE HelpError
	END WHEN

580	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.OPN"
	USE
		FILENAME$ = "AD_METHOD"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PROJECTED  ASSET  DEPRECIATION "
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	TEXT$ = "     Asset number       " + AD_35ASSET::ASSET_NUM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Description        " + AD_35ASSET::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "     Asset Type         " + AD_35ASSET::ASSET_TYPE

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Location           " + AD_35ASSET::LOCATION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Department         " + AD_35ASSET::DEPT_NUM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Service Date       " + PRNT_DATE(AD_35ASSET::SERVDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Basis              " + &
		FORMAT$(AD_35ASSET::COST, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Salvage            " + &
		FORMAT$(AD_35ASSET::SALVAGE, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     Section 179        " + &
		FORMAT$(AD_35ASSET::BONUS, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "     ITC                " + &
		FORMAT$(AD_35ASSET::ITC, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT1$ = "Class Description                              " + &
			"PT Meth OptTable RecPer " + &
			"FYConv DYConv CeilTable SalvF BonusF ITCF"

	TEXT2$ = STRING$(132%, A"."B)

	FOR LOOP% = 1% TO 4%
		GOTO NextLoop IF DEPCLASS$(LOOP%) = ""
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT2$, 2%)

17200		WHEN ERROR IN
			GET #AD_DEPCLASS.CH%, &
				KEY #0% EQ DEPCLASS$(LOOP%), &
			REGARDLESS
		USE
			CONTINUE 17270 IF ERR=155%
			FILENAME$ = "AD_DEP_CLASS"
			CONTINUE HelpError
		END WHEN

		REC_PER = VAL(AD_DEPCLASS::YEARS)

		GOTO ExitFunction IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 2%)

		TEXT$ = AD_DEPCLASS::DEPCLASS + "  " + &
			AD_DEPCLASS::DESCRIPTION + " " + &
			AD_DEPCLASS::PROPTYPE + " " + &
			AD_DEPCLASS::DEPMETHOD + " " + &
			AD_DEPCLASS::OPTTABLE + "   " + &
			AD_DEPCLASS::YEARS + "   " + &
			AD_DEPCLASS::FYCONV + "     " + &
			AD_DEPCLASS::DYCONV + "     " + &
			AD_DEPCLASS::CEILTABLE + "    " + &
			AD_DEPCLASS::SALVFACTOR + "     " + &
			AD_DEPCLASS::BONUSFACTOR + "      " + &
			AD_DEPCLASS::ITCFACTOR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		BASIS = AD_35ASSET::COST
		SALVAGE = AD_35ASSET::SALVAGE
		BONUS = AD_35ASSET::BONUS
		!ITC = AD_35ASSET::ITCBONUS
		SERV_DATE$ = AD_35ASSET::SERVDATE
		SERV_MONTH% = VAL(MID(AD_35ASSET::SERVDATE, 5%, 2%))

		!
		! Adjust basis if necessary
		!
		BASIS = BASIS - SALVAGE IF AD_DEPCLASS::SALVFACTOR = "Y"
		BASIS = BASIS - BONUS   IF AD_DEPCLASS::BONUSFACTOR = "Y"
		BASIS = BASIS - ITC IF AD_DEPCLASS::ITCFACTOR = "Y"

		AMOUNT_CTD = 0.0
		DEP_STATUS$ = "A"
		SEQ_NUMBER% = 0%

		IF ERA$(LOOP%) = ""
		THEN
			FIRST_MONTH% = 1%
			SERV_YEAR% = VAL(LEFT(AD_35ASSET::SERVDATE, 4%))
			D_YEAR% = 0%
		ELSE
			DEP_PERIOD$ = ""

			!
			! Find first period
			!
			P_STATUS% = READ_PERIOD("FIND", ERA$(LOOP%), &
				DEP_PERIOD$, &
				PERIOD_DESC$, "", BEG_DATE$, DEP_DATE$, 1%)

			WHILE DEP_DATE$ < SERV_DATE$ AND P_STATUS% = 0%
				P_STATUS% = READ_PERIOD("FIND", ERA$(LOOP%), &
					DEP_PERIOD$, PERIOD_DESC$, "", &
					BEG_DATE$, DEP_DATE$, 1%)
			NEXT

			IF P_STATUS%
			THEN
				TEXT$ = "... Can't project depreciation based " + &
					"on Era Code " + ERA$(LOOP%)
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					TEXT$, 0%)
				GOTO NextLoop
			ELSE
				V% = READ_PERIOD("READ", ERA$(LOOP%), &
					LEFT(DEP_PERIOD$, 4%), "", "", &
					FIRST_DATE$, "", 0%)
				FIRST_MONTH% = VAL(MID(FIRST_DATE$, 5%, 2%))
				FIRST_MONTH% = FIRST_MONTH% + 1% &
					IF VAL(MID(FIRST_DATE$, 7%, 2%)) > 15%
			END IF

		END IF

17250		WHEN ERROR IN
			GET #AD_METHOD.CH%, &
				KEY #0% EQ AD_DEPCLASS::DEPMETHOD, &
				REGARDLESS
		USE
			CONTINUE 17280 IF ERR = 155%
			FILENAME$ = "AD_METHOD"
			CONTINUE HelpError
		END WHEN

 DepYear:
		IF ERA$(LOOP%) = ""
		THEN
			BEG_DATE$ = FORMAT$(SERV_YEAR% + D_YEAR%, "####") + &
				"0101"
			DEP_DATE$ = FORMAT$(SERV_YEAR% + D_YEAR%, "####") + &
				"1231"
			DEP_PERIOD$ = LEFT(DEP_DATE$, 6%)
			PERIOD_DESC$ = "Calendar Year " + &
				FORMAT$(SERV_YEAR% + D_YEAR%, "####  ")
			D_YEAR% = D_YEAR% + 1%
		ELSE
			IF LEFT$(DEP_PERIOD$, 4%) <> LAST_YEAR$ AND &
				LAST_YEAR$ <> ""
			THEN
				FIRST_MONTH% = VAL(MID(FIRST_DATE$, 5%, 2%))
				FIRST_MONTH% = FIRST_MONTH% + 1% &
					IF VAL(MID(FIRST_DATE$, 7%, 2%)) > 15%
			END IF
			LAST_YEAR$ = LEFT$(DEP_PERIOD$, 4%)
		END IF

		!
		! First year month
		!
		INIT_MONTH% = FIRST_MONTH% - SERV_MONTH%
		INIT_MONTH% = 12% + INIT_MONTH% IF INIT_MONTH% <= 0%

		DEP_MONTH% = VAL(MID(DEP_DATE$, 5%, 2%))
		DEP_MONTH% = DEP_MONTH% - 1% IF VAL(MID(DEP_DATE$, 7%, 2%)) <= 15%
		DEP_MONTH% = DEP_MONTH% - FIRST_MONTH% + 1%
		DEP_MONTH% = DEP_MONTH% + 12% IF DEP_MONTH% <= 0%

		RATE_ENDYEAR, RATE_PRIOR = 0.0

		SELECT AD_METHOD::CALCULATION

		!
		! Method based on Unit-of-Production depreciation method
		!
		CASE "12"
			TEXT$ = "... Can't project depreciation based on " + &
				"Unit-of-Production method"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO NextLoop

		CASE ELSE
			!
			! What depreciation year
			!
			DEP_YEAR% = 1%
			DEP_MON% = DATE_MONCODE(DEP_DATE$)
			DEP_MON% = DEP_MON% - 1% &
				IF VAL(MID(DEP_DATE$, 7%, 2%)) <= 15%
			TEST_CODE% = DATE_MONCODE(AD_35ASSET::SERVDATE) + 12%
			WHILE TEST_CODE% <= DEP_MON%
				! AND DEP_YEAR% < REC_PER + 1%

				DEP_YEAR% = DEP_YEAR% + 1%
				TEST_CODE% = TEST_CODE% + 12%
			NEXT

			INDEX% = 13% - INIT_MONTH%
			DEP_YEAR% = DEP_YEAR% + 1% &
				IF DEP_MONTH%<INDEX%

			IF AD_METHOD::CALCULATION = "01"
			THEN
				!
				! Method based on a optional table
				!
				TABLE% = 0%
				GOSUB 18100
				IF TABLE%
				THEN
					TEXT$ = "... Can't find depreciation " + &
						"optional table"
					CALL OUTP_LINE("", UTL_REPORTX, &
						TITLE$(), TEXT$, 0%)
					GOTO NextLoop
				END IF

			ELSE
				!
				! Service date is first or second half of the month ?
				!
				SERV_HALF% = 1%
				SERV_HALF% = 2% &
					IF VAL(MID(SERV_DATE$, 7%, 2%)) > 15%

				!
				! Go for convention coefficient
				!
				CONV = AD_FUNC_CONVENTION(AD_DEPCLASS::FYCONV, &
					2% * INIT_MONTH% - SERV_HALF%)

				IF DEP_YEAR% > 1%
				THEN
					RATE_PRIOR = AD_FUNC_METHOD( &
						AD_METHOD::CALCULATION, &
						AD_DEPCLASS::YEARS, DEP_YEAR% * 1.0-1.0,CONV)
				END IF

				RATE_ENDYEAR = AD_FUNC_METHOD( &
					AD_METHOD::CALCULATION, &
					AD_DEPCLASS::YEARS, DEP_YEAR% * 1.0, CONV)
			END IF

		END SELECT

		IF BEG_DATE$ <= DISP_DATE$ AND DISP_DATE$ <= DEP_DATE$
		THEN
			DISP_MONTH% = VAL(MID(DISP_DATE$, 5%, 2%))
			!
			! Disposal date is first or second half of the month ?
			!
			DISP_HALF% = 1%
			DISP_HALF% = 2% &
				IF VAL(MID(DISP_DATE$, 7%, 2%)) > 15%
			!
			! Go for retired convention coefficient
			!
			DCONV = AD_FUNC_CONVENTION(AD_DEPCLASS::DYCONV, &
				2% * DISP_MONTH% - DISP_HALF%)
			DEP_STATUS$ = "R"
		ELSE
			GOTO NextLoop IF DISP_DATE$ < BEG_DATE$ AND &
				VAL(DISP_DATE$) <> 0.0
			DCONV = 1.0
		END IF

		!
		! Current depreciation amount
		!
		IF DEP_YEAR% = 1%
		THEN
			AMOUNT_CUR = FUNC_ROUND(RATE_PRIOR * BASIS, 2%) + &
				FUNC_ROUND( &
				(DEP_MONTH% + INIT_MONTH% - 12.0) * &
				(RATE_ENDYEAR - RATE_PRIOR) * &
				BASIS / INIT_MONTH% * DCONV, 2%)

		ELSE
			IF DEP_YEAR% > REC_PER AND TRM$(AD_DEPCLASS::CEILTABLE) = ""
			THEN
				DEP_MONTH% = 12.0 - INIT_MONTH% IF DEP_MONTH% > 12.0-INIT_MONTH%
				FRACT = 0.0
				FRACT = DEP_MONTH% / (12.0 - INIT_MONTH%) &
					IF INIT_MONTH% <> 12.0
				AMOUNT_CUR = FUNC_ROUND(RATE_PRIOR * BASIS, 2%) + &
					FUNC_ROUND(FRACT * (RATE_ENDYEAR - RATE_PRIOR) * &
					BASIS * DCONV, 2%)

			ELSE
				AMOUNT_CUR = FUNC_ROUND(RATE_PRIOR * BASIS, 2%) + &
					FUNC_ROUND(DEP_MONTH% / 12.0 * &
					(RATE_ENDYEAR - RATE_PRIOR) * &
					BASIS * DCONV, 2%)
			END IF
		END IF

		AMOUNT_CUR = AMOUNT_CUR - AMOUNT_CTD

		!AMOUNT_CUR = 0.0 IF AMOUNT_CUR < 0.0

		IF AD_DEPCLASS::CEILTABLE <> ""
		THEN
			!
			! Read ceiling table
			!
			GOSUB 18200
			IF TABLE%
			THEN
				TEXT$ = "... Can't find ceiling table"
				CALL OUTP_LINE("", UTL_REPORTX, &
					TITLE$(), TEXT$, 0%)
				GOTO NextLoop
			END IF

			AMOUNT_CUR = CEILING &
				IF CEILING <> 0.0 AND CEILING < AMOUNT_CUR
		ELSE
			DEP_STATUS$ = "F" &
				IF DEP_MON% - &
				DATE_MONCODE(AD_35ASSET::SERVDATE) >= &
				12% * REC_PER
		END IF

		AMOUNT_CTD = AMOUNT_CTD + AMOUNT_CUR

		IF BASIS <> 0.0
		THEN
			CUR_PERC = AMOUNT_CUR / BASIS * 100.0
			RUN_PERC = AMOUNT_CTD / BASIS * 100.0
		ELSE
			CUR_PERC, RUN_PERC = 0.0
		END IF

		SEQ_NUMBER% = SEQ_NUMBER% + 1%
		TEXT$ = FORMAT$(SEQ_NUMBER%, "##") + " " + &
			DEP_PERIOD$ + " " + &
			PERIOD_DESC$ + " " + &
			PRNT_DATE(DEP_DATE$, 8%) + " " + &
			FORMAT$(AMOUNT_CUR, "##,###,###.##") + " " + &
			FORMAT$(CUR_PERC, "###.##%") + " " + &
			FORMAT$(AMOUNT_CTD, "##,###,###.##") + " " + &
			FORMAT$(RUN_PERC, "###.##%")
		TEXT$ = TEXT$ + "  ... Disposal Year" IF DEP_STATUS$ = "R"

		CALL OUTP_LINE("", UTL_REPORTX,TITLE$(), TEXT$, 0%)
		GOTO ExitFunction IF UTL_REPORTX::STAT

		DEP_STATUS$ = "F" &
			IF FUNC_ROUND(AMOUNT_CTD, 2%) = BASIS

		GOTO NextLoop IF DEP_STATUS$ <> "A"

		IF ERA$(LOOP%) <> ""
		THEN
			!
			! Find next period
			!
			GOTO NextLoop IF READ_PERIOD("FIND", ERA$(LOOP%), &
				DEP_PERIOD$, PERIOD_DESC$, "", BEG_DATE$, &
				DEP_DATE$, 1%)
		END IF

		GOTO DepYear

17270		TEXT$ = "... Can't project depreciation. Undefined Depreciation " + &
			"Class " + DEPCLASS$(LOOP%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO NextLoop

17280		TEXT$ = "...  Can't project depreciation. Undefined Depreciation " + &
			"Method " + AD_DEPCLASS::DEPMETHOD
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 NextLoop:
	NEXT LOOP%

 ExitFunction:

	CALL OUTP_FINISH(UTL_REPORTX)

	CLOSE AD_METHOD.CH%
	CALL ASSG_FREECHANNEL(AD_METHOD.CH%)

	CLOSE AD_DEPCLASS.CH%
	CALL ASSG_FREECHANNEL(AD_DEPCLASS.CH%)

	CLOSE AD_TABLE.CH%
	CALL ASSG_FREECHANNEL(AD_TABLE.CH%)

	CLOSE AD_TABLEONE.CH%
	CALL ASSG_FREECHANNEL(AD_TABLEONE.CH%)

	CLOSE AD_TABLETWO.CH%
	CALL ASSG_FREECHANNEL(AD_TABLETWO.CH%)

	CLOSE AD_CEILING.CH%
	CALL ASSG_FREECHANNEL(AD_CEILING.CH%)

	CLOSE AD_CEILINGONE.CH%
	CALL ASSG_FREECHANNEL(AD_CEILINGONE.CH%)

	CLOSE AD_CEILINGTWO.CH%
	CALL ASSG_FREECHANNEL(AD_CEILINGTWO.CH%)

 Exit1:
	EXIT FUNCTION

	%PAGE

18100	!
	! Read optional table
	!
	OPTTABLE$ = STRING$(LEN(AD_TABLE::OPTTABLE), A"?"B)
	YEAR$ = STRING$(LEN(AD_TABLE::YEARS), A"?"B)
	EFFDATE$ = STRING$(LEN(AD_TABLE::EFFDATE), A"?"B)
	DIMEN$ = STRING$(LEN(AD_TABLE::DIMEN), A"?"B)

	WHEN ERROR IN
		FIND #AD_TABLE.CH%, &
			KEY #0% EQ AD_DEPCLASS::OPTTABLE + AD_DEPCLASS::YEARS, &
			REGARDLESS
	USE
		IF ERR = 155% OR ERR = 9%
		THEN
			TABLE% = -1%
			CONTINUE BackFromTable
		END IF
		FILENAME$ = "AD_TABLE"
		CONTINUE HelpError
	END WHEN

 NextOptTable:
	WHEN ERROR IN
		GET #AD_TABLE.CH%, REGARDLESS
	USE
		CONTINUE 18120 IF ERR = 11%
		FILENAME$ = "AD_TABLE"
		CONTINUE HelpError
	END WHEN

	GOTO 18120 IF AD_DEPCLASS::OPTTABLE <> AD_TABLE::OPTTABLE OR &
		AD_DEPCLASS::YEARS <> AD_TABLE::YEARS

	GOTO 18120 IF AD_TABLE::EFFDATE > AD_35ASSET::SERVDATE

	OPTTABLE$ = AD_TABLE::OPTTABLE
	YEAR$ = AD_TABLE::YEARS
	EFFDATE$ = AD_TABLE::EFFDATE
	DIMEN$ = AD_TABLE::DIMEN

	GOTO NextOptTable

18120	SELECT DIMEN$

	!
	! Get percentage from one dim table
	!
	CASE "1"

		WHEN ERROR IN
			GET #AD_TABLEONE.CH%, &
				KEY #0% EQ OPTTABLE$ + YEAR$ + EFFDATE$ + "01", &
				REGARDLESS
		USE
			IF ERR = 155% OR ERR = 9%
			THEN
				TABLE% = -1%
				CONTINUE BackFromTable
			END IF
			FILENAME$ = "AD_TABLEONE"
			CONTINUE HelpError
		END WHEN

		RATE_YEAR = 0.0001 * AD_TABLEONE::PERCENTAGE
		RATE_ENDYEAR = RATE_YEAR

18130		FOR I% = 2% TO DEP_YEAR%
			WHEN ERROR IN
				GET #AD_TABLEONE.CH%, &
					KEY #0% EQ OPTTABLE$ + YEAR$ + EFFDATE$ + &
					FORMAT$(I%, "<0>#"), REGARDLESS
			USE
				CONTINUE 18140 IF ERR=155%
				FILENAME$ = "AD_TABLEONE"
				CONTINUE HelpError
			END WHEN

			RATE_YEAR = 0.0001 * AD_TABLEONE::PERCENTAGE

18140			RATE_ENDYEAR = RATE_ENDYEAR + RATE_YEAR
		NEXT I%

		!
		! Get percentage from two dim table
		!
	CASE "2"

18150		WHEN ERROR IN
			GET #AD_TABLETWO.CH%, &
				KEY #0% EQ OPTTABLE$ + YEAR$ + EFFDATE$ + "01", &
				REGARDLESS
		USE
			IF ERR = 155% OR ERR = 9%
			THEN
				TABLE% = -1%
				CONTINUE BackFromTable
			END IF
			FILENAME$ = "AD_TABLETWO"

			CONTINUE HelpError
		END WHEN

		RATE_YEAR = 0.0001 * AD_TABLETWO::PERCENTAGE(INDEX% - 1%)
		RATE_ENDYEAR = RATE_YEAR

18160		FOR I% = 2% TO DEP_YEAR%
			WHEN ERROR IN
				GET #AD_TABLETWO.CH%, &
					KEY #0% EQ OPTTABLE$ + YEAR$ + EFFDATE$ + &
					FORMAT$(I%, "<0>#"), REGARDLESS
			USE
				CONTINUE 18170 IF ERR = 155%
				FILENAME$ = "AD_TABELTWO"
				CONTINUE HelpError
			END WHEN

			RATE_YEAR = 0.0001 * AD_TABLETWO::PERCENTAGE(INDEX% - 1%)

18170			RATE_ENDYEAR = RATE_ENDYEAR + RATE_YEAR
		NEXT I%

	CASE ELSE
		TABLE% = -1%
	END SELECT

	RATE_PRIOR = RATE_ENDYEAR - RATE_YEAR

 BackFromTable:
	RETURN

18200	!
	! Read ceiling table
	!
	CEILTABLE$ = STRING$(LEN(AD_CEILING::CEILTABLE), A"?"B)
	EFFDATE$ = STRING$(LEN(AD_CEILING::EFFDATE), A"?"B)
	DIMEN$ = STRING$(LEN(AD_CEILING::DIMEN), A"?"B)

	WHEN ERROR IN
		FIND #AD_CEILING.CH%, &
			KEY #0% EQ AD_DEPCLASS::CEILTABLE, &
			REGARDLESS
	USE
		IF ERR = 155% OR ERR = 9%
		THEN
			TABLE% = -1%
			CONTINUE BackFromCeil
		END IF
		FILENAME$ = "AD_CEILING"
		CONTINUE HelpError
	END WHEN


 NextCeilTable:
	WHEN ERROR IN
		GET #AD_CEILING.CH%, REGARDLESS
	USE
		CONTINUE 18220 IF ERR = 11%
		FILENAME$ = "AD_CEILING"
		CONTINUE HelpError
	END WHEN

	GOTO 18220 IF AD_DEPCLASS::CEILTABLE <> AD_CEILING::CEILTABLE

	GOTO 18220 IF AD_CEILING::EFFDATE > AD_35ASSET::SERVDATE

	CEILTABLE$ = AD_CEILING::CEILTABLE
	EFFDATE$ = AD_CEILING::EFFDATE
	DIMEN$ = AD_CEILING::DIMEN

	GOTO NextCeilTable

18220	SELECT DIMEN$

	!
	! Get ceiling from one dim table
	!
	CASE "1"

		WHEN ERROR IN
			GET #AD_CEILINGONE.CH%, &
				KEY #0% EQ CEILTABLE$ + EFFDATE$ + "01", &
				REGARDLESS
		USE
			IF ERR = 155% OR ERR = 9%
			THEN
				TABLE% = -1%
				CONTINUE BackFromCeil
			END IF
			FILENAME$ = "AD_CEILINGONE"
			CONTINUE HelpError
		END WHEN

		CEILING = AD_CEILINGONE::CEILING

18230		FOR I% = 2% TO DEP_YEAR%
			WHEN ERROR IN
				GET #AD_CEILINGONE.CH%, &
					KEY #0% EQ CEILTABLE$ + EFFDATE$ + &
					FORMAT$(I%, "<0>#"), REGARDLESS
			USE
				CONTINUE 18240 IF ERR=155%
				FILENAME$ = "AD_CEILINGONE"
				CONTINUE HelpError
			END WHEN

18240			CEILING = AD_CEILINGONE::CEILING
		NEXT I%

		!
		! Get ceiling from two dim table
		!
	CASE "2"

18250		WHEN ERROR IN
			GET #AD_CEILINGTWO.CH%, &
				KEY #0% EQ CEILTABLE$ + YEAR$ + EFFDATE$ + "01", &
				REGARDLESS
		USE
			IF ERR = 155% OR ERR = 9%
			THEN
				TABLE% = -1%
				CONTINUE BackFromCeil
			END IF
			FILENAME$ = "AD_CEILINGTWO"
			CONTINUE HelpError
		END WHEN

		CEILING = AD_CEILINGTWO::CEILING(INDEX% - 1%)

18260		FOR I% = 2% TO DEP_YEAR%
			WHEN ERROR IN
				GET #AD_CEILINGTWO.CH%, &
					KEY #0% EQ CEILTABLE$ + YEAR$ + EFFDATE$ + &
						FORMAT$(I%, "<0>#"), &
					REGARDLESS
			USE
				CONTINUE 18270 IF ERR = 155%
				FILENAME$ = "AD_CEILINGTWO"
				CONTINUE HelpError
			END WHEN

18270			CEILING = AD_CEILINGTWO::CEILING(INDEX% - 1%)
		NEXT I%

	CASE ELSE
		TABLE% = -1%
	END SELECT

 BackFromCeil:
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
