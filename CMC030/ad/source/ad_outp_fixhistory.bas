1	%TITLE "Asset Depreciation History Report"
	%SBTTL "AD_OUTP_FIXHISTORY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_OUTP_FIXHISTORY(AD_35ASSET_CDD AD_35ASSET)

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
	! ID:AD051
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints Projected depreciation.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_OUTP_FIXHISTORY/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AD_OUTP_FIXHISTORY
	!	$ DELETE AD_OUTP_FIXHISTORY.OBJ;*
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
	!		Update to V3.6 Calico coding standards.
	!
	!	04/12/95 - Kevin Handy
	!		Change scope.exit% to scope::scope.exit
	!
	!	08/21/95 - Kevin Handy
	!		Format closer to 80 columns.
	!
	!	08/27/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/27/2000 - Kevin Handy
	!		Use A"x"B
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS)	AD_DEPCLASS_CDD	AD_DEPCLASS

	%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.HB"
	MAP (AD_HISTORY)	AD_HISTORY_CDD	AD_HISTORY

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.HB"
	MAP (AD_DEPRECIATION)	AD_DEPRECIATION_CDD	AD_DEPRECIATION

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	AD_OUTP_FIXHISTORY = 0%
	REPORT$ = "AD051"

400	!

 SetScreen:
	!******************************************************************
	! Set up the report settings screen
	!******************************************************************
	!
	! Ask user to change settings
	!
	GOTO ExitFunction &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!
	! Set up from user input
	!
	OBJECT$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Objects\*
	!	.b
	!	.lm +5
	!	The ^*Objects\* field enters all objects
	!	desired to be displayed in the Depreciation History Query.
	!	.b
	!	The procedure for entry may include "wildcarding".
	!	.lm -5
	!
	! Index:
	!	.x Objects>Depreciation History
	!	.x Depreciation History>Objects
	!
	!--

510	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.OPN"
	USE
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

520	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_HISTORY.OPN"
	USE
		CONTINUE 530 IF ERR = 5%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

530	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ASSET  DEPRECIATION  HISTORY"
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

	!PD_PRODTYPE::DESCRIPTION = &
	!	STRING$(LEN(PD_PRODTYPE::DESCRIPTION), A"?"B)
	!GET #PD_PRODTYPE.CH%, KEY #0% EQ AD_35ASSET::PROD_TYPE, &
	!		REGARDLESS

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
		"FYConv DYConv CeilTable SalvF BonusF ITCF   DepObject"

	TEXT2$ = STRING$(132%, A"."B)

17100	WHEN ERROR IN
		FIND #AD_DEPRECIATION.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM, &
			REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 155%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

17150	WHEN ERROR IN
		GET #AD_DEPRECIATION.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "AD_DEPRECIATION"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF AD_DEPRECIATION::ASSET_NUM <> AD_35ASSET::ASSET_NUM

	GOTO 17150 &
		IF COMP_STRING(AD_DEPRECIATION::DEP_OBJECT, OBJECT$) = 0% AND &
		OBJECT$ <> ""

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT2$, 2%)

17200	WHEN ERROR IN
		GET #AD_DEPCLASS.CH%, &
			KEY #0% EQ AD_DEPRECIATION::DEPCLASS, &
			REGARDLESS
	USE
		CONTINUE 17150 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 2%)

	TEXT$ = AD_DEPCLASS::DEPCLASS + "  " + &
		AD_DEPCLASS::DESCRIPTION +  " " + &
		AD_DEPCLASS::PROPTYPE + " " + &
		AD_DEPCLASS::DEPMETHOD + " " + &
		AD_DEPCLASS::OPTTABLE + "   " + &
		AD_DEPCLASS::YEARS + "   " + &
		AD_DEPCLASS::FYCONV + "     " + &
		AD_DEPCLASS::DYCONV + "     " + &
		AD_DEPCLASS::CEILTABLE + "    " + &
		AD_DEPCLASS::SALVFACTOR + "     " + &
		AD_DEPCLASS::BONUSFACTOR + "      " + &
		AD_DEPCLASS::ITCFACTOR + "      " + &
		AD_DEPRECIATION::DEP_OBJECT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	SEQ_NUMBER% = 0%
	AMOUNT_CTD = 0.0

17210	WHEN ERROR IN
		FIND #AD_HISTORY.CH%, &
			KEY #0% EQ AD_35ASSET::ASSET_NUM + OBJECT$, &
			REGARDLESS
	USE
		CONTINUE 17150 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

	BASIS = AD_35ASSET::COST
	SALVAGE = AD_35ASSET::SALVAGE
	BONUS = AD_35ASSET::BONUS
	ITC = AD_35ASSET::ITCREDUCE

	!
	! Adjust basis if necessary
	!
	BASIS = BASIS - SALVAGE IF AD_DEPCLASS::SALVFACTOR = "Y"
	BASIS = BASIS - BONUS   IF AD_DEPCLASS::BONUSFACTOR = "Y"
	BASIS = BASIS - ITC IF AD_DEPCLASS::ITCFACTOR = "Y"

17215	WHEN ERROR IN
		GET #AD_HISTORY.CH%, REGARDLESS
	USE
		CONTINUE 17150 IF ERR = 11%
		FILENAME$ = "AD_HISTORY"
		CONTINUE HelpError
	END WHEN

	GOTO 17150 IF AD_35ASSET::ASSET_NUM <> AD_HISTORY::ASSET_NUM OR &
		AD_DEPRECIATION::DEP_OBJECT <> AD_HISTORY::DEP_OBJECT
	AMOUNT_CUR = AD_HISTORY::AMOUNT_HIS
	AMOUNT_CTD = AMOUNT_CTD + AD_HISTORY::AMOUNT_HIS
	DEP_PERIOD$ = AD_HISTORY::PERIOD

	IF BASIS <> 0.0
	THEN
		CUR_PERC = AMOUNT_CUR / BASIS * 100.0
		RUN_PERC = AMOUNT_CTD / BASIS * 100.0
	ELSE
		CUR_PERC, RUN_PERC = 0.0
	END IF

	IF READ_PERIOD("READ", ERA$(LOOP%), &
		DEP_PERIOD$, PERIOD_DESC$, "", BEG.DATE$, &
		DEP_DATE$, 0.0)
	THEN
		DEP_PERIOD$ = "??????"
		PERIOD_DESC$ = STRING$(20%, A"?"B)
		DEP_DATE$ = "000000"
	END IF

	SEQ_NUMBER% = SEQ_NUMBER% + 1%
	TEXT$ = FORMAT$(SEQ_NUMBER%, "##") + " " + &
		DEP_PERIOD$ + " " + &
		PERIOD_DESC$ + " " + &
		PRNT_DATE(DEP_DATE$, 8%) + " " + &
		FORMAT$(AMOUNT_CUR, "##,###,###.##") + " " + &
		FORMAT$(CUR_PERC, "###.##%") + " " + &
		FORMAT$(AMOUNT_CTD, "##,###,###.##") + " " + &
		FORMAT$(RUN_PERC, "###.##%") + " " + &
		AD_HISTORY::DEP_STATUS

	CALL OUTP_LINE("", UTL_REPORTX,TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT
	GOTO 17215


 ExitFunction:

	CALL OUTP_FINISH(UTL_REPORTX)

	CLOSE AD_DEPCLASS.CH%
	CALL ASSG_FREECHANNEL(AD_DEPCLASS.CH%)

	CLOSE AD_HISTORY.CH%
	CALL ASSG_FREECHANNEL(AD_HISTORY.CH%)

 Exit1:
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitFunction

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
