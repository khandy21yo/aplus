1	%TITLE "Units of Production"
	%SBTTL "AD_RPRT_REGUNIT"
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
	! ID:AD019
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Units of Production\* report
	!	prints the Units Register File. This reports included the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Asset Number
	!	.le
	!	Object
	!	.le
	!	Period
	!	.le
	!	Activity Date
	!	.le
	!	Station Man
	!	.le
	!	Quantity
	!	.els
	!
	! Index:
	!	.x Report>Units of Production
	!	.x Units of Production>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_REGUNIT/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_REGUNIT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_REGUNIT.OBJ;*
	!
	! AUTHOR:
	!
	!	01/18/88 - Lance Williams
	!
	! MODIFIDATION HISTORY:
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/96 - Kevin Handy
	!		Lose hard coding of channels.
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/23/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.HB"
	MAP (AD_REGUNIT) AD_REGUNIT_CDD AD_REGUNIT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Asset _#\*
	!	.B
	!	.LM +5
	!	The ^*From Asset _#\* field prints
	!	the report beginning with a particular asset number.
	!	.B
	!	A blank field will cause the report to begin with the first
	!	asset number in file.
	!	.LM -5
	!
	! Index:
	!	.x From Asset Number>Units of Production Report
	!	.x Units of Production Report>From Asset Number
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Asset _#\*
	!	.b
	!	.lm +5
	!	The ^*To Asset _#\* field prints the report
	!	ending with a particular asset number.
	!	.b
	!	A blank field causes the report to end with the last
	!	asset number in file.
	!	.lm -5
	!
	! Index:
	!	.x To Asset Number>Units of Production Report
	!	.x Units of Production Report>To Asset Number
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting
	!	selects assets using the wildcarding
	!	technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Units of Production Report
	!	.x Units of Production Report>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.OPN"
	USE
		FILENAME$ = "AD_REGUNIT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "AD REGUNIT  REPORT"
	TITLE$(2%) = " Ad system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Asset#     Object Period ActDate    " + &
		"StationMan  Quantity"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AD_REGUNIT.CH%
		ELSE
			FIND #AD_REGUNIT.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
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
		GET #AD_REGUNIT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_REGUNIT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record.
	!
	GOTO ExitTotal IF (AD_REGUNIT::ASSET_NUM > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AD_REGUNIT::ASSET_NUM, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

17300	!
	! Print out one line
	!
	IF AD_REGUNIT::ASSET_NUM <> STORE_CHECK$ AND STORE_CHECK$ <> ""
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	TEXT$ = AD_REGUNIT::ASSET_NUM + " " + &
		AD_REGUNIT::DEP_OBJECT + "      " + &
		AD_REGUNIT::PERIOD + " " + &
		PRNT_DATE(AD_REGUNIT::ACTION_DATE, 8%) + " " + &
		AD_REGUNIT::STATIONMAN + " " + &
		FORMAT$(AD_REGUNIT::QUANTITY, "###,###.#")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT
	STORE_CHECK$ = AD_REGUNIT::ASSET_NUM

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
