1	%TITLE "Depreciation Class List"
	%SBTTL "AD_RPRT_DEPCLASS"
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
	! ID:AD028
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Depreciation Class List\* prints the Depreciation
	!	Class Table. This table contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Class
	!	.le
	!	Description
	!	.le
	!	Property Type
	!	.le
	!	Method
	!	.le
	!	Optional Table
	!	.le
	!	Recovery Period
	!	.le
	!	First Year Convention
	!	.le
	!	Disposal Year Convention
	!	.le
	!	Ceiling Table
	!	.le
	!	Salvage Flag
	!	.le
	!	Bonus Flag
	!	.le
	!	Investment Tax Credit Flag
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Depreciation Class>Report
	!	.x Report>Depreciation Class
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_DEPCLASS/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_DEPCLASS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_DEPCLASS.OBJ;*
	!
	! AUTHOR:
	!
	!	09/15/88 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code.
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/22/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS) AD_DEPCLASS_CDD AD_DEPCLASS

	!
	! External functions
	!
	DECLARE STRING FROM_ITEM, TO_ITEM, WLDCRD
	DECLARE WORD CONSTANT PRINT_WIDTH = 132%

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, PRINT_WIDTH)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Depreciation Class\*
	!	.b
	!	.lm +5
	!	The ^*From Depreciation Class\* field
	!	causes the report to begin printing with a selected depreciation class.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	depreciation class in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Depreciation Class>Depreciation Class List
	!	.x Depreciation Class List>From Depreciaiton Class
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Depreciation Class\*
	!	.b
	!	.lm +5
	!	The ^*To Depreciation Class\* field causes
	!	the report to end with a selected depreciation class.
	!	.b
	!	A blank setting will cause the report to end with the last depreciation
	!	class in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Depreciation Class>Depreciation Class List
	!	.x Depreciation Class List>To Depreciation Class
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated depreciation
	!	classes to be printed by entering a Wildcard value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Depreciation Class List
	!	.x Depreication Class List>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.OPN"
	USE
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "DEPRECIATION  CLASS  LIST"
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Class Description                              " + &
		"PT Meth OptTable RecPer " + &
		"FYConv DYConv CeilTable SalvF BonusF ITCF"

	TITLE$(5%) = "."

	LYT_LINE$ = "$Class:006,$Description:047,$PT:050,$Meth:055" + &
		",$OptTable:064,$RecPer:071,$FYConv:078,$DYConv:085" + &
		",$CeilTable:095,$SalvF:101,$BonusF:108,$ITCF:113"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #AD_DEPCLASS.CH%
		ELSE
			FIND #AD_DEPCLASS.CH%, &
				KEY #0% GE FROM_ITEM, &
				REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		FILENAME$ = "AD_DEPCLASS"
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
		GET #AD_DEPCLASS.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_DEPCLASS"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AD_DEPCLASS::DEPCLASS > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AD_DEPCLASS::DEPCLASS, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
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
		AD_DEPCLASS::ITCFACTOR

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

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
