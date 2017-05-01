1	%TITLE " Print Journal"
	%SBTTL "AD_RPRT_UNITS"
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
	! ID:AD001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Journal\* option prints
	!	the Journal Units File. A specific file can be selected
	!	for printing by entering a designated batch number.
	!	This journal print out contains the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Object
	!	.le
	!	Date
	!	.le
	!	Asset Number
	!	.le
	!	Description
	!	.le
	!	Quantity
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Print>Journal
	!	.x Journal>Print
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_UNITS/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_UNITS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_UNITS.OBJ;*
	!
	! AUTHOR:
	!
	!	12/11/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/96 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_JOURNAL.HB"
	MAP (AD_JOURNAL)	AD_JOURNAL_CDD	AD_JOURNAL

	%INCLUDE "SOURCE:[AD.OPEN]AD_UNITS.HB"
	MAP (AD_UNITS)	AD_UNITS_CDD	AD_UNITS

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD	AD_35ASSET

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Object\*
	!	.b
	!	.lm +5
	!	The ^*From Object\* field causes the printing
	!	to begin with the selected object.
	!	.b
	!	A blank field causes the report to start with the first
	!	record in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Object>Print Journal
	!	.x Print Journal>From Object
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Object\*
	!	.b
	!	.lm +5
	!	The ^*To Object\* field causes the printing
	!	to end with the selected object.
	!	.b
	!	A blank field causes the report to end with the last
	!	record in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Object>Print Journal
	!	.x Print Journal>To Object
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^* (05) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a
	!	particular batch to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Print Journal
	!	.x Print Journal>Batch Number
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field causes the printing
	!	to begin with the selected date.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x From Date>Print Journal
	!	.x Print Journal>From Date
	!
	!--

	TO_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%))

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field causes the printing
	!	to end with the selected date.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x To Date>Print Journal
	!	.x Print Journal>To Date
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_JOURNAL.OPN"
	USE
		FILENAME$ = "AD_JOURNAL"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_UNITS.OPN"
	USE
		FILENAME$ = "AD_UNITS"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "UNITS  OF  PRODUCTION  JOURNAL  " + &
		"BATCH No. " + BATCH_NO$
	TITLE$(2%) = "Asset depreciation system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Object Date       Asset#     Description" + &
		"                                    Qty"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AD_JOURNAL.CH%
		ELSE
			FIND #AD_JOURNAL.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
	END WHEN

 GetNextSto:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AD_JOURNAL.CH%, REGARDLESS
	USE
		CONTINUE IF ERR = 155%
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_JOURNAL"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AD_JOURNAL::DEP_OBJECT > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextSto IF (AD_JOURNAL::ACTION_DATE < FROM_DATE$) &
		AND FROM_DATE$ <> ""

	GOTO GetNextSto IF (AD_JOURNAL::ACTION_DATE > TO_DATE$) &
		AND TO_DATE$ <> ""

	FIND #AD_UNITS.CH%, &
		KEY #0% EQ AD_JOURNAL::DEP_OBJECT + AD_JOURNAL::ACTION_DATE, &
		REGARDLESS

 GetNextRec:
17030	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AD_UNITS.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_UNITS"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	IF AD_UNITS::DEP_OBJECT <> AD_JOURNAL::DEP_OBJECT OR &
		AD_UNITS::ACTION_DATE <> AD_JOURNAL::ACTION_DATE
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO GetNextSto
	END IF

17300	!
	! Print out one line
	!
	GOSUB 18500

	TEXT$ = AD_JOURNAL::DEP_OBJECT + "      " + &
		PRNT_DATE(AD_JOURNAL::ACTION_DATE, 8%) + " " + &
		AD_UNITS::ASSET_NUM + " " + &
		AD_35ASSET::DESCRIPTION + " " + &
		FORMAT$(AD_UNITS::QUANTITY, "#########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

18500	WHEN ERROR IN
		GET #AD_35ASSET.CH%, &
			KEY #0% EQ AD_UNITS::ASSET_NUM, &
			REGARDLESS
	USE
		AD_35ASSET::DESCRIPTION = &
			STRING$(LEN(AD_35ASSET::DESCRIPTION), A"?"B)

		CONTINUE ComeBack1 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

 ComeBack1:
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
