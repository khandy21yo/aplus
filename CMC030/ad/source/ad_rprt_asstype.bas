1	%TITLE "Asset Type List"
	%SBTTL "AD_RPRT_ASSTYPE"
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
	! ID:AD011
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	The ^*Asset Type\* option prints
	!	the Asset Type Description List. This list contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Code
	!	.le
	!	Description
	!	.els
	!	.LM -5
	!
	! Index:
	!	.x Asset Type>Report
	!	.x Report>Asset Type
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_ASSTYPE/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_ASSTYPE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_ASSTYPE.OBJ;*
	!
	! AUTHOR:
	!
	!	12/05/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/18/92 - Dan Perkins
	!		Changed I and J variables to I% and J%.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	08/27/96 - Kevin Handy
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
	!	06/15/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.HB"
	MAP (AD_ASSTYPE) AD_ASSTYPE_CDD AD_ASSTYPE

	!
	! External functions
	!
	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD, PRINT_FORM
	DECLARE BYTE J
	DECLARE LONG LOOP, TEST_LOOP, I
	DECLARE WORD CONSTANT PRINT_WIDTH = 132%, INIT_DIM = 1000%

	DIMENSION STRING CODE(INIT_DIM), DESCR(INIT_DIM)
	DIMENSION LONG COLUMN(5%)

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
	!	^*(01) From Type\*
	!	.B
	!	.LM +5
	!	The ^*From Type\* field causes printing of
	!	the report to begin with a particular "Type".
	!	.B
	!	A blank field will cause the report to start with the
	!	first Type in the file.
	!	.LM -5
	!
	! Index:
	!	.x From Type
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Type\*
	!	.B
	!	.LM +5
	!	The ^*To Type\* field will cause the printing of
	!	the report to end with a particular "Type".
	!	.B
	!	A blank field will cause the report to end with the
	!	last Type in the file.
	!	.LM -5
	!
	! Index:
	!	.x To Type
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.B
	!	.LM +5
	!	The ^*Wildcard\* setting enables the user to print a
	!	report including selected location numbers using the
	!	wildcarding technique.
	!	.LM -5
	!
	! Index:
	!	.x Wildcard>Asset Type List
	!	.x Asset Type List>Wildcard
	!
	!--

	PRINT_FORM = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Format (H,V)\*
	!	.B
	!	.LM +5
	!	The ^*Format\* setting prints the report
	!	vertically (V) or horizontally (H).
	!	.B
	!	An entry is required in this field.
	!	.LM -5
	!
	! Index:
	!	.x Format>Asset Type List
	!	.x Asset Type List>Format
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.OPN"
	USE
		FILENAME$ = "AD_ASSTYPE.OPN"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ASSET  TYPE  LIST"
	TITLE$(2%) = "Asset depreciation system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	SELECT PRINT_FORM

	CASE "V"
		TITLE$(4%) = "Code Description"

	CASE "H"
		TITLE$(4%) = "Code Description          " + &
			"Code Description          " + &
			"Code Description          " + &
			"Code Description          " + &
			"Code Description         "

	END SELECT

	TITLE$(5%) = "."

	LYT_LINE$ = "$CODE:005,$DESCRTIPTION:045"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #AD_ASSTYPE.CH%
		ELSE
			FIND #AD_ASSTYPE.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
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
		GET #AD_ASSTYPE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_ASSTYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AD_ASSTYPE::ASSET_TYPE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AD_ASSTYPE::ASSET_TYPE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	SELECT PRINT_FORM

	CASE "V"
		TEXT = AD_ASSTYPE::ASSET_TYPE + "   " + &
			AD_ASSTYPE::DESCRIPTION

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT, 0%)

	CASE "H"
		LOOP = LOOP + 1%
		CODE(LOOP) = AD_ASSTYPE::ASSET_TYPE
		DESCR(LOOP) = AD_ASSTYPE::DESCRIPTION

	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	IF PRINT_FORM = "H" AND LOOP > 0%
	THEN
		TEST_LOOP = LOOP
		FOR I% = 2% TO 5%
			COLUMN(I%) = COLUMN(I% - 1%) + &
				INT(TEST_LOOP / (7.0 - I%) + 0.9)
			TEST_LOOP = LOOP - COLUMN(I%)
		NEXT I%

		FOR I% = 1% TO COLUMN(2%) - 1% STEP 1%
			TEXT = ""
			FOR J% = 1% TO 5%
				TEXT = TEXT + CODE(I% + COLUMN(J%)) + "   " + &
					DESCR(I% + COLUMN(J%)) + " "
			NEXT J%

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)
		NEXT I%

		TEXT = ""

		FOR J% = 1% TO LOOP - 5% * (COLUMN(2%) - 1%)
			TEXT = TEXT + CODE(COLUMN(2%) + COLUMN(J%)) + "   " + &
				DESCR(COLUMN(2%) + COLUMN(J%)) + " "
		NEXT J%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)

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
