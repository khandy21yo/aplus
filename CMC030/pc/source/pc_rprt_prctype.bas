1	%TITLE "Price Type List"
	%SBTTL "PC_RPRT_PRCTYPE"
	%IDENT "V3.6a Calico"

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
	! ID:PC005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Price Type\* option prints
	!	a report which will contain the following information:
	!	.table 3,25
	!	.te
	!	Type
	!	.te
	!	Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Price Type>Report
	!	.x Report>Price Type
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_RPRT_PRCTYPE/LINE
	!	$ LINK/EXE=PC_EXE: PC_RPRT_PRCTYPE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_RPRT_PRCTYPE.OBJ;*
	!
	! Author:
	!
	!	07/20/87 - Frank F. Starman
	!
	! Modification History:
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/29/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP	(PC_PRCTYPE)	PC_PRCTYPE_CDD	PC_PRCTYPE

	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD, PRINT_FORM

	DECLARE	WORD	CONSTANT PRINT_WIDTH = 132%, INIT_DIM = 1000%

	DIMENSION	STRING	CODE(INIT_DIM), DESCR(INIT_DIM)
	DIMENSION	LONG	COLUMN(5%)

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
	!	.b
	!	.lm +5
	!	The ^*From Type\* field
	!	begins the list with a selected
	!	price or cost type code.
	!	.b
	!	If this field is blank, the list will begin printing with the
	!	first record in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Type>Price Type
	!	.x Price Type>From Type
	!	.x Type>From
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Type\*
	!	.b
	!	.lm +5
	!	The ^*To Type\* field
	!	concludes the list with a
	!	selected price or cost type code.
	!	.b
	!	If this field is blank, the list will end with the last
	!	record in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Type>Price Type
	!	.x Price Type>To Type
	!	.x Type>To
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects designated price or
	!	cost type codes to be printed using a ^*"Wildcard"\*..
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Price Type
	!	.x Price Type>Wildcard
	!
	!--

	PRINT_FORM = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Format (H,V)\*
	!	.b
	!	.lm +5
	!	The ^*Format (H,V)\* field
	!	determines the format in which the
	!	list will be printed.
	!	.b
	!	A vertical format will print a single column of type codes and
	!	related descriptions. A horizontal format will print five (5) columns
	!	of information across the page.
	!	.b
	!	A value in this field is required. Valid values are:
	!	.table 3,25
	!	.te
	!	^*H\* = Horizontal Format
	!	.te
	!	^*V\* = Vertical Format
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Format>Price Type
	!	.x Price Type>Format
	!
	!--



300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.OPN"
	USE
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRICE  TYPE  LIST"
	TITLE$(2%) = "Product Price & Cost System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	SELECT PRINT_FORM

	CASE "V"
		TITLE$(4%) = "Type Description"

	CASE "H"
		TITLE$(4%) = "Type Description          " + &
			"Type Description          " + &
			"Type Description          " + &
			"Type Description          " + &
			"Type Description         "

	END SELECT

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #PC_PRCTYPE.CH%
		ELSE
			FIND #PC_PRCTYPE.CH%, &
				KEY #0% GE FROM_ITEM, &
				REGARDLESS
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
		GET #PC_PRCTYPE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PC_PRCTYPE::CODE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(PC_PRCTYPE::CODE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	SELECT PRINT_FORM

	CASE "V"
		TEXT = PC_PRCTYPE::CODE + "   " + &
			PC_PRCTYPE::DESCRIPTION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)

	CASE "H"
		LOOP% = LOOP% + 1%
		CODE(LOOP%) = PC_PRCTYPE::CODE
		DESCR(LOOP%) = PC_PRCTYPE::DESCRIPTION

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
	IF PRINT_FORM = "H" AND LOOP% > 0%
	THEN
		TEST_LOOP% = LOOP%

		FOR I% = 2% TO 5%

			COLUMN(I%) = COLUMN(I% - 1%) + &
				INT(TEST_LOOP% / (7.0 - I%) + 0.9)

			TEST_LOOP% = LOOP% - COLUMN(I%)
		NEXT I%

		FOR I5% = 1% TO COLUMN(2%) - 1% STEP 1%
			TEXT = ""

			FOR J% = 1% TO 5%
				TEXT = TEXT + CODE(I% + COLUMN(J%)) + "   " + &
					DESCR(I% + COLUMN(J%)) + " "
			NEXT J%

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)
		NEXT I5%

		TEXT = ""

		FOR J% = 1% TO LOOP% - 5% * (COLUMN(2%) - 1%)

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
