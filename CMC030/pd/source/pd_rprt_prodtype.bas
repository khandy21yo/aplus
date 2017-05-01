1	%TITLE "Product Type List"
	%SBTTL "PD_RPRT_PRODTYPE"
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
	! ID:PD006
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Type\* option
	!	prints a list of the product types from the Product Type
	!	file. The following fields are included:
	!	.table 3,25
	!	.te
	!	^*Code
	!	.te
	!	Description\*
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Product Type>List
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_RPRT_PRODTYPE/LINE
	!	$ LINK/EXE=PD_EXE: PD_RPRT_PRODTYPE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_RPRT_PRODTYPE.OBJ;*
	!
	! Author:
	!
	!	07/20/87 - Frank F. Starman
	!
	! Modification History:
	!
	!	04/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/04/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP	(PD_PRODTYPE)	PD_PRODTYPE_CDD	PD_PRODTYPE

	DECLARE	STRING	TEXT, &
			FROM_ITEM, &
			TO_ITEM, &
			WLDCRD, &
			PRINT_FORM
	DECLARE	BYTE	J
	DECLARE	LONG	LOOP, &
			TEST_LOOP, &
			I
	DECLARE	WORD	CONSTANT PRINT_WIDTH = 132%, &
			INIT_DIM = 1000%

	DIMENSION STRING	CODE(INIT_DIM), &
				DESCR(INIT_DIM)
	DIMENSION LONG	COLUMN(5%)

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
	!	^*(01) From Code\*
	!	.b
	!	.lm +5
	!	The ^*From Code\* field begins printing
	!	the list with a selected type code
	!	.b
	!	A blank field will cause the list to begin with the first type
	!	code in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Code>Product Type
	!	.x Product Type>From Code
	!	.x Code>From
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Code\*
	!	.b
	!	.lm +5
	!	The ^*To Code\* field ends printing
	!	with a selected type code.
	!	.b
	!	A blank field will cause the list to end with the last type code in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Code>Product Type
	!	.x Product Type>To Code
	!	.x Code>To
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects designated product types to be printed
	!	on the list by entering a "wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Product Type
	!	.x Product Type>Wildcard
	!
	!--

	PRINT_FORM = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.x Format>Product Type
	!	^*(05) Format\*
	!	.b
	!	.lm +5
	!	The ^*Format\* field
	!	prints the list in a horizontal or vertical
	!	format.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*H\*	- Horizontal Format
	!	.te
	!	^*V\*	- Vertical Format
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Product Type>Format
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.OPN"
	USE
		FILENAME$ = "PD_PRODTYPE.OPN"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  TYPE  LIST"
	TITLE$(2%) = "Product description system"
	TITLE$(3%) = ""

	!
	! Heading
	!
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

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #PD_PRODTYPE.CH%
		ELSE
			FIND #PD_PRODTYPE.CH%, &
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
		GET #PD_PRODTYPE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODTYPE.OPN"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PD_PRODTYPE::CODE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(PD_PRODTYPE::CODE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	SELECT PRINT_FORM

	CASE "V"
		TEXT = PD_PRODTYPE::CODE + "   " + &
			PD_PRODTYPE::DESCRIPTION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)

	CASE "H"
		LOOP = LOOP + 1%
		CODE(LOOP) = PD_PRODTYPE::CODE
		DESCR(LOOP) = PD_PRODTYPE::DESCRIPTION

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
				INT(TEST_LOOP/(7.0 - I%) + 0.9)

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
