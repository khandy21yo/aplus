1	%TITLE "Print Product Labels"
	%SBTTL "PD_RPRT_PRINTLABEL"
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
	! ID:PD002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Product Labels\* program prints out labels for the specified
	!	products.
	!	.lm -5
	!
	! Index:
	!	.x Print Product Labels
	!	.x Product Labels>Print
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_RPRT_PRINTLABEL/LINE
	!	$ LINK/EXE=PD_EXE: PD_RPRT_PRINTLABEL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_RPRT_PRINTLABEL.OBJ;*
	!
	! Author:
	!
	!	08/28/87 - Frank F. Starman
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
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/27/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field begins the label printing
	!	with a selected product number code.
	!	.b
	!	In order to be operative, the value in field (10) must be ^*P\*.
	!	.b
	!	A blank field will cause the labels to begin printing with the first
	!	record in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Print Product Labels
	!	.x Print Product Labels>From Item
	!	.x Item>Print
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field ends the label printing
	!	with a selected product number code.
	!	.b
	!	In order to be operative, the value in field (10) must be ^*P\*.
	!	.b
	!	A blank field will cause the labels to end with the last record in
	!	the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Print Product Labels
	!	.x Print Product Labels>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	This ^*Wildcard\* field
	!	selects designated product numbers to be printed
	!	on the product labels by entering a "wildcard" value in the field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Print Product Labels
	!	.x Print Product Labels>Wildcard
	!
	!--

	HORIZONT% = VAL%(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Horizontal Length
	!	.B
	!	.LM +5
	!	The ^*Horizontal Length\* field enters the number of
	!	characters which will be in each horizontal line of the label.
	!	.LM -5
	!
	! Index:
	!	.x Horizontal Length
	!	.x Length>Horizontal
	!
	!--

	VERTICAL% = VAL%(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) Vertical Length\*
	!	.B
	!	.LM +5
	!	The ^*Vertical Length\* field enters the number of
	!	vertical lines which the label will contain.
	!	.LM -5
	!
	! Index:
	!	.x Vertical Length
	!	.x Length>Vertical
	!
	!--

	CENTER_LINE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	.x Center
	!	^*(08) Center\*
	!	.B
	!	.LM +5
	!	The ^*Center\* field centers the label information.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	.x Sort by>Print Product Labels
	!	^*(10) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field
	!	prints product labels in product number
	!	or type code order.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*P\*	- Product number
	!	.te
	!	^*T\*	- Product Type
	!	.te
	!	^*C\*	- Product Category
	!	.te
	!	^*D\*	- Product Description
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Print Product Labels>Sort by
	!
	!--


	SELECT SORT_BY$
	CASE "C"
		SORT_KEY% = 2%
	CASE "D"
		SORT_KEY% = 3%
	CASE "P"
		SORT_KEY% = 0%
	CASE "T"
		SORT_KEY% = 1%
	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  LABEL  LIST"
	TITLE$(2%) = "Product Description System"
	TITLE$(3%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
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
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

	END SELECT

17300	!
	! Print out one line
	!
	LINE_COUNT% = 0%
	SELECT CENTER_LINE$

	CASE "Y"

		TEXT$ = LEFT(SPACE$((HORIZONT% - &
			LEN(TRM$(PD_PRODUCT::PRODUCT_NUM))) / 2%) + &
			PD_PRODUCT::PRODUCT_NUM, HORIZONT%)
		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, TEXT$, 0%)
		LINE_COUNT% = LINE_COUNT% + 1%

		TEXT$ = LEFT(SPACE$((HORIZONT% - &
			LEN(TRM$(PD_PRODUCT::DESCRIPTION))) / 2%) + &
			PD_PRODUCT::DESCRIPTION, HORIZONT%)
		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, TEXT$, 0%)
		LINE_COUNT% = LINE_COUNT% + 1%

	CASE "N"

		TEXT$ = LEFT(PD_PRODUCT::PRODUCT_NUM, HORIZONT%)
		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, TEXT$, 0%)
		LINE_COUNT% = LINE_COUNT% + 1%

		TEXT$ = LEFT(PD_PRODUCT::DESCRIPTION, HORIZONT%)
		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, TEXT$, 0%)
		LINE_COUNT% = LINE_COUNT% + 1%

	END SELECT

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR I% = LINE_COUNT% + 1% TO VERTICAL%


17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

 ExitProgram:
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

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
