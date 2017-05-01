1	%TITLE "Purchase Order Category List"
	%SBTTL "PO_RPRT_CATEGORY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:PO005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Category\* option
	!	prints a list of categories from the Category
	!	Definition file.
	!	.lm -5
	!
	! Index:
	!	.x Report>Category
	!	.x Category>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_CATEGORY/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_CATEGORY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_CATEGORY.OBJ;*
	!
	! Author:
	!
	!	04/06/90 - Kevin Handy
	!
	! Modification History:
	!
	!	11/01/87 - Frank F. Starman
	!		Added PRINT_FORM field.
	!
	!	04/06/90 - Kevin Handy
	!		Taken from PD_RPRT_CATEGORY.
	!
	!	06/19/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!		Also, rewrote with regards to the horizontal format.
	!
	!	10/11/91 - Dan Perkins
	!		Got horizontal format to work.
	!		Cleaned up program code.  Checked error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_CATEGORY.HB"
	MAP	(PO_CATEGORY)	PO_CATEGORY_CDD		PO_CATEGORY

	!
	! Declare variables and constants
	!
	DECLARE	STRING	FROM_ITEM		!
	DECLARE	STRING	TO_ITEM			! User
	DECLARE	STRING	WLDCRD			! Inputs
	DECLARE	STRING	PRINT_FORM		!

	!
	! Dimension arrays
	!
	DIM PO_CATEGORY_CDD PO_CAT_ARR(5%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initialize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Code\*
	!	.b
	!	.lm +5
	!	The ^*From Code\* field
	!	begins the list with a selected code by entering
	!	that selection in this field.
	!	.b
	!	If this field is blank, the list will begin printing with the
	!	first record in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Code\*
	!	.b
	!	.lm +5
	!	The ^*To Code\* field
	!	ends the list with a selected
	!	code by entering the selected code in this field.
	!	.b
	!	If this field is blank, the list will end with the
	!	last record in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects designated product categories
	!	to be printed by entering a "wildcard" value in this
	!	field.
	!	.lm -5
	!
	! Index:
	!
	!--

	PRINT_FORM = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Format	H,V,\*
	!	.b
	!	.lm +5
	!	The ^*Format\* field
	!	determines the format in which the report
	!	will be printed.
	!	.b
	!	Valid options are:
	!	.table 3,25
	!	.te
	!	^*H\* - Horizontal (Five columns per page)
	!	.te
	!	^*V\* - Vertical (Single column per page)
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open the PO Category definition file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_CATEGORY.OPN"
	USE
		FILENAME$ = "PO_CATEGORY"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "PRUCHASE  ORDER  CATEGORY  LIST"
	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Column headings
	!
	IF PRINT_FORM = "V"
	THEN
		TITLE$(4%) = "Code     Description"
	ELSE
		TITLE$(4%) = "Code     Description" + &
			"                        " + &
			"Code     Description" + &
			"                        " + &
			"Code     Description"
	END IF

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	LOOP% = 0%

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #PO_CATEGORY.CH%
		ELSE
			FIND #PO_CATEGORY.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PO_CATEGORY"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	WHEN ERROR IN
		GET #PO_CATEGORY.CH%, REGARDLESS
	USE
		CONTINUE EndProg IF ERR = 11%
		FILENAME$ = "PO_CATEGORY"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record versus TO_ITEM
	!
	GOTO ExitProgram IF &
		PO_CATEGORY::CODE > TO_ITEM AND &
		TO_ITEM <> ""

	!
	! Check current record versus the WildCard
	!
	GOTO GetNextRec IF &
		COMP_STRING(EDIT$(PO_CATEGORY::CODE, -1%), WLDCRD) = 0% AND &
		WLDCRD <> ""

	!
	! Decide which print form we are using
	!
	SELECT PRINT_FORM

	CASE "V"
		TEXT$ = PO_CATEGORY::CODE + "     " + &
			PO_CATEGORY::DESCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	CASE "H"
		PO_CAT_ARR(LOOP%) = PO_CATEGORY

		LOOP% = LOOP% + 1%

		IF LOOP% > 2%
		THEN
			TEXT$ = ""

			FOR I% = 0% TO LOOP%

				TEXT$ = TEXT$ + &
					PO_CAT_ARR(I%)::CODE + &
					"     "     + &
					LEFT$(PO_CAT_ARR(I%)::DESCR, + &
					30%) + "     "
			NEXT I%

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			LOOP% = 0%
		END IF

	END SELECT

	GOTO GetNextRec

	%PAGE

 EndProg:
	IF PRINT_FORM = "H"
	THEN
		TEXT$ = ""

		FOR I% = 0% TO LOOP% - 1%

			TEXT$ = TEXT$ + PO_CAT_ARR(I%)::CODE + "     " + &
				LEFT$(PO_CAT_ARR(I%)::DESCR, 30%) + "     "
		NEXT I%

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
