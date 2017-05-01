1	%TITLE "Print Work Order Form"
	%SBTTL "WP_FORM_JOBJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:WPJOUR
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Work Order Form\* option
	!	prints the Work or Job Orders on user designed forms.
	!	.lm -5
	!
	! Index:
	!	.x Print>Work Order>Form
	!	.x Work Order>Form>Print
	!	.x Job Order>Form>Print
	!	.x Print>Job Order>Form
	!
	! Option:
	!
	!	WP_REPORT$JOUR
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_FORM_JOBJOUR/LINE
	!	$ LINK/EXECUTABLE=WP_EXE:*.EXE WP_FORM_JOBJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_FORM_JOBJOUR.OBJ;*
	!
	! Author:
	!
	!	06/03/91 - Val James Allen
	!
	! Modification history:
	!
	!
	!	06/19/91 - Craig Tanner
	!		Added section to update UTL_REPORT from master.
	!
	!	09/11/91 - Frank F. Starman
	!		Fix error trapping for WP_REGLINE file.
	!
	!	03/05/93 - Dan Perkins
	!		Fixed error trapping at line 18025 to trap error 11
	!		and not error 9.
	!
	!	06/18/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/08/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 Calico standards.
	!
	!	04/08/95 - Kevin Handy
	!		Added code to zero totals, so it doesn't accumulate
	!		across jobs printed.
	!
	!	04/12/95 - Kevin Handy
	!		Change scope.exit% to scope::scope_exit
	!
	!	07/27/95 - Kevin Handy
	!		Format closer to 80 columns.
	!
	!	10/31/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/07/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	05/21/98 - Kevin Handy
	!		Fix file name on some error messages.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/98 - Kevin Handy
	!		Don't bother erasing SMG_SCREEN_DATA, which is
	!		never created.
	!
	!	11/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)	WP_JOB_CDD	WP_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD	WP_REGLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP	(JC_TYPE)	JC_TYPE_CDD	JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP	(JC_CLASS)	JC_CLASS_CDD	JC_CLASS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM	FORM_GROUP_CDD FORM_GROUP(10%)

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
		MAX_LINE%, &
		HEADER_TOTAL, &
		LINE_TOTAL, &
		ROLLCOUNT$ = 4%, &
		STATEMENT_DATE$ = 8%, &
		PAGE_NUMBER%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "WPJOUR"

	!
	! Look up device
	!
	CALL  READ_DEVICE("WP_FORM", WP_FORM.DEV$, STAT%)

500	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	!
	! store original values for the help message
	!
	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM

	!
	! Ask user to change settings
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)
	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field
	!	enters the batch number for which form(s) are to be
	!	printed.
	!	.b
	!	Each journal file is assigned two (2) alphanumeric characters as a batch
	!	number.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Order Entry Order Form
	!	.x Order Entry Order Form>Batch Number
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)
	!++
	! Abstract:FLD02
	!	^*(02) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field selects the order in which the
	!	forms are to be printed.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*O\* - Order Number
	!	.te
	!	^*T\* - Order Type
	!	.te
	!	^*C\* - Order Class
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Order Entry Order Form
	!	.x Order Entry Order Form>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the forms to print beginning with
	!	the number entered in this field. The value must be in agreement with
	!	field (02) Sort by.
	!	.b
	!	A blank field will cause the forms to print beginning with the first item in
	!	the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Order Entry Order Form
	!	.x Order Entry Order Form>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.B
	!	.LM +5
	!	The ^*To Item\* field causes the printing to end
	!	with the item designated.  The value must be in agreement with the value
	!	entered in field (02) Sort by.
	!	.B
	!	A blank value in this field will cause the printing to continue
	!	until the last item in the file has been printed.
	!	.LM -5
	!
	! Index:
	!	.x To Item>Order Entry Order Form
	!	.x Order Entry Order Form>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.B
	!	.LM +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a "wildcard" using the wildcarding technique. The value
	!	entered must be in agreement with field (02) Sort by.
	!	.B
	!	For information on "wildcarding", refer to Appendix B.
	!	.LM -5
	!
	! Index:
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(5%))
	! Abstract:FLD06
	!	^*(06) Form Name\*
	!	.B
	!	.LM +5
	!	The ^*Form Name\* field enters the form name which
	!	will be used when printing. The format(s) are created in the
	!	Edit Forms option in the Utility Section of the menu.
	!	.lm -5
	!
	! Index:
	!	.x Form Name
	!
	!--

	SELECT SORTBY$

	CASE "O"
		K_NUM% = 0%

	CASE "T"
		K_NUM% = 1%

	CASE "C"
		K_NUM% = 2%

	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!***************************************************************
	! Open all other files
	!***************************************************************

600	!
	! Open WIP order journal header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.OPN"
	USE
		FILENAME$ = "WP_JOB_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

610	!
	! Open WIP order journal line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.OPN"
	USE
		FILENAME$ = "WP_ORDERLINE_"  + BATCH_NO$
		CONTINUE HelpError
	END WHEN

620	!
	! Open product description  file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE 630 IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

630	!
	! Open WIP register line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
	USE
		CONTINUE 640 IF ERR = 5%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

640	!
	! Open Location description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

650	!
	! Open Job Type description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.OPN"
	USE
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

660	!
	! Open Job Class description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.OPN"
	USE
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
	END WHEN

	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	!
	! Load In The Form
	!
	GOSUB LoadForm

	!
	! Go check out aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through header file
	!*******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #WP_JOB.CH%, KEY #K_NUM%
		ELSE
			FIND #WP_JOB.CH%, KEY #K_NUM% GE FROM_ITEM$
		END IF
	USE
		CONTINUE 3000
	END WHEN

 GetNextRec:
2010	!
	! Get next record
	!
	WHEN ERROR IN
		GET #WP_JOB.CH%, REGARDLESS
	USE
		CONTINUE 3000
	END WHEN

	SELECT SORTBY$

	CASE "O"
		GOTO 3000 IF (WP_JOB::JOB > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(WP_JOB::JOB, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO 3000 IF (WP_JOB::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(WP_JOB::TTYPE, -1%), &
			WLDCRD$) = 0%

	CASE "C"
		GOTO 3000 IF (WP_JOB::CLASS > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(WP_JOB::CLASS, -1%), &
			WLDCRD$) = 0%

	END SELECT

	LINE_TOTAL = 0.0
	HEADER_TOTAL = 0.0

2100	!
	! Get the records that match in the tables
	!
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, KEY #0% EQ WP_JOB::LOCATION
	USE
		UTL_LOCATION::LOCNAME = ""
		UTL_LOCATION::ADDRESS1 = ""
		UTL_LOCATION::ADDRESS2 = ""
		UTL_LOCATION::CITY = ""
		UTL_LOCATION::STATE = ""
		UTL_LOCATION::ZIP = ""

		CONTINUE 2120
	END WHEN

2120	WHEN ERROR IN
		GET #JC_TYPE.CH%, KEY #0% EQ WP_JOB::TTYPE
	USE
		JC_TYPE::DESCR = ""
		CONTINUE 2130
	END WHEN

2130	WHEN ERROR IN
		GET #JC_CLASS.CH%, KEY #0% EQ WP_JOB::CLASS
	USE
		JC_CLASS::DESCR = ""
		CONTINUE 2150
	END WHEN

2150	GOSUB PrintStmt

2190	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Go get the next  record
	!
	GOTO GetNextRec

3000	!*******************************************************************
	! Found the end of the order journal file
	!*******************************************************************

	%PAGE

 ExitProgram:
4000	!******************************************************************
	! Exit the program
	!******************************************************************
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 PrintStmt:
18000	!***************************************************************
	! Print the Statement now
	!***************************************************************

	LINE_COUNT% = 0%
	BODY_COUNT% = 0%
	PAGE_NUMBER% = 1%
	ROLLCOUNT$ = "0000"

18010	!
	! Print the top of statement
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Find out if this sucker is already in the register file
	!
18020	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, KEY #0% EQ WP_JOB::JOB
	USE
		CONTINUE 18030 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

18025	WHEN ERROR IN
		GET #WP_REGLINE.CH%
	USE
		CONTINUE 18030 IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 18030 IF WP_REGLINE::JOB <> WP_JOB::JOB

	ROLLCOUNT$ = WP_REGLINE::LLINE

	GOTO 18025

	!
	! Get the lines for the header
	!
18030	WHEN ERROR IN
		FIND #WP_ORDERLINE.CH%, KEY #0% GE WP_JOB::JOB
	USE
		CONTINUE 18090 IF ERR = 155%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	CURRENT_LINE% = 0%

 ReadLine:
	WHEN ERROR IN
		GET #WP_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE 18090 IF ERR = 11%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	IF (WP_ORDERLINE::JOB = WP_JOB::JOB)
	THEN
		CURRENT_LINE% = CURRENT_LINE% + 1%
		GOSUB DumpLines
		GOTO ReadLine
	END IF

18090	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_BODY%)::NUMBER

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_BODY%)::NUMBER

	!
	! Print the bottom of statement
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Print lines to botton of the voucher
	!
	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO FORM_GROUP(FRM_TOP%)::NUMBER

18100	!
	! Do the next group
	!
	RETURN

 DumpLines:
18200	!*******************************************************************
	! Dump all collected lines to invoice
	!*******************************************************************

	!
	! Print all the lines
	!
	!
	! Skip to a new page if necessary
	!
	IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER
	THEN
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
			FOR I% = BODY_COUNT% + 1% TO &
			FORM_GROUP(FRM_BODY%)::NUMBER

		LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_BODY%)::NUMBER

		!
		! Print the bottom of statement
		!
		LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BOTTOM%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		!
		! Print lines to botton of the voucher
		!
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
			FOR LOOP% = LINE_COUNT% + 1% TO &
			FORM_GROUP(FRM_TOP%)::NUMBER

		!
		! Print the top of statement
		!
		LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_TOP%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		BODY_COUNT% = 0%
		PAGE_NUMBER% = PAGE_NUMBER% + 1%

	END IF

	!
	! Calculate line total
	!
	LINE_TOTAL = FUNC_ROUND(WP_ORDERLINE::QTY * WP_ORDERLINE::COST, 2%)

	HEADER_TOTAL = HEADER_TOTAL + LINE_TOTAL

	V% = FUNC_INCREMENT(ROLLCOUNT$)

	PD_PRODUCT::DESCRIPTION = ""

	GOTO 18220 IF WP_ORDERLINE::TTYPE = "L"

18210	!
	! Get Product Description
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ WP_ORDERLINE::ITEMCODE, REGARDLESS
	USE
		CONTINUE 18220 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

18220	!
	! Print one line
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

18290	RETURN

	%PAGE

 LoadForm:
	!*******************************************************************
	! Initilize Statement form
	!*******************************************************************

	!
	! Get form from the WP form library
	!
	SMG_STATUS% = OUTP_FORMINIT( &
		WP_FORM.DEV$ + "WP_FORM", REPORT$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "order form is missing", "E", &
			SCOPE::PRG_PROGRAM, REPORT$, NUM1$(SMG_STATUS%))
		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_TOP% = 0%
	FRM_BODY% = 0%
	FRM_BOTTOM% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-BODY"
			FRM_BODY% = I%

		CASE "FRM-BOTTOM"
			FRM_BOTTOM% = I%

		END SELECT

	NEXT I%

	RETURN

	%PAGE

 Alignment:
	!*******************************************************************
	! Print alignment form, if desireable
	!*******************************************************************

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	PAGE_NUMBER% = 1%
	LINE_COUNT% = 0%
	BODY_COUNT% = 0%

	UTL_REPORTX::LINENO = 0%
	UTL_REPORTX::PAGENO = 0%

	SCOPE::PRG_ITEM = "ALIGNMENT"
	!++
	! Abstract:ALIGNMENT
	!
	!
	! Index:
	!
	!
	!--
	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Do you want an alignment form?  " + &
		"Confirm then press <Do> ", &
		"N", 0%, "'E", "")

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SELECT SCOPE::SCOPE_EXIT

	!
	! An exit key was typed
	!
	CASE 3%, SMG$K_TRM_CTRLZ, SMG$K_TRM_F10
		GOTO ExitProgram

	!
	! Return, etc. act as next screen
	!
	CASE 10%, 12%, 13%, SMG$K_TRM_F7, SMG$K_TRM_DO

	!
	! Case else
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Alignment

	END SELECT

	GOTO AlignmentReturn IF JUNK$ <> "Y"

	!
	! Print the top of statement
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%) &
		FOR I% = 1% TO 3%

	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_BODY%)::NUMBER

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_BODY%)::NUMBER

	!
	! Display BOTTOM
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	!
	! Print lines to bottom of the voucher
	!
	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO FORM_GROUP(FRM_TOP%)::NUMBER

	!
	! Do they need another?
	!
	GOTO Alignment

 AlignmentReturn:
	RETURN

	%PAGE

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME 19990

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	!
	! Handle the cases where a file couldn't be opened
	!
	END

20000	SUB FORM_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)	WP_JOB_CDD	WP_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE


	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD	WP_REGLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP	(JC_TYPE)	JC_TYPE_CDD	JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP	(JC_CLASS)	JC_CLASS_CDD	JC_CLASS

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
		MAX_LINE%, &
		HEADER_TOTAL, &
		LINE_TOTAL, &
		ROLLCOUNT$ = 4%, &
		STATEMENT_DATE$ = 8%, &
		PAGE_NUMBER%

	%PAGE

	!
	! Set up default values
	!
	REALVALUE = 0.0
	TEXTVALUE$ = "????????"

	!
	! Pick by variable
	!
	SELECT VARNAME$

	!************************************************************
	! Fields for the Order Entery Order Journal Header file
	!************************************************************

	CASE "WP_JOB::JOB"
		TEXTVALUE$ = WP_JOB::JOB

	CASE "WP_JOB::BDATE"
		TEXTVALUE$ = PRNT_DATE(WP_JOB::BDATE, 8%)

	CASE "WP_JOB::TTYPE"
		TEXTVALUE$ = WP_JOB::TTYPE

	CASE "WP_JOB::CLASS"
		TEXTVALUE$ = WP_JOB::CLASS

	CASE "WP_JOB::LOCATION"
		TEXTVALUE$ = WP_JOB::LOCATION

	CASE "WP_JOB::OPERATOR"
		TEXTVALUE$ = WP_JOB::OPERATOR

	CASE "WP_JOB::NOTE1"
		TEXTVALUE$ = WP_JOB::NOTES(0%)

	CASE "WP_JOB::NOTE2"
		TEXTVALUE$ = WP_JOB::NOTES(1%)

	CASE "WP_JOB::REFNO"
		TEXTVALUE$ = WP_JOB::REFNO

	CASE "WP_JOB::DESCR"
		TEXTVALUE$ = WP_JOB::DESCR

	!************************************************************
	! Fields for the Order Entry Order Line file
	!************************************************************

	CASE "WP_ORDERLINE::JOB"
		TEXTVALUE$ = WP_ORDERLINE::JOB

	CASE "WP_ORDERLINE::ITEMCODE"
		TEXTVALUE$ = WP_ORDERLINE::ITEMCODE

	CASE "WP_ORDERLINE::QTY"
		REALVALUE  = WP_ORDERLINE::QTY

	CASE "WP_ORDERLINE::COST"
		REALVALUE  = WP_ORDERLINE::COST

	CASE "WP_ORDERLINE::START_DATE"
		TEXTVALUE$ = PRNT_DATE(WP_ORDERLINE::START_DATE, 8%)

	CASE "WP_ORDERLINE::COMP_DATE"
		TEXTVALUE$ = PRNT_DATE(WP_ORDERLINE::COMP_DATE, 8%)

	CASE "WP_ORDERLINE::DESCR"
		TEXTVALUE$ = WP_ORDERLINE::DESCR

	CASE "WP_ORDERLINE::TTYPE"
		TEXTVALUE$ = WP_ORDERLINE::TTYPE

	!************************************************************
	! Fields for the Product Description file
	!************************************************************

	CASE "PD_PRODUCT::PRODUCT_NUM"
		TEXTVALUE$ = PD_PRODUCT::PRODUCT_NUM

	CASE "PD_PRODUCT::DESCRIPTION"
		TEXTVALUE$ = PD_PRODUCT::DESCRIPTION

	CASE "PD_PRODUCT::PROD_TYPE"
		TEXTVALUE$ = PD_PRODUCT::PROD_TYPE

	CASE "PD_PRODUCT::CATEGORY"
		TEXTVALUE$ = PD_PRODUCT::CATEGORY

	CASE "PD_PRODUCT::UOM"
		TEXTVALUE$ = PD_PRODUCT::UOM

	CASE "PD_PRODUCT::PACK"
		TEXTVALUE$ = PD_PRODUCT::PACK

	CASE "PD_PRODUCT::LABEL"
		TEXTVALUE$ = PD_PRODUCT::LABEL

	CASE "PD_PRODUCT::METHOD"
		TEXTVALUE$ = PD_PRODUCT::METHOD

	CASE "PD_PRODUCT::BDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT::BDATE, 8%)

	CASE "PD_PRODUCT::SSTATUS"
		TEXTVALUE$ = PD_PRODUCT::SSTATUS

	CASE "PD_PRODUCT::EDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT::EDATE, 8%)

	CASE "PD_PRODUCT::SECONDARY_CODE"
		TEXTVALUE$ = PD_PRODUCT::SECONDARY_CODE

	!***********************************************************
	! Fields for Location Descriptions
	!***********************************************************

	CASE "UTL_LOCATION::LOCNAME"
		TEXTVALUE$ = UTL_LOCATION::LOCNAME

	CASE "UTL_LOCATION::ADDRESS1"
		TEXTVALUE$ = UTL_LOCATION::ADDRESS1

	CASE "UTL_LOCATION::ADDRESS2"
		TEXTVALUE$ = UTL_LOCATION::ADDRESS2

	CASE "UTL_LOCATION::CITY"
		TEXTVALUE$ = UTL_LOCATION::CITY

	CASE "UTL_LOCATION::STATE"
		TEXTVALUE$ = UTL_LOCATION::STATE

	CASE "UTL_LOCATION::ZIP"
		TEXTVALUE$ = UTL_LOCATION::ZIP

	!***********************************************************
	! Fields for Job Type Descriptions
	!***********************************************************

	CASE "JC_TYPE::DESCR"
		TEXTVALUE$ = JC_TYPE::DESCR

	!***********************************************************
	! Fields for Job Class Descriptions
	!***********************************************************

	CASE "JC_CLASS::DESCR"
		TEXTVALUE$ = JC_CLASS::DESCR

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "HEADER_TOTAL" ! Total of the Job
		REALVALUE = HEADER_TOTAL

	CASE "LINE_TOTAL" ! Total of the Job Line
		REALVALUE = LINE_TOTAL

	CASE "STATEMENT_DATE" ! Report Date for Form
		TEXTVALUE$ = PRNT_DATE(STATEMENT_DATE$, 6%)

	CASE "PAGE_NUMBER" ! Page Number of Form
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "ROLLCOUNT" ! Job Line number (assigned when printed)
		TEXTVALUE$ = ROLLCOUNT$

	END SELECT

	END SUB
