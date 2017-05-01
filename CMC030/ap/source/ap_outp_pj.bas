1	%TITLE "Print Invoice Form"
	%SBTTL "AP_OUTP_PJ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_OUTP_PJ(STRING INVOICE, STRING BATCH, LONG FLAG)

	!
	! COPYRIGHT (C) 2002 BY
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:APINV
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	This program prints an Invoice form from the AR Sales Journal
	!	from a print request in the Journal maintenance process.
	!	.LM -5
	!
	! Index:
	!	.x Print>Invoice Form
	!	.x Invoice Form
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_OUTP_PJ/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_OUTP_PJ
	!	$ DELETE AP_OUTP_PJ.OBJ;*
	!
	! Author:
	!
	!	02/12/2002 - Kevin Handy
	!
	! Modification history:
	!
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
	%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.HB"
	MAP (AP_PJH)		AP_PJH_CDD		AP_PJH

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.HB"
	MAP (AP_PJL)		AP_PJL_CDD		AP_PJL

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	COM (AP_VENDOR_EXAM)	AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	COM (GL_CHART)		GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT
	DECLARE			UTL_REPORT_CDD		UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM	FORM_GROUP_CDD		FORM_GROUP(10%)	! Max of 10 groups

	!
	! Declare variables
	!
	DECLARE STRING ORDER_NUM
	DECLARE STRING VENDOR_NUM
	DECLARE STRING INV_ARRAY(50%)

	MAP (JOUR_FORM) &
		AP_VENDOR.ADDLINE$(4%) = 50%, &
		TOTAL_AMT, &
		TOTAL_QTY, &
		DIS_AMT, &
		DIS_QTY, &
		PAGE_NUMBER%, &
		BATCH_NO$

	COM (CH_AP_PJH) AP_PJH.CH%
	COM (CH_AP_PJL) AP_PJL.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG   FUNCTION GL_EXAM_CHART
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG   FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	REPORT$ = "APINVO"

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_FORM", AP_FORM.DEV$, STAT%)

	!***************************************************************
	! Open Report files
	!***************************************************************

	!
	! Set report fields to pass to function
	!
	SETCHECK$ = "00" + BATCH + ",01" + INVOICE + ",02" + INVOICE
	SETCHECK$ = SETCHECK$ + ",DIRECT" IF FLAG = 1%

	!
	! Get Report
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, &
		SETCHECK$) <> CMC$_NORMAL

	!
	! store original values for the help message
	!
	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	SCOPE::PRG_PROGRAM = "AP_OUTP_PJ"
	SCOPE::PRG_IDENT = "H"

	!
	! Get user input for report settings
	!
	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Batch Number	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field allows for entry of a selected
	!	batch number. Each Journal File is assigned
	!	a user batch number consisting of two (2) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) From Invoice\*
	!	.b
	!	.lm +5
	!	The ^*From Invoice\* entered in this field will cause the
	!	printing to begin with the selected
	!	invoice.
	!	.b
	!	A blank field will cause the form to start with
	!	the first invoice in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) To Invoice\*
	!	.b
	!	.lm +5
	!	The ^*To Invoice\* entered in this field will cause the
	!	printing to end with the selected
	!	invoice.
	!	.b
	!	A blank field will cause the form to end with the
	!	last invoice in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "wildcard" for Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(4%))

	!++
	! Abstract:FLD05
	!	^*(05) Form\*
	!	.b
	!	.lm +5
	!	The ^*Forms\* option will allow the user to enter the form number for
	!	printing. (See Forms Controlling under Utility Section.)
	!	.lm -5
	!
	! Index:
	!
	!--

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!***************************************************************
	! Open all other files
	!***************************************************************

610	!
	! Open Header file
	!
	IF AP_PJH.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.MOD"
		USE
			FILENAME$ = "AP_PJH_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

620	!
	! Open Line file
	!
	IF AP_PJL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.MOD"
		USE
			FILENAME$ = "AP_PJL_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

 EndOpen:

	!
	! Load In The Form
	!
	GOSUB LoadForm

	!
	! Go check out aligment routine
	!
	GOSUB Alignment IF (FLAG AND 1%) = 0%

	%PAGE

	!
	! Initialize some variables
	!
	TESTVENNUM$ = ""
	HANDLING, MISC, FREIGHT = 0.0

2000	!*******************************************************************
	! Read through header file
	!*******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AP_PJH.CH%, KEY #0%
		ELSE
			FIND #AP_PJH.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AP_PJH"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #AP_PJH.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "AP_PJH"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF AP_PJH::TRANKEY > TO_ITEM$ AND TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" &
		AND COMP_ARRAY(EDIT$(AP_PJH::TRANKEY, -1%), &
		WLDCRD$) = 0%

	!
	! Account Payable VENDOR File
	!
	V% = AP_EXAM_VENDOR(AP_PJH::VENNUM, AP_VENDOR_EXAM)

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(AP_VENDOR_EXAM::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%

		AP_VENDOR.ADDLINE$(I%) = &
			EDIT$(AP_VENDOR_EXAM::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AP_VENDOR_EXAM::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%

		AP_VENDOR.ADDLINE$(I%) = &
			EDIT$(AP_VENDOR_EXAM::ADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	AP_VENDOR.ADDLINE$(I%) = &
		EDIT$(EDIT$(AP_VENDOR_EXAM::CITY, 128%) + &
		", " + AP_VENDOR_EXAM::STATE + " " + &
		AP_VENDOR_EXAM::ZIP + " " + &
		AP_VENDOR_EXAM::COUNTRY, 8% + 16% + 32% + 128%)

	AP_VENDOR.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 4%	! Blank 'em out

	!
	! See if we have good account numbers
	!
	V% = GL_EXAM_CHART(AP_PJH::AP_ACCT, GL_CHART_EXAM)

	!
	! We can now print the top of the form
	!
	LINE_COUNT% = 0%
	BODY_COUNT% = 0%
	PAGE_NUMBER% = 1%

	!
	! Print the top of statement
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	GOSUB PrintStmt

	!
	! Print lines to bottom of statement
	!
	FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_BODY%)::NUMBER
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%)
	NEXT I%

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

	CALL OUTP_NEWPAGE(UTL_REPORTX)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Go get the next record
	!
	GOTO GetNextRec

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	CLOSE UTL_REPORT.CH%
	CALL ASSG_FREECHANNEL(UTL_REPORT.CH%)

	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	EXIT FUNCTION

	%PAGE

 PrintStmt:
	!***************************************************************
	! Print the Statement now
	!***************************************************************
	!
	! Initialize variables
	!
	TOTAL_AMT, DIS_AMT = 0.0
	TOTAL_QTY, DIS_QTY = 0.0

	!
	! Find the frist record
	!
18000	WHEN ERROR IN
		FIND #AP_PJL.CH%, KEY #0% EQ AP_PJH::TRANKEY, REGARDLESS
	USE
		CONTINUE ExitStmt IF ERR = 155%
		FILENAME$ = "AP_PJL"
		CONTINUE HelpError
	END WHEN

	!
	! Get the lines for the header
	!
 ReadLine:
18010	WHEN ERROR in
		GET #AP_PJL.CH%, REGARDLESS
	USE
		CONTINUE ExitStmt IF ERR = 11%
		FILENAME$ = "AP_PJL"
		CONTINUE HelpError
	END WHEN

	GOTO ExitStmt IF AP_PJL::TRANKEY <> AP_PJH::TRANKEY

	TOTAL_AMT = TOTAL_AMT - AP_PJL::AMOUNT
	TOTAL_QTY = TOTAL_QTY + AP_PJL::UNITS

	!
	! See if we have a good account number
	!
	V% = GL_EXAM_CHART(AP_PJL::ACCT, GL_CHART_EXAM)

	!*******************************************************************
	! Dump lines to invoice
	!*******************************************************************

	!
	! Skip to a new page if necessary
	!
	IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER
	THEN
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
			FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_BODY%)::NUMBER

		LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_BODY%)::NUMBER

		!
		! Print the bottom of statement
		!
		LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_SUBBOTTOM%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		CALL OUTP_NEWPAGE(UTL_REPORTX)

		BODY_COUNT% = 0%
		PAGE_NUMBER% = PAGE_NUMBER% + 1%

		!
		! Print the top of statement
		!
		LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_TOP%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

	END IF

	!
	! Print one line
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	GOTO ReadLine

 ExitStmt:
	!
	! Do the next group
	!
	RETURN

	%PAGE

 LoadForm:
	!*******************************************************************
	! Initilize Statement form
	!*******************************************************************

	!
	! Get form from the OE form library
	!
	SMG_STATUS% = OUTP_FORMINIT( &
		AP_FORM.DEV$ + "AP_FORM", REPORT$, &
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
	FRM_SUBBOTTOM% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-BODY"
			FRM_BODY% = I%

		CASE "FRM-BOTTOM"
			FRM_BOTTOM% = I%

		CASE "FRM-SUBBOTTOM"
			FRM_SUBBOTTOM% = I%

		END SELECT

	NEXT I%

	IF FRM_SUBBOTTOM% = 0%
	THEN
		FRM_SUBBOTTOM% = FRM_BOTTOM%
	END IF

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
		"Do you want an alignment form?  Confirm then press <Do> ", &
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
		FOR I% = 1% TO FORM_GROUP(FRM_BODY%)::NUMBER

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
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

	CALL OUTP_NEWPAGE(UTL_REPORTX)

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

	END FUNCTION


20000	SUB AP_OUTP_PJ_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.HB"
	MAP (AP_PJH)		AP_PJH_CDD		AP_PJH

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.HB"
	MAP (AP_PJL)		AP_PJL_CDD		AP_PJL

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	COM (AP_VENDOR_EXAM)	AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	COM (GL_CHART)		GL_CHART_CDD		GL_CHART_EXAM

	MAP (JOUR_FORM) &
		AP_VENDOR.ADDLINE$(4%) = 50%, &
		TOTAL_AMT, &
		TOTAL_QTY, &
		DIS_AMT, &
		DIS_QTY, &
		PAGE_NUMBER%, &
		BATCH_NO$

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
	! Fields for the Header file
	!************************************************************

	CASE "AP_PJH::TRANKEY"
		TEXTVALUE$ = AP_PJH::TRANKEY

	CASE "AP_PJH::VENNUM"
		TEXTVALUE$ = AP_PJH::VENNUM

	CASE "AP_PJH::TRANKEY_DATE"
		TEXTVALUE$ = PRNT_DATE(AP_PJH::TRANKEY_DATE, 8%)

	CASE "AP_PJH::INVNUM"
		TEXTVALUE$ = AP_PJH::INVNUM

	CASE "AP_PJH::INVDAT"
		TEXTVALUE$ = PRNT_DATE(AP_PJH::INVDAT, 8%)

	CASE "AP_PJH::INVAMT"
		REALVALUE = AP_PJH::INVAMT

	CASE "AP_PJH::CODE_1099"
		TEXTVALUE$ = AP_PJH::CODE_1099

	CASE "AP_PJH::AMT_1099"
		REALVALUE = AP_PJH::AMT_1099

	CASE "AP_PJH::USE_JOB_NUM"
		TEXTVALUE$ = AP_PJH::USE_JOB_NUM

	CASE "AP_PJH::USE_AMT"
		REALVALUE = AP_PJH::USE_AMT

	CASE "AP_PJH::DISCDAT"
		TEXTVALUE$ = PRNT_DATE(AP_PJH::DISCDAT, 8%)

	CASE "AP_PJH::DISCAMT"
		REALVALUE = AP_PJH::DISCAMT

	CASE "AP_PJH::DUEDAT"
		TEXTVALUE$ = PRNT_DATE(AP_PJH::DUEDAT, 8%)

	CASE "AP_PJH::PONUM"
		TEXTVALUE$ = AP_PJH::PONUM

	CASE "AP_PJH::AP_ACCT"
		TEXTVALUE$ = AP_PJH::AP_ACCT

	CASE "AP_PJH::CASH_ACCT"
		TEXTVALUE$ = AP_PJH::CASH_ACCT

	CASE "AP_PJH::CKNUM"
		TEXTVALUE$ = AP_PJH::CKNUM

	CASE "AP_PJH::CKDAT"
		TEXTVALUE$ = PRNT_DATE(AP_PJH::CKDAT, 8%)

	CASE "AP_PJH::DESCR"
		TEXTVALUE$ = AP_PJH::DESCR

	CASE "AP_PJH::CKAMT"
		REALVALUE = AP_PJH::CKAMT


	!************************************************************
	! Fields for the Line file
	!************************************************************

	CASE "AP_PJL::TRANKEY"
		TEXTVALUE$ = AP_PJL::TRANKEY

	CASE "AP_PJL::SLINE"
		TEXTVALUE$ = AP_PJL::SLINE

	CASE "AP_PJL::PONUM"
		TEXTVALUE$ = AP_PJL::PONUM

	CASE "AP_PJL::PO_LINE"
		TEXTVALUE$ = AP_PJL::PO_LINE

	CASE "AP_PJL::ACCT"
		TEXTVALUE$ = AP_PJL::ACCT

	CASE "AP_PJL::SUBACC"
		TEXTVALUE$ = AP_PJL::SUBACC

	CASE "AP_PJL::OPERATION"
		TEXTVALUE$ = AP_PJL::OPERATION

	CASE "AP_PJL::UNITS"
		REALVALUE = AP_PJL::UNITS

	CASE "AP_PJL::AMOUNT"
		REALVALUE = AP_PJL::AMOUNT

	CASE "AP_PJL::DISCAMT"
		REALVALUE = AP_PJL::DISCAMT

	CASE "AP_PJL::USE_TAX_FLAG"
		TEXTVALUE$ = AP_PJL::USE_TAX_FLAG


	!************************************************************
	! Fields for the Accounts Payable VENDOR file
	!************************************************************

	CASE "AP_VENDOR::VENNUM"
		TEXTVALUE$ = AP_VENDOR_EXAM::VENNUM

	CASE "AP_VENDOR::VENNAM"
		TEXTVALUE$ = AP_VENDOR_EXAM::VENNAM

	CASE "AP_VENDOR::ADD1"
		TEXTVALUE$ = AP_VENDOR_EXAM::ADD1

	CASE "AP_VENDOR::ADD2"
		TEXTVALUE$ = AP_VENDOR_EXAM::ADD2

	CASE "AP_VENDOR::CITY"
		TEXTVALUE$ = AP_VENDOR_EXAM::CITY

	CASE "AP_VENDOR::STATE"
		TEXTVALUE$ = AP_VENDOR_EXAM::STATE

	CASE "AP_VENDOR::ZIP"
		TEXTVALUE$ = AP_VENDOR_EXAM::ZIP

	CASE "AP_VENDOR::COUNTRY"
		TEXTVALUE$ = AP_VENDOR_EXAM::COUNTRY

	CASE "AP_VENDOR:ADDLINE1"	! Substitute VENDORer Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(1%)

	CASE "AP_VENDOR:ADDLINE2"	! Substitute VENDORer Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(2%)

	CASE "AP_VENDOR:ADDLINE3"	! Substitute VENDORer Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(3%)

	CASE "AP_VENDOR:ADDLINE4"	! Substitute VENDORer Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(4%)

	CASE "AP_VENDOR::PHONE"
		TEXTVALUE$ = AP_VENDOR_EXAM::PHONE

	CASE "AP_VENDOR::POADD1"
		TEXTVALUE$ = AP_VENDOR_EXAM::POADD1

	CASE "AP_VENDOR::POADD2"
		TEXTVALUE$ = AP_VENDOR_EXAM::POADD2

	CASE "AP_VENDOR::POCITY"
		TEXTVALUE$ = AP_VENDOR_EXAM::POCITY

	CASE "AP_VENDOR::POSTATE"
		TEXTVALUE$ = AP_VENDOR_EXAM::POSTATE

	CASE "AP_VENDOR::POZIP"
		TEXTVALUE$ = AP_VENDOR_EXAM::POZIP

	CASE "AP_VENDOR::POCOUNTRY"
		TEXTVALUE$ = AP_VENDOR_EXAM::POCOUNTRY

	CASE "AP_VENDOR::POPHONE"
		TEXTVALUE$ = AP_VENDOR_EXAM::POPHONE

	CASE "AP_VENDOR::PURGE"
		TEXTVALUE$ = AP_VENDOR_EXAM::PURGE

	CASE "AP_VENDOR::FEDID"
		TEXTVALUE$ = AP_VENDOR_EXAM::FEDID

	CASE "AP_VENDOR::FLG1099"
		TEXTVALUE$ = AP_VENDOR_EXAM::FLG1099

	CASE "AP_VENDOR::DUEDAYS"
		REALVALUE = AP_VENDOR_EXAM::DUEDAYS
		TEXTVALUE$ = NUM1$(AP_VENDOR_EXAM::DUEDAYS)

	CASE "AP_VENDOR::DUEDATE"
		TEXTVALUE$ = AP_VENDOR_EXAM::DUEDATE

	CASE "AP_VENDOR::DISDAYS"
		REALVALUE = AP_VENDOR_EXAM::DISDAYS
		TEXTVALUE$ = NUM1$(AP_VENDOR_EXAM::DISDAYS)

	CASE "AP_VENDOR::DISDATE"
		TEXTVALUE$ = AP_VENDOR_EXAM::DISDATE

	CASE "AP_VENDOR::DISCPER"
		REALVALUE = AP_VENDOR_EXAM::DISCPER
		TEXTVALUE$ = NUM1$(AP_VENDOR_EXAM::DISCPER)

	CASE "AP_VENDOR::ALPSRT"
		TEXTVALUE$ = AP_VENDOR_EXAM::ALPSRT

	!************************************************************
	! Fields for the Chart of Accounts file
	!************************************************************

	CASE "GL_CHART::ACCT"
		TEXTVALUE$ = GL_CHART_EXAM::ACCT

	CASE "GL_CHART::DESCR"
		TEXTVALUE$ = GL_CHART_EXAM::DESCR

	CASE "GL_CHART::ACCTYPE"
		TEXTVALUE$ = GL_CHART_EXAM::ACCTYPE

	CASE "GL_CHART::FLOW"
		TEXTVALUE$ = GL_CHART_EXAM::FLOW

	CASE "GL_CHART::WORK"
		TEXTVALUE$ = GL_CHART_EXAM::WORK

	CASE "GL_CHART::FINTYPE"
		TEXTVALUE$ = GL_CHART_EXAM::FINTYPE

	CASE "GL_CHART::SUMMARY"
		TEXTVALUE$ = GL_CHART_EXAM::SUMMARY

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "TOTAL_AMT"
		REALVALUE = TOTAL_AMT

	CASE "TOTAL_QTY"
		REALVALUE = TOTAL_QTY

	CASE "DIS_AMT"
		REALVALUE = DIS_AMT

	CASE "DIS_QTY"
		REALVALUE = DIS_QTY

	CASE "PAGE_NUMBER"
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "BATCH"
		TEXTVALUE$ = BATCH_NO$

	END SELECT

	TEXTVALUE$ = EDIT$(TEXTVALUE$, 8% + 128%)

	END SUB
