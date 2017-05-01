1	%TITLE "Print Invoice Form"
	%SBTTL "AR_OUTP_CRJ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_OUTP_CRJ(STRING INVOICE, STRING BATCH, LONG FLAG)

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
	! ID:ARCRJ
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	This program prints an Invoice form from the AR
	!	Cash Receipts Journal
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
	!	$ BAS AR_SOURCE:AR_OUTP_CRJ/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AR_OUTP_CRJ
	!	$ DELETE AR_OUTP_CRJ.OBJ;*
	!
	! Author:
	!
	!	02/11/2002 - Kevin Handy
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
	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.HB"
	MAP (AR_CRJH)		AR_CRJH_CDD		AR_CRJH

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.HB"
	MAP (AR_CRJL)		AR_CRJL_CDD		AR_CRJL

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	COM (GL_CHART)		GL_CHART_CDD		GL_CHART

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
	DECLARE STRING CUSTOM_NUM
	DECLARE STRING INV_ARRAY(50%)

	MAP (JOUR_FORM) &
		AR_35CUSTOM.ADDLINE$(4%) = 50%, &
		TOTAL_AMT, &
		PAGE_NUMBER%, &
		BATCH_NO$

	COM (CH_AR_CRJH) AR_CRJH.CH%
	COM (CH_AR_CRJL) AR_CRJL.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG   FUNCTION GL_EXAM_CHART
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG   FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	REPORT$ = "ARCRJ"

	!
	! Look up device
	!
	CALL READ_DEVICE("AR_FORM", AR_FORM.DEV$, STAT%)

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
	SCOPE::PRG_PROGRAM = "AR_OUTP_CRJ"
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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 128%)

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

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 128%)

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
	IF AR_CRJH.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.MOD"
		USE
			FILENAME$ = "AR_CRJH_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

620	!
	! Open Line file
	!
	IF AR_CRJL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.MOD"
		USE
			FILENAME$ = "AR_CRJL_" + BATCH_NO$
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
	TESTCUSNUM$ = ""
	HANDLING, MISC, FREIGHT = 0.0

2000	!*******************************************************************
	! Read through header file
	!*******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_CRJH.CH%, KEY #0%
		ELSE
			FIND #AR_CRJH.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AR_CRJH"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #AR_CRJH.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "AR_CRJH"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF AR_CRJH::RECNUM > TO_ITEM$ AND TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" &
		AND COMP_ARRAY(EDIT$(AR_CRJH::RECNUM, -1%), &
		WLDCRD$) = 0%

	!
	! Account Receivable Customer File
	!
	V% = AR_EXAM_CUSTOM(AR_CRJH::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(AR_35CUSTOM_EXAM::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%

		AR_35CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM_EXAM::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM_EXAM::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%

		AR_35CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM_EXAM::ADD2, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM_EXAM::ADD3, -1%) <> ""
	THEN
		I% = I% + 1%

		AR_35CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM_EXAM::ADD3, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	AR_35CUSTOM.ADDLINE$(I%) = &
		EDIT$(EDIT$(AR_35CUSTOM_EXAM::CITY, 128%) + &
		", " + AR_35CUSTOM_EXAM::STATE + " " + &
		AR_35CUSTOM_EXAM::ZIP + " " + &
		AR_35CUSTOM_EXAM::COUNTRY, 8% + 16% + 32% + 128%)

	AR_35CUSTOM.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 4%	! Blank 'em out

	!
	! See if we have good account numbers
	!
	V% = GL_EXAM_CHART(AR_CRJH::ACCT, GL_CHART)

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
	TOTAL_AMT = 0.0

	!
	! Find the frist record
	!
18000	WHEN ERROR IN
		FIND #AR_CRJL.CH%, KEY #0% EQ AR_CRJH::RECNUM, REGARDLESS
	USE
		CONTINUE ExitStmt IF ERR = 155%
		FILENAME$ = "AR_CRJL"
		CONTINUE HelpError
	END WHEN

	!
	! Get the lines for the header
	!
 ReadLine:
18010	WHEN ERROR in
		GET #AR_CRJL.CH%, REGARDLESS
	USE
		CONTINUE ExitStmt IF ERR = 11%
		FILENAME$ = "AR_CRJL"
		CONTINUE HelpError
	END WHEN

	GOTO ExitStmt IF AR_CRJL::RECNUM <> AR_CRJH::RECNUM

	TOTAL_AMT = TOTAL_AMT + AR_CRJL::AMOUNT

	!
	! See if we have a good account number
	!
	V% = GL_EXAM_CHART(AR_CRJL::ACCT, GL_CHART)

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
		AR_FORM.DEV$ + "AR_FORM", REPORT$, &
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


20000	SUB AR_OUTP_CRJ_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.HB"
	MAP (AR_CRJH)		AR_CRJH_CDD		AR_CRJH

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.HB"
	MAP (AR_CRJL)		AR_CRJL_CDD		AR_CRJL

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	COM (GL_CHART)		GL_CHART_CDD		GL_CHART

	MAP (JOUR_FORM) &
		AR_35CUSTOM.ADDLINE$(4%) = 50%, &
		TOTAL_AMT, &
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

	CASE "AR_CRJH::RECNUM"
		TEXTVALUE$ = AR_CRJH::RECNUM

	CASE "AR_CRJH::CUSNUM"
		TEXTVALUE$ = AR_CRJH::CUSNUM

	CASE "AR_CRJH::CHECK"
		TEXTVALUE$ = AR_CRJH::CHECK

	CASE "AR_CRJH::DEPOSIT"
		TEXTVALUE$ = AR_CRJH::DEPOSIT

	CASE "AR_CRJH::TRADAT"
		TEXTVALUE$ = PRNT_DATE(AR_CRJH::TRADAT, 8%)

	CASE "AR_CRJH::TRADAT6"
		TEXTVALUE$ = PRNT_DATE(AR_CRJH::TRADAT, 6%)

	CASE "AR_CRJH::ACCT"
		TEXTVALUE$ = AR_CRJH::ACCT

	CASE "AR_CRJH::AMNT"
		REALVALUE = AR_CRJH::AMNT

	CASE "AR_CRJH::TRATYP"
		TEXTVALUE$ = AR_CRJH::TRATYP

	CASE "AR_CRJH::DESCR"
		TEXTVALUE$ = AR_CRJH::DESCR

	!************************************************************
	! Fields for the Line file
	!************************************************************

	CASE "AR_CRJL::RECNUM"
		TEXTVALUE$ = AR_CRJL::RECNUM

	CASE "AR_CRJL::LLINE"
		TEXTVALUE$ = AR_CRJL::LLINE

	CASE "AR_CRJL::INVNUM"
		TEXTVALUE$ = AR_CRJL::INVNUM

	CASE "AR_CRJL::ACCT"
		TEXTVALUE$ = AR_CRJL::ACCT

	CASE "AR_CRJL::AMOUNT"
		REALVALUE = AR_CRJL::AMOUNT

	CASE "AR_CRJL::TRATYP"
		TEXTVALUE$ = AR_CRJL::TRATYP

	CASE "AR_CRJL::SALNUM"
		TEXTVALUE$ = AR_CRJL::SALNUM

	!************************************************************
	! Fields for the Accounts Receivable Ver. 3.5 Customer file
	!************************************************************

	CASE "AR_35CUSTOM::CUSNUM"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CUSNUM

	CASE "AR_35CUSTOM::CUSNAM"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CUSNAM

	CASE "AR_35CUSTOM::TTYPE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TTYPE

	CASE "AR_35CUSTOM::CATEGORY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CATEGORY

	CASE "AR_35CUSTOM::BDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::BDATE, 8%)

	CASE "AR_35CUSTOM::BDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::BDATE, 6%)

	CASE "AR_35CUSTOM::SSTATUS"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SSTATUS

	CASE "AR_35CUSTOM::EDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::EDATE, 8%)

	CASE "AR_35CUSTOM::EDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::EDATE, 6%)

	CASE "AR_35CUSTOM::ADD1"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ADD1

	CASE "AR_35CUSTOM::ADD2"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ADD2

	CASE "AR_35CUSTOM::ADD3"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ADD3

	CASE "AR_35CUSTOM::CITY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CITY

	CASE "AR_35CUSTOM::STATE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::STATE

	CASE "AR_35CUSTOM::ZIP"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ZIP

	CASE "AR_35CUSTOM::COUNTRY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::COUNTRY

	CASE "AR_35CUSTOM::COUNTY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::COUNTY

	CASE "AR_35CUSTOM:ADDLINE1"	! Substitute Customer Address
		TEXTVALUE$ = AR_35CUSTOM.ADDLINE$(1%)

	CASE "AR_35CUSTOM:ADDLINE2"	! Substitute Customer Address
		TEXTVALUE$ = AR_35CUSTOM.ADDLINE$(2%)

	CASE "AR_35CUSTOM:ADDLINE3"	! Substitute Customer Address
		TEXTVALUE$ = AR_35CUSTOM.ADDLINE$(3%)

	CASE "AR_35CUSTOM:ADDLINE4"	! Substitute Customer Address
		TEXTVALUE$ = AR_35CUSTOM.ADDLINE$(4%)

	CASE "AR_35CUSTOM::PHONE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::PHONE

	CASE "AR_35CUSTOM::METHOD"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::METHOD

	CASE "AR_35CUSTOM::STMTFLG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::STMTFLG

	CASE "AR_35CUSTOM::ALPSRT"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ALPSRT

	CASE "AR_35CUSTOM::SERCHRG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SERCHRG

	CASE "AR_35CUSTOM::TAXCODE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXCODE

	CASE "AR_35CUSTOM::TAXEXEMP"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXEXEMP

	CASE "AR_35CUSTOM::LOCATION"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::LOCATION

	CASE "AR_35CUSTOM::TERMS"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TERMS

	CASE "AR_35CUSTOM::CARRIER"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CARRIER

	CASE "AR_35CUSTOM::SALESMAN"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SALESMAN

	CASE "AR_35CUSTOM::CREDITLIM"
		REALVALUE = AR_35CUSTOM_EXAM::CREDITLIM

	CASE "AR_35CUSTOM::DISCOUNT"
		REALVALUE = AR_35CUSTOM_EXAM::DISCOUNT

	CASE "AR_35CUSTOM::BACKORDER"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::BACKORDER

	CASE "AR_35CUSTOM::TAXFLAG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXFLAG

	!************************************************************
	! Fields for the Chart of Accounts file
	!************************************************************

	CASE "GL_CHART::ACCT"
		TEXTVALUE$ = GL_CHART::ACCT

	CASE "GL_CHART::DESCR"
		TEXTVALUE$ = GL_CHART::DESCR

	CASE "GL_CHART::ACCTYPE"
		TEXTVALUE$ = GL_CHART::ACCTYPE

	CASE "GL_CHART::FLOW"
		TEXTVALUE$ = GL_CHART::FLOW

	CASE "GL_CHART::WORK"
		TEXTVALUE$ = GL_CHART::WORK

	CASE "GL_CHART::FINTYPE"
		TEXTVALUE$ = GL_CHART::FINTYPE

	CASE "GL_CHART::SUMMARY"
		TEXTVALUE$ = GL_CHART::SUMMARY

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "TOTAL_AMT"
		REALVALUE = TOTAL_AMT

	CASE "PAGE_NUMBER"
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "BATCH"
		TEXTVALUE$ = BATCH_NO$

	END SELECT

	TEXTVALUE$ = EDIT$(TEXTVALUE$, 8% + 128%)

	END SUB
