1	%TITLE "Print Invoice Form"
	%SBTTL "AR_OUTP_SJ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_OUTP_SJ(STRING INVOICE, STRING BATCH, LONG FLAG)

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:ARINV
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
	!	$ BAS AR_SOURCE:AR_OUTP_SJ/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AR_OUTP_SJ
	!	$ DELETE AR_OUTP_SJ.OBJ;*
	!
	! Author:
	!
	!	01/25/93 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/02/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/09/97 - Kevin Handy
	!		Lose PRNT.CH% and JJ$ variables.
	!
	!	09/08/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/28/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.HB"
	MAP (AR_SJH)		AR_SJH_CDD		AR_SJH

	%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.HB"
	MAP (AR_SJL)		AR_SJL_CDD		AR_SJL

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

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
	DECLARE STRING CUSTOM_NUM
	DECLARE STRING INV_ARRAY(50%)

	MAP (JOUR_FORM) &
		AR_35CUSTOM.ADDLINE$(4%) = 50%, &
		SAL_AMT, &
		SAL_QTY, &
		DIS_AMT, &
		DIS_QTY, &
		FGT_AMT, &
		FGT_QTY, &
		TAX_AMT, &
		TAX_QTY, &
		COS_AMT, &
		COS_QTY, &
		INV_AMT, &
		INV_QTY, &
		OTH_AMT, &
		OTH_QTY, &
		PAGE_NUMBER%, &
		BATCH_NO$

	COM (CH_AR_SJH) AR_SJH.CH%
	COM (CH_AR_SJL) AR_SJL.CH%

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

	REPORT$ = "ARINVO"

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
	SCOPE::PRG_PROGRAM = "AR_OUTP_SJ"
	SCOPE::PRG_IDENT   = "H"

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
	IF AR_SJH.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.MOD"
		USE
			FILENAME$ = "AR_SJH_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

620	!
	! Open Line file
	!
	IF AR_SJL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.MOD"
		USE
			FILENAME$ = "AR_SJL_" + BATCH_NO$
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
			RESET #AR_SJH.CH%, KEY #0%
		ELSE
			FIND #AR_SJH.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AR_SJH"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #AR_SJH.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "AR_SJH"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF AR_SJH::INVNUM > TO_ITEM$ AND TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" &
		AND COMP_ARRAY(EDIT$(AR_SJH::INVNUM, -1%), &
		WLDCRD$) = 0%

	!
	! Account Receivable Customer File
	!
	V% = AR_EXAM_CUSTOM(AR_SJH::CUSNUM, AR_35CUSTOM_EXAM)

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
	V% = GL_EXAM_CHART(AR_SJH::ARACCT, GL_CHART_EXAM)

	!
	! We can now print the top of the form
	!
	LINE_COUNT%  = 0%
	BODY_COUNT%  = 0%
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
	SAL_AMT, DIS_AMT, FGT_AMT, TAX_AMT, COS_AMT, INV_AMT, OTH_AMT = 0.0
	SAL_QTY, DIS_QTY, FGT_QTY, TAX_QTY, COS_QTY, INV_QTY, OTH_QTY = 0.0

	!
	! Find the frist record
	!
18000	WHEN ERROR IN
		FIND #AR_SJL.CH%, KEY #0% EQ AR_SJH::INVNUM, REGARDLESS
	USE
		CONTINUE ExitStmt IF ERR = 155%
		FILENAME$ = "AR_SJL"
		CONTINUE HelpError
	END WHEN

	!
	! Get the lines for the header
	!
 ReadLine:
18010	WHEN ERROR in
		GET #AR_SJL.CH%, REGARDLESS
	USE
		CONTINUE ExitStmt IF ERR = 11%
		FILENAME$ = "AR_SJL"
		CONTINUE HelpError
	END WHEN

	GOTO ExitStmt IF AR_SJL::INVNUM <> AR_SJH::INVNUM

	SELECT AR_SJL::LTYPE

	CASE "S"	! Sales
		SAL_AMT = SAL_AMT + AR_SJL::AMOUNT
		SAL_QTY = SAL_QTY + AR_SJL::QTY

	CASE "D"	! Discount
		DIS_AMT = DIS_AMT + AR_SJL::AMOUNT
		DIS_QTY = DIS_QTY + AR_SJL::QTY
		GOTO ReadLine

	CASE "F"	! Freight
		FGT_AMT = FGT_AMT + AR_SJL::AMOUNT
		FGT_QTY = FGT_QTY + AR_SJL::QTY
		GOTO ReadLine

	CASE "T"	! Salestax
		TAX_AMT = TAX_AMT + AR_SJL::AMOUNT
		TAX_QTY = TAX_QTY + AR_SJL::QTY
		GOTO ReadLine

	CASE "C"	! Cost of Sales
		COS_AMT = COS_AMT + AR_SJL::AMOUNT
		COS_QTY = COS_QTY + AR_SJL::QTY
		GOTO ReadLine

	CASE "I"	! Inventory
		INV_AMT = INV_AMT + AR_SJL::AMOUNT
		INV_QTY = INV_QTY + AR_SJL::QTY
		GOTO ReadLine

	CASE "O"	! Other
		OTH_AMT = OTH_AMT + AR_SJL::AMOUNT
		OTH_QTY = OTH_QTY + AR_SJL::QTY
		GOTO ReadLine

	CASE ELSE	! Sales
		SAL_AMT = SAL_AMT + AR_SJL::AMOUNT
		SAL_QTY = SAL_QTY + AR_SJL::QTY

	END SELECT

	!
	! See if we have a good account number
	!
	V% = GL_EXAM_CHART(AR_SJL::ACCT, GL_CHART_EXAM)

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

		BODY_COUNT%  = 0%
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
	LINE_COUNT%  = 0%
	BODY_COUNT%  = 0%

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
		"Confirm then press <Do> ", "N", 0%, "'E", "")

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


20000	SUB AR_OUTP_SJ_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.HB"
	MAP (AR_SJH)		AR_SJH_CDD		AR_SJH

	%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.HB"
	MAP (AR_SJL)		AR_SJL_CDD		AR_SJL

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	COM (GL_CHART)		GL_CHART_CDD		GL_CHART_EXAM

	MAP (JOUR_FORM) &
		AR_35CUSTOM.ADDLINE$(4%) = 50%, &
		SAL_AMT, &
		SAL_QTY, &
		DIS_AMT, &
		DIS_QTY, &
		FGT_AMT, &
		FGT_QTY, &
		TAX_AMT, &
		TAX_QTY, &
		COS_AMT, &
		COS_QTY, &
		INV_AMT, &
		INV_QTY, &
		OTH_AMT, &
		OTH_QTY, &
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

	CASE "AR_SJH::INVNUM"
		TEXTVALUE$ = AR_SJH::INVNUM

	CASE "AR_SJH::CUSNUM"
		TEXTVALUE$ = AR_SJH::CUSNUM

	CASE "AR_SJH::TRATYP"
		TEXTVALUE$ = AR_SJH::TRATYP

	CASE "AR_SJH::TRADAT"
		TEXTVALUE$ = PRNT_DATE(AR_SJH::TRADAT, 8%)

	CASE "AR_SJH::TRADAT6"
		TEXTVALUE$ = PRNT_DATE(AR_SJH::TRADAT, 6%)

	CASE "AR_SJH::AMOUNT"
		REALVALUE = AR_SJH::AMOUNT

	CASE "AR_SJH::ARACCT"
		TEXTVALUE$ = AR_SJH::ARACCT

	CASE "AR_SJH::RECNUM"
		TEXTVALUE$ = AR_SJH::RECNUM

	CASE "AR_SJH::CHECK"
		TEXTVALUE$ = AR_SJH::CHECK

	CASE "AR_SJH::DEPOSIT"
		TEXTVALUE$ = AR_SJH::DEPOSIT

	CASE "AR_SJH::DESCR"
		TEXTVALUE$ = AR_SJH::DESCR

	!************************************************************
	! Fields for the Line file
	!************************************************************

	CASE "AR_SJL::INVNUM"
		TEXTVALUE$ = AR_SJL::INVNUM

	CASE "AR_SJL::SLINE"
		TEXTVALUE$ = AR_SJL::SLINE

	CASE "AR_SJL::ACCT"
		TEXTVALUE$ = AR_SJL::ACCT

	CASE "AR_SJL::SUBACCT"
		TEXTVALUE$ = AR_SJL::SUBACCT

	CASE "AR_SJL::AMOUNT"
		REALVALUE  = AR_SJL::AMOUNT

	CASE "AR_SJL::QTY"
		REALVALUE  = AR_SJL::QTY

	CASE "AR_SJL::LTYPE"
		TEXTVALUE$ = AR_SJL::LTYPE

	CASE "AR_SJL::TAXTYP"
		TEXTVALUE$ = AR_SJL::TAXTYP

	CASE "AR_SJL::DESCR"
		TEXTVALUE$ = AR_SJL::DESCR

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
		REALVALUE  = AR_35CUSTOM_EXAM::CREDITLIM

	CASE "AR_35CUSTOM::DISCOUNT"
		REALVALUE  = AR_35CUSTOM_EXAM::DISCOUNT

	CASE "AR_35CUSTOM::BACKORDER"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::BACKORDER

	CASE "AR_35CUSTOM::TAXFLAG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXFLAG

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

	CASE "SAL_AMT"
		REALVALUE = SAL_AMT

	CASE "SAL_QTY"
		REALVALUE = SAL_QTY

	CASE "DIS_AMT"
		REALVALUE = DIS_AMT

	CASE "DIS_QTY"
		REALVALUE = DIS_QTY

	CASE "FGT_AMT"
		REALVALUE = FGT_AMT

	CASE "FGT_QTY"
		REALVALUE = FGT_QTY

	CASE "TAX_AMT"
		REALVALUE = TAX_AMT

	CASE "TAX_QTY"
		REALVALUE = TAX_QTY

	CASE "COS_AMT"
		REALVALUE = COS_AMT

	CASE "COS_QTY"
		REALVALUE = COS_QTY

	CASE "INV_AMT"
		REALVALUE = INV_AMT

	CASE "INV_QTY"
		REALVALUE = INV_QTY

	CASE "OTH_AMT"
		REALVALUE = OTH_AMT

	CASE "OTH_QTY"
		REALVALUE = OTH_QTY

	CASE "PAGE_NUMBER"
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "BATCH"
		TEXTVALUE$ = BATCH_NO$

	END SELECT

	TEXTVALUE$ = EDIT$(TEXTVALUE$, 8% + 128%)

	END SUB
