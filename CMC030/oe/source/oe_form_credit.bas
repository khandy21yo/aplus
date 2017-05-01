1	%TITLE "Print Credit Form"
	%SBTTL "OE_FORM_CREDIT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:OEFORM
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Credit Form\* option
	!	prints the credit memos on a user defined form.  (See Forms Controlling
	!	in the Utility Section.)
	!	.lm -5
	!
	! Index:
	!	.x Print>Credit Memo Form
	!	.x Credit Memo>Form>Print
	!
	! Option:
	!
	!	OE_REPORT$JOUR
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_FORM_CREDIT/LINE
	!	$ LINK/EXECUTABLE=OE_EXE:*.EXE OE_FORM_CREDIT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_FORM_CREDIT.OBJ;*
	!
	! Author:
	!	10/20/91 - Frank F. Starman
	!		needs more work
	!
	! Modification history:
	!
	!
	!	06/16/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM function.
	!
	!	01/20/93 - Dan Perkins
	!		Allow dates to be printed in six or eight character
	!		format.  Eg. 01/01/93 or 01/01/1993.
	!		Use COMP_ARRAY instead of COMP_STRING.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/05/93 - Dan Perkins
	!		Moved line 2130 to line 18300 to get CREASON info
	!		from the creditline reason.
	!
	!	09/23/93 - Kevin Handy
	!		Removed JJ$ and READ_SYSJOB.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Changed STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	02/07/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/10/97 - Kevin Handy
	!		Lose PRNT.CH% variable
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/98 - Kevin Handy
	!		Don't erase SMG_SCREEN_DATA%, which is
	!		never created.
	!
	!	12/14/99 - Kevin Handy
	!		Map SA_SALESMAN_EXAM into it's own MAP instead of
	!		using SB_SUBACCOUNT which caused major problems.
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!
	!	07/02/2003 - Kevin Handy
	!		Updated for changes in OE_CREASON layout.
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
	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.HB"
	MAP (OE_CREDITJOUR)	OE_CREDITJOUR_CDD	OE_CREDITJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.HB"
	MAP (OE_CREDITLINE)	OE_CREDITLINE_CDD	OE_CREDITLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	COM (OE_PROMO_READ)	OE_PROMO_CDD		OE_PROMO_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	COM (PD_PRODUCT_EXAM)	PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SA_SALESMAN_EXAM)	SA_SALESMAN_CDD		SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(10%)	! Max of 10 groups

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
		MAX_LINE%, &
		HEADER_TOTAL, &
		ORDER_TOTAL, &
		CREDITLINE, &
		LINE_TOTAL, &
		REAL_PRICE, &
		STATEMENT_DATE$ = 8%, &
		PAGE_NUMBER%, &
		AR_CUSTOM.ADDLINE$(4%) = 50%, &
		OE_CREDITJOUR.ADDLINE$(4%) = 50%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL LONG    FUNCTION OE_READ_PROMO
	EXTERNAL LONG    FUNCTION OUTP_FORMINIT
	EXTERNAL LONG    FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG    FUNCTION COMP_ARRAY
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "OECRED"

	!
	! Look up device
	!
	CALL  READ_DEVICE("OE_FORM", OE_FORM.DEV$, STAT%)

	!***************************************************************
	! Open Report files
	!***************************************************************

370	!
	! Open REPORT file
	!
	GOTO ExitProgram IF OUTP_INITFORM(UTL_REPORTX, &
		REPORT$, "") <> CMC$_NORMAL

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Batch Number>Order Entry Order Form
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a
	!	batch number to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry Order Form>Batch Number
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	.x Sort by>Order Entry Order Form
	!	^*(02) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field enters the order
	!	in which the report is to print.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*O\* - Order Number
	!	.te
	!	^*T\* - Order Type
	!	.te
	!	^*N\* - Customer Number
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry Order Form>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	printing of the statement to begin with a particular
	!	item. The value must be in agreement with the value
	!	entered in field (02) Sort by.
	!	.b
	!	A blank field will cause the statement to start with
	!	the first item in the file.
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
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	printing of the statement to end with a particular
	!	item in the file. The value must be in agreement with
	!	field (02) Sort by.
	!	.b
	!	A blank field causes the statement to end with the
	!	last item in the file.
	!	.lm -5
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
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "wildcard" for
	!	Wildcarding Technique.
	!	.b
	!	For more information on "Wildcarding" techniques, refer to
	!	Appendix B.
	!	.lm -5
	!
	! Index:
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Report Name\*
	!	.b
	!	.lm +5
	!	The ^*Report Name\* option enters the name of the
	!	report to be printed.  (See Forms Controlling in the
	!	Utility Section.)
	!	.lm -5
	!
	! Index:
	!
	!--

	SELECT SORTBY$

	CASE "M"
		K_NUM% = 0%

		!
		! Routine to load left justified zeros into FROM_ITEM$
		! and TO_ITEM$ if any order numbers are entered as ranges
		!
		! Do FROM_ITEM$
		!
		IF FROM_ITEM$ <> ""
		THEN
			ONLEN% = LEN(OE_CREDITJOUR::MEMONUM)
			FILEN% = LEN(FROM_ITEM$)
			IF ONLEN% <> FILEN%
			THEN
				FOR I% = 1% TO ONLEN% - FILEN%
					FROM_ITEM$ = "0" + FROM_ITEM$
				NEXT I%
			END IF
		END IF

		!
		! Do TO_ITEM$
		!
		IF TO_ITEM$ <> ""
		THEN
			ONLEN% = LEN(OE_CREDITJOUR::MEMONUM)
			TILEN% = LEN(TO_ITEM$)
			IF ONLEN% <> TILEN%
			THEN
				FOR I% = 1% TO ONLEN% - TILEN%
					TO_ITEM$ = "0" + TO_ITEM$
				NEXT I%
			END IF
		END IF

	CASE "N"
		K_NUM% = 1%

	CASE "R"
		K_NUM% = 2%

	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!***************************************************************
	! Open all other files
	!***************************************************************

600	!
	! Open purchase order journal header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.OPN"
	USE
		FILENAME$ = "OE_CREDITJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

610	!
	! Open purchase order journal line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.OPN"
	USE
		FILENAME$ = "OE_CREDITLINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

620	!
	! Open reason file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.OPN"
	USE
		CONTINUE EndOpen IF ERR = 5%
		FILENAME$ = "OE_CREASON"
		CONTINUE HelpError
	END WHEN

 EndOpen:
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
			RESET #OE_CREDITJOUR.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_CREDITJOUR.CH%, KEY #K_NUM% GE FROM_ITEM$
		END IF
	USE
		CONTINUE 3000 IF ERR = 155%
		FILENAME$ = "OE_CREDITJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	!
	! Get next record
	!
	ORDER_TOTAL, HEADER_TOTAL = 0.0

	WHEN ERROR IN
		GET #OE_CREDITJOUR.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "OE_CREDITJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Check for end item
	!
	SELECT SORTBY$

	CASE "M"
		GOTO 3000 IF (OE_CREDITJOUR::MEMONUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_CREDITJOUR::MEMONUM, -1%), &
			WLDCRD$) = 0%

	CASE "R"
		GOTO 3000 IF (OE_CREDITJOUR::REASON > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_CREDITJOUR::REASON, -1%), &
			WLDCRD$) = 0%

	CASE "N"
		GOTO 3000 IF (OE_CREDITJOUR::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_CREDITJOUR::CUSNUM, -1%), &
			WLDCRD$) = 0%

	END SELECT

	!
	! Create an shipto address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(OE_CREDITJOUR::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		OE_CREDITJOUR.ADDLINE$(I%) = &
			EDIT$(OE_CREDITJOUR::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(OE_CREDITJOUR::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		OE_CREDITJOUR.ADDLINE$(I%) = &
			EDIT$(OE_CREDITJOUR::ADD2, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(OE_CREDITJOUR::ADD3, -1%) <> ""
	THEN
		I% = I% + 1%
		OE_CREDITJOUR.ADDLINE$(I%) = &
			EDIT$(OE_CREDITJOUR::ADD3, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	OE_CREDITJOUR.ADDLINE$(I%) = &
		EDIT$(EDIT$(OE_CREDITJOUR::CITY, 128%) + ", " + &
		OE_CREDITJOUR::STATE + " " + OE_CREDITJOUR::ZIP + " " + &
		OE_CREDITJOUR::COUNTRY, 8% + 16% + 32% + 128%)

	OE_CREDITJOUR.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 4%	! Blank 'em out

	!
	! Get the records that match in the tables
	!
	! Account Receivable Customer File
	!
	V% = AR_EXAM_CUSTOM(OE_CREDITJOUR::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(AR_35CUSTOM_EXAM::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM_EXAM::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM_EXAM::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM_EXAM::ADD2, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM_EXAM::ADD3, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM_EXAM::ADD3, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	AR_CUSTOM.ADDLINE$(I%) = &
		EDIT$(EDIT$(AR_35CUSTOM_EXAM::CITY, 128%) + ", " + &
		AR_35CUSTOM_EXAM::STATE + " " + AR_35CUSTOM_EXAM::ZIP + " " + &
		AR_35CUSTOM_EXAM::COUNTRY, 8% + 16% + 32% + 128%)

	AR_CUSTOM.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 4%	! Blank 'em out

	!
	! Find info about the first salesman
	!
	IF SB_EXAM_SUBACCOUNT("S", OE_CREDITJOUR::SALESMAN, &
		SA_SALESMAN_EXAM) <> CMC$_NORMAL
	THEN
		SA_SALESMAN_EXAM::DESCR = OE_CREDITJOUR::SALESMAN
	END IF

2130	OE_CREASON::TAXABLE = ""
	OE_CREASON::CR_ACCT = ""
	OE_CREASON::DESCR   = ""

2140	GOSUB PrintStmt

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

18010	!
	! Print the top of statement
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

18030	!
	! Get the lines for the header
	!
	WHEN ERROR IN
		FIND #OE_CREDITLINE.CH%, KEY #0% EQ OE_CREDITJOUR::MEMONUM
	USE
		CONTINUE 18090 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "OE_CREDITLINE"
		CONTINUE HelpError
	END WHEN

	CURRENT_LINE% = 0%

 ReadLine:
	WHEN ERROR IN
		GET #OE_CREDITLINE.CH%, REGARDLESS
	USE
		CONTINUE 18090 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "OE_CREDITLINE"
		CONTINUE HelpError
	END WHEN

	IF (OE_CREDITLINE::MEMONUM = OE_CREDITJOUR::MEMONUM)
	THEN
		CURRENT_LINE% = CURRENT_LINE% + 1
		GOSUB DumpLines
		GOTO ReadLine
	END IF

18090	!
	! Print notes lines
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_HNOTES%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)


	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
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
	! Skip to a new page if necessary
	!
	GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER

	REAL_PRICE = FUNC_ROUND((1 - OE_CREDITLINE::DISCOUNT / 100) * &
		(OE_CREDITLINE::PRICE - OE_CREDITLINE::PROMO), 3%)

	!
	! Calculate line total based on ship quantity
	!
	LINE_TOTAL = FUNC_ROUND(REAL_PRICE * OE_CREDITLINE::INVQTY, 2%)

	!
	! Calculate line total based on credit quantity
	!
	CREDITLINE = FUNC_ROUND(REAL_PRICE * OE_CREDITLINE::CREDQTY, 2%)

	HEADER_TOTAL = HEADER_TOTAL + LINE_TOTAL
	ORDER_TOTAL  = ORDER_TOTAL + CREDITLINE

	!
	! Get Product Description
	!
	V% = PD_EXAM_PRODUCT(OE_CREDITLINE::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Get Reason information
	!
	OE_CREASON::TAXABLE = ""
	OE_CREASON::CR_ACCT = STRING$(LEN(OE_CREASON::CR_ACCT), A"?"B)
	OE_CREASON::DESCR   = ""

18300	WHEN ERROR IN
		GET #OE_CREASON.CH%, &
			KEY #0% EQ OE_CREDITLINE::REASON, &
			REGARDLESS
	USE
		FILENAME$ = "OE_CREASON"
		CONTINUE HelpError
	END WHEN

	!
	! Read Promo if any
	!
	IF OE_READ_PROMO(OE_CREDITLINE::PRODUCT, OE_CREDITJOUR::ORDDATE, &
		OE_CREDITJOUR::CUSNUM, OE_PROMO_READ, 0.0, 0.0) <> CMC$_NORMAL &
		OR OE_CREDITLINE::PROMO = 0.0
	THEN
		OE_PROMO_READ::REFPROMO  = SPACE$(LEN(OE_PROMO_READ::REFPROMO))
		OE_PROMO_READ::DESCRIPTION = "" !OE_CREDITLINE::NOTES
		!OE_CREDITLINE::NOTES = SPACE$(LEN(OE_CREDITLINE::NOTES))
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

	!
	! Print notes one line
	!
	!IF TRM$(OE_CREDITLINE::NOTES) <> ""
	!THEN
	!	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
	!		FRM_LNOTES%, &
	!		FORM_TEXT$, &
	!		FORM_GROUPS%, &
	!		FORM_GROUP(), &
	!		0%)
	!END IF

	RETURN

 NewPage:
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

	!
	! Print lines to botton of the voucher
	!
	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO FORM_GROUP(FRM_TOP%)::NUMBER

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

	BODY_COUNT% = 0%

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
		OE_FORM.DEV$ + "OE_FORM", REPORT$, &
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
	FRM_HNOTES% = 0%
	FRM_BOTTOM% = 0%
	FRM_SUBBOTTOM% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-BODY"
			FRM_BODY% = I%

		CASE "FRM-HNOTES"
			FRM_HNOTES% = I%

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

	FOR I% = 1% TO 3%

		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			1%)

		!BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		!	FRM_LNOTES%, &
		!	FORM_TEXT$, &
		!	FORM_GROUPS%, &
		!	FORM_GROUP(), &
		!	1%)

	NEXT I%

	!BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
	!	FRM_HNOTES%, &
	!	FORM_TEXT$, &
	!	FORM_GROUPS%, &
	!	FORM_GROUP(), &
	!	1%)

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
	RESUME HelpError

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
	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.HB"
	MAP (OE_CREDITJOUR)	OE_CREDITJOUR_CDD	OE_CREDITJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.HB"
	MAP (OE_CREDITLINE)	OE_CREDITLINE_CDD	OE_CREDITLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	COM (OE_PROMO_READ)	OE_PROMO_CDD	OE_PROMO_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD	OE_CREASON

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	COM (PD_PRODUCT_EXAM)	PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SA_SALESMAN_EXAM)	SA_SALESMAN_CDD	SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
		MAX_LINE%, &
		HEADER_TOTAL, &
		ORDER_TOTAL, &
		CREDITLINE, &
		LINE_TOTAL, &
		REAL_PRICE, &
		STATEMENT_DATE$ = 8%, &
		PAGE_NUMBER%, &
		AR_CUSTOM.ADDLINE$(4%) = 50%, &
		OE_CREDITJOUR.ADDLINE$(4%) = 50%

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

	CASE "OE_CREDITJOUR::MEMONUM"
		TEXTVALUE$ = OE_CREDITJOUR::MEMONUM

	CASE "OE_CREDITJOUR::MEMODATE"
		TEXTVALUE$ = PRNT_DATE(OE_CREDITJOUR::MEMODATE, 8%)

	CASE "OE_CREDITJOUR::MEMODATE6"
		TEXTVALUE$ = PRNT_DATE(OE_CREDITJOUR::MEMODATE, 6%)

	CASE "OE_CREDITJOUR::REASON"
		TEXTVALUE$ = OE_CREDITJOUR::REASON

	CASE "OE_CREDITJOUR::ORDDATE"
		TEXTVALUE$ = PRNT_DATE(OE_CREDITJOUR::ORDDATE, 8%)

	CASE "OE_CREDITJOUR::ORDDATE6"
		TEXTVALUE$ = PRNT_DATE(OE_CREDITJOUR::ORDDATE, 6%)

	CASE "OE_CREDITJOUR::ORDTYPE"
		TEXTVALUE$ = OE_CREDITJOUR::ORDTYPE

	CASE "OE_CREDITJOUR::CUSNUM"
		TEXTVALUE$ = OE_CREDITJOUR::CUSNUM

	CASE "OE_CREDITJOUR::DISC"
		REALVALUE  = OE_CREDITJOUR::DISC

	CASE "OE_CREDITJOUR::MISC"
		REALVALUE  = OE_CREDITJOUR::MISC

	CASE "OE_CREDITJOUR::SHIPNAM"
		TEXTVALUE$ = OE_CREDITJOUR::SHIPNAM

	CASE "OE_CREDITJOUR::ADD1"
		TEXTVALUE$ = OE_CREDITJOUR::ADD1

	CASE "OE_CREDITJOUR::ADD2"
		TEXTVALUE$ = OE_CREDITJOUR::ADD2

	CASE "OE_CREDITJOUR::ADD3"
		TEXTVALUE$ = OE_CREDITJOUR::ADD3

	CASE "OE_CREDITJOUR::CITY"
		TEXTVALUE$ = OE_CREDITJOUR::CITY

	CASE "OE_CREDITJOUR::STATE"
		TEXTVALUE$ = OE_CREDITJOUR::STATE

	CASE "OE_CREDITJOUR::ZIP"
		TEXTVALUE$ = OE_CREDITJOUR::ZIP

	CASE "OE_CREDITJOUR::COUNTRY"
		TEXTVALUE$ = OE_CREDITJOUR::COUNTRY

	CASE "OE_CREDITJOUR:ADDLINE1"	! Substitute Ship to Address
		TEXTVALUE$ = OE_CREDITJOUR.ADDLINE$(1%)

	CASE "OE_CREDITJOUR:ADDLINE2"	! Substitute Ship to Address
		TEXTVALUE$ = OE_CREDITJOUR.ADDLINE$(2%)

	CASE "OE_CREDITJOUR:ADDLINE3"	! Substitute Ship to Address
		TEXTVALUE$ = OE_CREDITJOUR.ADDLINE$(3%)

	CASE "OE_CREDITJOUR:ADDLINE4"	! Substitute Ship to Address
		TEXTVALUE$ = OE_CREDITJOUR.ADDLINE$(4%)

	CASE "OE_CREDITJOUR::SALESTAX"
		REALVALUE  = OE_CREDITJOUR::SALESTAX

	CASE "OE_CREDITJOUR::LOCATION"
		TEXTVALUE$ = OE_CREDITJOUR::LOCATION

	CASE "OE_CREDITJOUR::OPERATOR"
		TEXTVALUE$ = OE_CREDITJOUR::OPERATOR

	CASE "OE_CREDITJOUR::SALESMAN1"
		TEXTVALUE$ = OE_CREDITJOUR::SALESMAN

	CASE "OE_CREDITJOUR::NOTE1"
		TEXTVALUE$ = OE_CREDITJOUR::NOTES(0%)

	CASE "OE_CREDITJOUR::NOTE2"
		TEXTVALUE$ = OE_CREDITJOUR::NOTES(1%)

	CASE "OE_CREDITJOUR::FREIGHT"
		REALVALUE  = OE_CREDITJOUR::FREIGHT

	!************************************************************
	! Fields for the Order Entry Order Line file
	!************************************************************

	CASE "OE_CREDITLINE::MEMONUM"
		TEXTVALUE$ = OE_CREDITLINE::MEMONUM

	CASE "OE_CREDITLINE::PRODUCT"
		TEXTVALUE$ = OE_CREDITLINE::PRODUCT

	CASE "OE_CREDITLINE::CREDQTY"
		REALVALUE  = OE_CREDITLINE::CREDQTY

	CASE "OE_CREDITLINE::INVQTY"
		REALVALUE  = OE_CREDITLINE::INVQTY

	CASE "OE_CREDITLINE::PRICE"
		REALVALUE  = OE_CREDITLINE::PRICE

	CASE "OE_CREDITLINE::DISCOUNT"
		REALVALUE  = OE_CREDITLINE::DISCOUNT

	CASE "OE_CREDITLINE::PROMO"
		REALVALUE  = OE_CREDITLINE::PROMO

	CASE "OE_CREDITLINE::COST"
		REALVALUE  = OE_CREDITLINE::COST

	!************************************************************
	! Fields for the Product Description file
	!************************************************************

	CASE "PD_PRODUCT::PRODUCT_NUM"
		TEXTVALUE$ = PD_PRODUCT_EXAM::PRODUCT_NUM

	CASE "PD_PRODUCT::DESCRIPTION"
		TEXTVALUE$ = PD_PRODUCT_EXAM::DESCRIPTION

	CASE "PD_PRODUCT::PROD_TYPE"
		TEXTVALUE$ = PD_PRODUCT_EXAM::PROD_TYPE

	CASE "PD_PRODUCT::CATEGORY"
		TEXTVALUE$ = PD_PRODUCT_EXAM::CATEGORY

	CASE "PD_PRODUCT::UOM"
		TEXTVALUE$ = PD_PRODUCT_EXAM::UOM

	CASE "PD_PRODUCT::PACK"
		TEXTVALUE$ = PD_PRODUCT_EXAM::PACK

	CASE "PD_PRODUCT::LABEL"
		TEXTVALUE$ = PD_PRODUCT_EXAM::LABEL

	CASE "PD_PRODUCT::METHOD"
		TEXTVALUE$ = PD_PRODUCT_EXAM::METHOD

	CASE "PD_PRODUCT::BDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::BDATE, 8%)

	CASE "PD_PRODUCT::BDATE6"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::BDATE, 6%)

	CASE "PD_PRODUCT::SSTATUS"
		TEXTVALUE$ = PD_PRODUCT_EXAM::SSTATUS

	CASE "PD_PRODUCT::EDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::EDATE, 8%)

	CASE "PD_PRODUCT::EDATE6"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::EDATE, 6%)

	CASE "PD_PRODUCT::SECONDARY_CODE"
		TEXTVALUE$ = PD_PRODUCT_EXAM::SECONDARY_CODE

	!************************************************************
	! Fields for the Accounts Receivable Ver. 3.5 Customer file
	!************************************************************

	CASE "AR_35CUSTOM::CUSNUM", "AR_CUSTOM::CUSNUM"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CUSNUM

	CASE "AR_35CUSTOM::CUSNAM", "AR_CUSTOM::CUSNAM"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CUSNAM

	CASE "AR_35CUSTOM::TTYPE", "AR_CUSTOM::TTYPE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TTYPE

	CASE "AR_35CUSTOM::CATEGORY", "AR_CUSTOM::CATEGORY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CATEGORY

	CASE "AR_35CUSTOM::BDATE", "AR_CUSTOM::BDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::BDATE, 8%)

	CASE "AR_35CUSTOM::BDATE", "AR_CUSTOM::BDATE6"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::BDATE, 6%)

	CASE "AR_35CUSTOM::SSTATUS", "AR_CUSTOM::SSTATUS"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SSTATUS

	CASE "AR_35CUSTOM::EDATE", "AR_CUSTOM::EDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::EDATE, 8%)

	CASE "AR_35CUSTOM::EDATE", "AR_CUSTOM::EDATE6"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::EDATE, 6%)

	CASE "AR_35CUSTOM::ADD1", "AR_CUSTOM::ADD1"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ADD1

	CASE "AR_35CUSTOM::ADD2", "AR_CUSTOM::ADD2"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ADD2

	CASE "AR_35CUSTOM::ADD3", "AR_CUSTOM::ADD3"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ADD3

	CASE "AR_35CUSTOM::CITY", "AR_CUSTOM::CITY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CITY

	CASE "AR_35CUSTOM::STATE", "AR_CUSTOM::STATE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::STATE

	CASE "AR_35CUSTOM::ZIP", "AR_CUSTOM::ZIP"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ZIP

	CASE "AR_35CUSTOM::COUNTRY", "AR_CUSTOM::COUNTRY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::COUNTRY

	CASE "AR_35CUSTOM::COUNTY", "AR_CUSTOM::COUNTY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::COUNTY

	CASE "AR_CUSTOM:ADDLINE1"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(1%)

	CASE "AR_CUSTOM:ADDLINE2"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(2%)

	CASE "AR_CUSTOM:ADDLINE3"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(3%)

	CASE "AR_CUSTOM:ADDLINE4"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(4%)

	CASE "AR_35CUSTOM::PHONE", "AR_CUSTOM::PHONE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::PHONE

	CASE "AR_35CUSTOM::METHOD", "AR_CUSTOM::METHOD"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::METHOD

	CASE "AR_35CUSTOM::STMTFLG", "AR_CUSTOM::STMTFLG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::STMTFLG

	CASE "AR_35CUSTOM::ALPSRT", "AR_CUSTOM::ALPSRT"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ALPSRT

	CASE "AR_35CUSTOM::SERCHRG", "AR_CUSTOM::SERCHRG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SERCHRG

	CASE "AR_35CUSTOM::TAXCODE", "AR_CUSTOM::TAXCODE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXCODE

	CASE "AR_35CUSTOM::TAXEXEMP", "AR_CUSTOM::TAXEXEMP"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXEXEMP

	CASE "AR_35CUSTOM::LOCATION", "AR_CUSTOM::LOCATION"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::LOCATION

	CASE "AR_35CUSTOM::TERMS", "AR_CUSTOM::TERMS"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TERMS

	CASE "AR_35CUSTOM::CARRIER", "AR_CUSTOM::CARRIER"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CARRIER

	CASE "AR_35CUSTOM::SALESMAN", "AR_CUSTOM::SALESMAN"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SALESMAN

	CASE "AR_35CUSTOM::CREDITLIM", "AR_CUSTOM::CREDITLIM"
		REALVALUE  = AR_35CUSTOM_EXAM::CREDITLIM

	CASE "AR_35CUSTOM::DISCOUNT", "AR_CUSTOM::DISCOUNT"
		REALVALUE  = AR_35CUSTOM_EXAM::DISCOUNT

	CASE "AR_35CUSTOM::BACKORDER", "AR_CUSTOM::BACKORDER"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::BACKORDER

	CASE "AR_35CUSTOM::TAXFLAG", "AR_CUSTOM::TAXFLAG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXFLAG

	!************************************************************
	! Fields for the Reason Description file
	!************************************************************

	CASE "OE_CREASON::CREASON"
		TEXTVALUE$ = OE_CREASON::CREASON

	CASE "OE_CREASON::DESCR"
		TEXTVALUE$ = OE_CREASON::DESCR

	CASE "OE_CREASON::TAXABLE"
		TEXTVALUE$ = OE_CREASON::TAXABLE

	CASE "OE_CREASON::CR_ACCT"
		TEXTVALUE$ = OE_CREASON::CR_ACCT

	!************************************************************
	! Fields for the Promo Description file
	!************************************************************

	CASE "OE_PROMO::REFPROMO"
		TEXTVALUE$ = OE_PROMO_READ::REFPROMO

	CASE "OE_PROMO::DESCRIPTION"
		TEXTVALUE$ = OE_PROMO_READ::DESCRIPTION

	!************************************************************
	! Fields for the Salesman Description file
	!************************************************************

	CASE "SA_SALESMAN::SALNAME1"
		TEXTVALUE$ = SA_SALESMAN_EXAM::DESCR

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "HEADER_TOTAL"
		REALVALUE = HEADER_TOTAL

	CASE "ORDER_TOTAL"
		REALVALUE = ORDER_TOTAL

	CASE "CREDITLINE"
		REALVALUE = CREDITLINE

	CASE "LINE_TOTAL"
		REALVALUE = LINE_TOTAL

	CASE "REAL_PRICE"
		REALVALUE = REAL_PRICE

	CASE "STATEMENT_DATE"
		TEXTVALUE$ = PRNT_DATE(STATEMENT_DATE$, 8%)

	CASE "STATEMENT_DATE6"
		TEXTVALUE$ = PRNT_DATE(STATEMENT_DATE$, 6%)

	CASE "PAGE_NUMBER"
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	END SELECT

	END SUB
	!+-+-+
	!++
	! Abstract:FLD07
	!	^*(07) Memo Number\*
	!	.b
	!	.lm +5
	!	The ^*Memo Number\* field is the
	!	first Memo number assigned by the user and will be incremented by the
	!	system one numeric number for each subsequent invoice entered.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Memo Number>Memo Form
	!	.x Memo Form>Memo Number
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD08
	!	.x Invoice Date>Order Entry Invoice Print
	!	^*(08) Invoice Date\*
	!	.b
	!	.lm +5
	!	The ^*Invoice Date\* field is the date which is
	!	to be printed on the invoice form.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry Invoice Print>Invoice Date
	!
	!--
