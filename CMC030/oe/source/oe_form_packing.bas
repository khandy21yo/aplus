1	%TITLE "Print Packing Form"
	%SBTTL "OE_FORM_PACKING"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!++
	! ID:OEFORM
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Packing Form\* option prints order entry or
	!	packing forms on user designed forms.  (See Forms Controlling in the
	!	Utility Section.)
	!	.lm -5
	!
	! Index:
	!	.x Print>Packing Form
	!	.x Packing Form>Print
	!
	! Option:
	!
	!	OE_REPORT$PACK
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_FORM_PACKING/LINE
	!	$ LINK/EXECUTABLE=OE_EXE:*.EXE OE_FORM_PACKING, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_FORM_PACKING.OBJ;*
	!
	! Author:
	!
	!	10/18/91 - Frank F. Starman
	!
	! Modification History:
	!
	!	04/27/92 - Kevin Handy
	!		Added modification history section
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	05/05/92 - Dan Perkins
	!		Changed call from OUTP_LINENOTITLE to OUTP_NEWPAGE.
	!
	!	06/02/92 - Dan Perkins
	!		Changes to accomodate changes in Regheader file
	!		layout.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(..., ASCII(" ")) to "" in
	!		several places.
	!
	!	08/21/96 - Kevin Handy
	!		Remove lots of commented out code.
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/08/97 - Kevin Handy
	!		Use OUTP_INITFORM function.
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
	!		never created
	!
	!	12/14/99 - Kevin Handy
	!		Map SA_SALESMAN into its own map instead of
	!		SB_SUBACCOUNT which caused major problems.
	!
	!	12/05/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	COM (OE_REGLINE_READ)	OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_INVJOUR.HB"
	MAP (OE_INVJOUR)	OE_INVJOUR_CDD		OE_INVJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_INVLINE.HB"
	MAP (OE_INVLINE)	OE_INVLINE_CDD		OE_INVLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	COM (OE_PROMO_READ)	OE_PROMO_CDD		OE_PROMO_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	COM (PD_PRODUCT_EXAM)	PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SA_SALESMAN_EXAM)	SA_SALESMAN_CDD		SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT
	DECLARE			UTL_REPORT_CDD		UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(10%)

	MAP (JOUR_FORM) &
		HEADER_TOTAL, &
		HEADER_NET, &
		HEADER_QTY, &
		DISC_NET, &
		DISC_TOTAL, &
		LINE_TOTAL, &
		LINE_DISC, &
		NET_PRICE, &
		ORIGORD_QTY, &
		ORD_QTY, &
		BALANCE, &
		STATEMENT_DATE$ = 8%, &
		PAGE_NUMBER%, &
		XOUTASSIGN$ = 30%, &
		AR_CUSTOM.ADDLINE$(4%) = 50%, &
		OE_REGHEADER.ADDLINE$(4%) = 50%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG   FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL LONG   FUNCTION OE_READ_PROMO
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION OE_READ_REGLINE
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "OEPACK"

	!
	! Look up device
	!
	CALL  READ_DEVICE("OE_FORM", OE_FORM.DEV$, STAT%)

500	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	!
	! store original values for the help message
	!
	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM


510	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	!
	! Ask user to change settings
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Batch Number>Order Entry Order Form
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a selected
	!	batch number which is to be printed.
	!	.b
	!	Only one batch number at a time may be printed.
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
	!	The ^*Sort by\* field enters a selected order
	!	in which the report is to print.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*D\* - Document Number
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*T\* - Sale Type
	!	.te
	!	^*C\* - Sale Category
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
	!	printing to begin with a particular
	!	item. The value must be in agreement with the value
	!	entered in field (02) Sort by.
	!	.b
	!	A blank field will cause the form to start with
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
	!	printing to end with a particular
	!	item. The value must be in agreement with
	!	the field (02) Sort by.
	!	.b
	!	A blank field will cause the form to end with the
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
	!	For information on "Wildcarding" techniques, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Form\*
	!	.b
	!	.lm +5
	!	The ^*Forms\* option enters a specific form number for
	!	printing. (See Forms Controlling in the Utility Section.)
	!	.lm -5
	!
	! Index:
	!
	!--

	SELECT SORTBY$
	CASE "D"
		K_NUM% = 0%
		FROM_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - &
			LEN(TO_ITEM$)) + TO_ITEM$

	CASE "T"
		K_NUM% = 1%

	CASE "C"
		K_NUM% = 2%

	CASE "N"
		K_NUM% = 3%
	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!***************************************************************
	! Open all other files
	!***************************************************************

600	!
	! Open register order journal header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

640	!
	! Open carrier description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.OPN"
	USE
		CONTINUE 650 IF ERR = 5%
		FILENAME$ = "UTL_CARRIER"
		CONTINUE HelpError
	END WHEN

650	!
	! Open order entry invoice header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_INVJOUR.UPD"
	USE
		FILENAME$ = "OE_INVJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

660	!
	! Open order entry invoice line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_INVLINE.OPN"
	USE
		FILENAME$ = "OE_INVLINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

665	!
	! Open shipto file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.OPN"
	USE
		CONTINUE 670 IF ERR = 5%
		FILENAME$ = "OE_SHIPTO"
		CONTINUE HelpError
	END WHEN

670	!
	! Open terms description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.OPN"
	USE
		CONTINUE EndOpen IF ERR = 5%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

 EndOpen:
	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT   = TEMP_IDENT$
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
			RESET #OE_REGHEADER.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_REGHEADER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE 3000 IF ERR = 155%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	!
	! Get next record
	!
	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	!
	!
	! Check for end item
	!
	SELECT SORTBY$

	CASE "D"
		GOTO 3000 IF (OE_REGHEADER::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(OE_REGHEADER::ORDNUM, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO 3000 IF (OE_REGHEADER::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(OE_REGHEADER::ORDTYPE, -1%), &
			WLDCRD$) = 0%

	CASE "C"
		GOTO 3000 IF (OE_REGHEADER::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(OE_REGHEADER::ORDCAT, -1%), &
			WLDCRD$) = 0%

	CASE "N"
		GOTO 3000 IF (OE_REGHEADER::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(OE_REGHEADER::CUSNUM, -1%), &
			WLDCRD$) = 0%

	END SELECT

2050	!
	! We need to make sure that there is something to print
	! for this customer before proceeding.  We will print
	! him if there are current records in the OE_INVLINE file.
	!
	! Search the open file for a printable record.
	!
	WHEN ERROR IN
		GET #OE_INVJOUR.CH%, KEY #0% EQ OE_REGHEADER::ORDNUM
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 2010 IF ERR = 155%
		FILENAME$ = "OE_INVJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(OE_REGHEADER::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		OE_REGHEADER.ADDLINE$(I%) = &
			EDIT$(OE_REGHEADER::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(OE_REGHEADER::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		OE_REGHEADER.ADDLINE$(I%) = &
			EDIT$(OE_REGHEADER::ADD2, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(OE_REGHEADER::ADD3, -1%) <> ""
	THEN
		I% = I% + 1%
		OE_REGHEADER.ADDLINE$(I%) = &
			EDIT$(OE_REGHEADER::ADD3, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	OE_REGHEADER.ADDLINE$(I%) = &
		EDIT$(EDIT$(OE_REGHEADER::CITY, 128%) + ", " + &
		OE_REGHEADER::STATE + " " + OE_REGHEADER::ZIP + " " + &
		OE_REGHEADER::COUNTRY, 8% + 16% + 32% + 128%)

	OE_REGHEADER.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 4%

	!
	! Account Receivable Customer File
	!
	V% = AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_35CUSTOM_EXAM)

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
	IF SB_EXAM_SUBACCOUNT("S", OE_REGHEADER::SALESMAN, &
		SA_SALESMAN_EXAM) <> CMC$_NORMAL
	THEN
		SA_SALESMAN_EXAM::DESCR = OE_REGHEADER::SALESMAN
	END IF

2110	!
	! Utility Carrier Description File
	!
	WHEN ERROR IN
		GET #UTL_CARRIER.CH%, &
			KEY #0% EQ OE_INVJOUR::SHIPVIA, &
			REGARDLESS
	USE
		UTL_CARRIER::DESCR = ""

		CONTINUE 2120 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "UTL_CARRIER"
		CONTINUE HelpError
	END WHEN

2120	!
	! Utility Terms Description File
	!
	WHEN ERROR IN
		GET #UTL_TERMS.CH%, KEY #0% EQ OE_INVJOUR::TERMS, REGARDLESS
	USE
		UTL_TERMS::DESCR	= ""
		UTL_TERMS::DISCOUNT	= 0.0

		CONTINUE 2130 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

2130	!
	! Shipto file
	!
	WHEN ERROR IN
		GET #OE_SHIPTO.CH%, &
			KEY #0% EQ OE_REGHEADER::CUSNUM + &
			OE_REGHEADER::SHIPLIN, &
			REGARDLESS
	USE
		OE_SHIPTO::NOTES(I%) = "" &
			FOR I% = 0% TO 2%

		CONTINUE 2150 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_SHIPTO"
		CONTINUE HelpError
	END WHEN

2150	GOTO 2185 IF TRM$(OE_INVJOUR::SHIPNO) <> ""

	V% = FUNC_INCREMENT(OE_REGHEADER::SHIPNO)

	OE_INVJOUR::SHIPNO = OE_REGHEADER::SHIPNO

	WHEN ERROR IN
		UPDATE #OE_INVJOUR.CH%
	USE
		FILENAME$ = "OE_INVJOUR"
		CONTINUE HelpError
	END WHEN

2185	GOSUB PrintStmt

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Go get the next record
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
	HEADER_TOTAL,HEADER_NET,HEADER_QTY = 0.0
	REGLIN$ = "    "

 ReadLine:
	GOTO 18090 IF OE_READ_REGLINE(OE_INVJOUR::ORDNUM, REGLIN$, "GT", &
		OE_REGLINE_READ, QTY()) <> CMC$_NORMAL

	REGLIN$ = OE_REGLINE_READ::LIN

	WHEN ERROR IN
		GET #OE_INVLINE.CH%, &
			KEY #0% EQ OE_REGHEADER::ORDNUM + OE_REGLINE_READ::LIN, &
			REGARDLESS
	USE
		CONTINUE TestBackOrder IF ERR = 155%
		FILENAME$ = "OE_INVLINE"
		CONTINUE HelpError
	END WHEN

	GOSUB DumpLines
	GOTO ReadLine

 TestBackOrder:
	IF QTY(0%) <> 0%
	THEN
		OE_INVLINE::PRICE     = OE_REGLINE_READ::PRICE
		OE_INVLINE::PROMO     = OE_REGLINE_READ::PROMO
		OE_INVLINE::DISCOUNT  = OE_REGLINE_READ::DISCOUNT
		OE_INVLINE::CANCELQTY = 0.0
		OE_INVLINE::INVQTY    = 0.0

		GOSUB DumpLines
	END IF

	GOTO ReadLine

18090	CALL OUTP_NEWPAGE(UTL_REPORTX)

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_BODY%)::NUMBER

	DISC_NET = FUNC_ROUND(HEADER_NET * OE_INVJOUR::DISC / 100.0, 2%)
	DISC_TOTAL = FUNC_ROUND(HEADER_TOTAL * OE_INVJOUR::DISC / 100.0, 2%)

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
	CALL OUTP_NEWPAGE(UTL_REPORTX)

18100	!
	! Do the next group
	!
	RETURN

 DumpLines:
18200	!*******************************************************************
	! Dump all collected lines to invoice
	!*******************************************************************

	!
	! Skip to a new page if necessary
	!
	IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER
	THEN
		CALL OUTP_NEWPAGE(UTL_REPORTX)

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

	V% = PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, PD_PRODUCT_EXAM)

	BALANCE = QTY(1%) - QTY(2%) - QTY(3%) - &
		OE_INVLINE::INVQTY - OE_INVLINE::CANCELQTY

	BALANCE = 0.0 IF BALANCE <= 0.0

	ORIGORD_QTY = QTY(1%)
	ORD_QTY = QTY(1%) - QTY(2%) - QTY(3%) - OE_INVLINE::CANCELQTY

	ORD_QTY = 0.0 IF ORD_QTY <= 0.0

	LINE_TOTAL = FUNC_ROUND(OE_INVLINE::INVQTY * OE_INVLINE::PRICE, 2%)
	PROMO = FUNC_ROUND(OE_INVLINE::INVQTY * OE_INVLINE::PROMO, 2%)
	DISCOUNT = FUNC_ROUND(OE_INVLINE::INVQTY * &
		(OE_INVLINE::PRICE - OE_INVLINE::PROMO) * &
		OE_INVLINE::DISCOUNT / 100, 2%)

	LINE_DISC = LINE_TOTAL - PROMO - DISCOUNT

	NET_PRICE = FUNC_ROUND((OE_INVLINE::PRICE - OE_INVLINE::PROMO) * &
		(1 - OE_INVLINE::DISCOUNT / 100.0), 3%)

	HEADER_TOTAL = HEADER_TOTAL + LINE_TOTAL
	HEADER_NET   = HEADER_NET + LINE_DISC
	HEADER_QTY   = HEADER_QTY + OE_INVLINE::INVQTY

	!
	! Read Promo if any
	!
	IF OE_READ_PROMO(OE_REGLINE_READ::PRODUCT, OE_REGLINE_READ::TDATE, &
		OE_REGHEADER::CUSNUM, OE_PROMO_READ, 0.0, 0.0) <> &
		CMC$_NORMAL OR &
		OE_INVLINE::PROMO = 0.0
	THEN
		OE_PROMO_READ::REFPROMO= SPACE$(LEN(OE_PROMO_READ::REFPROMO))
		OE_PROMO_READ::DESCRIPTION = &
			SPACE$(LEN(OE_PROMO_READ::DESCRIPTION))
	END IF

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
		CALL HELP_34MESSAGE(SCOPE, "statement form is missing", &
			"E", SCOPE::PRG_PROGRAM, REPORT$, NUM1$(SMG_STATUS%))
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
		CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
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

	CALL OUTP_NEWPAGE(UTL_REPORTX)

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
	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	COM (OE_REGLINE_READ)	OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_INVJOUR.HB"
	MAP (OE_INVJOUR)	OE_INVJOUR_CDD		OE_INVJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_INVLINE.HB"
	MAP (OE_INVLINE)	OE_INVLINE_CDD		OE_INVLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	COM (OE_PROMO_READ)	OE_PROMO_CDD		OE_PROMO_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	COM (PD_PRODUCT_EXAM)	PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SA_SALESMAN_EXAM)	SA_SALESMAN_CDD		SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	MAP (JOUR_FORM) &
		HEADER_TOTAL, &
		HEADER_NET, &
		HEADER_QTY, &
		DISC_NET, &
		DISC_TOTAL, &
		LINE_TOTAL, &
		LINE_DISC, &
		NET_PRICE, &
		ORIGORD_QTY, &
		ORD_QTY, &
		BALANCE, &
		STATEMENT_DATE$ = 8%, &
		PAGE_NUMBER%, &
		XOUTASSIGN$ = 30%, &
		AR_CUSTOM.ADDLINE$(4%) = 50%, &
		OE_REGHEADER.ADDLINE$(4%) = 50%

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
	! Fields for the Shipping Journal Header file
	!************************************************************

	CASE "OE_INVJOUR::INVOICE"
		TEXTVALUE$ = OE_INVJOUR::INVOICE

	CASE "OE_INVJOUR::ORDNUM"
		TEXTVALUE$ = OE_INVJOUR::ORDNUM

	CASE "OE_INVJOUR::INVDATE"
		TEXTVALUE$ = PRNT_DATE(OE_INVJOUR::INVDATE, 8%)

	CASE "OE_INVJOUR::SHIPDATE"
		TEXTVALUE$ = PRNT_DATE(OE_INVJOUR::SHIPDATE, 8%)

	CASE "OE_INVJOUR::TERMS"
		TEXTVALUE$ = OE_INVJOUR::TERMS

	CASE "OE_INVJOUR::HANDLING"
		REALVALUE  = OE_INVJOUR::HANDLING

	CASE "OE_INVJOUR::DISC"
		REALVALUE  = OE_INVJOUR::DISC

	CASE "OE_INVJOUR::MISC"
		REALVALUE  = OE_INVJOUR::MISC

	CASE "OE_INVJOUR::FREIGHT"
		REALVALUE  = OE_INVJOUR::FREIGHT

	CASE "OE_INVJOUR::SALESTAX"
		REALVALUE  = OE_INVJOUR::SALESTAX

	CASE "OE_INVJOUR::OPERATOR"
		TEXTVALUE$ = OE_INVJOUR::OPERATOR

	CASE "OE_INVJOUR::NOTE1"
		TEXTVALUE$ = OE_INVJOUR::NOTES(0%)

	CASE "OE_INVJOUR::NOTE2"
		TEXTVALUE$ = OE_INVJOUR::NOTES(1%)

	CASE "OE_INVJOUR::NOTE3"
		TEXTVALUE$ = OE_INVJOUR::NOTES(2%)

	CASE "OE_INVJOUR::NOTE4"
		TEXTVALUE$ = OE_INVJOUR::NOTES(3%)

	!************************************************************
	! Fields for the Shipping Journal Line file
	!************************************************************

	CASE "OE_INVLINE::ORDNUM"
		TEXTVALUE$ = OE_INVLINE::ORDNUM

	CASE "OE_INVLINE::LIN"
		TEXTVALUE$ = OE_INVLINE::LIN

	CASE "OE_INVLINE::INVQTY"
		REALVALUE  = OE_INVLINE::INVQTY

	CASE "OE_INVLINE::PRICE"
		REALVALUE  = OE_INVLINE::PRICE

	CASE "OE_INVLINE::DISCOUNT"
		REALVALUE  = OE_INVLINE::DISCOUNT

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

	CASE "PD_PRODUCT::SSTATUS"
		TEXTVALUE$ = PD_PRODUCT_EXAM::SSTATUS

	CASE "PD_PRODUCT::EDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::EDATE, 8%)

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

	CASE "AR_35CUSTOM::SSTATUS", "AR_CUSTOM::SSTATUS"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SSTATUS

	CASE "AR_35CUSTOM::EDATE", "AR_CUSTOM::EDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::EDATE, 8%)

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
	! Fields for the Utility Carrier Description file
	!************************************************************

	CASE "UTL_CARRIER::DESCR"
		TEXTVALUE$ = UTL_CARRIER::DESCR

	CASE "UTL_TERMS::DESCR"
		TEXTVALUE$ = UTL_TERMS::DESCR

	!************************************************************
	! Fields for the Order Entry Register Header file
	!************************************************************

	CASE "OE_REGHEADER::ORDNUM"
		TEXTVALUE$ = OE_REGHEADER::ORDNUM

	CASE "OE_REGHEADER::ORDTYPE"
		TEXTVALUE$ = OE_REGHEADER::ORDTYPE

	CASE "OE_REGHEADER::ORDCAT"
		TEXTVALUE$ = OE_REGHEADER::ORDCAT

	CASE "OE_REGHEADER::ORDDATE"
		TEXTVALUE$ = PRNT_DATE(OE_REGHEADER::ORDDATE, 8%)

	CASE "OE_REGHEADER::ASTATUS"
		TEXTVALUE$ = OE_REGHEADER::ASTATUS

	CASE "OE_REGHEADER::SDATE"
		TEXTVALUE$ = PRNT_DATE(OE_REGHEADER::SDATE, 8%)

	CASE "OE_REGHEADER::CUSNUM"
		TEXTVALUE$ = OE_REGHEADER::CUSNUM

	CASE "OE_REGHEADER::SHIPNAM"
		TEXTVALUE$ = OE_REGHEADER::SHIPNAM

	CASE "OE_REGHEADER::ADD1"
		TEXTVALUE$ = OE_REGHEADER::ADD1

	CASE "OE_REGHEADER::ADD2"
		TEXTVALUE$ = OE_REGHEADER::ADD2

	CASE "OE_REGHEADER::ADD3"
		TEXTVALUE$ = OE_REGHEADER::ADD3

	CASE "OE_REGHEADER::CITY"
		TEXTVALUE$ = OE_REGHEADER::CITY

	CASE "OE_REGHEADER::STATE"
		TEXTVALUE$ = OE_REGHEADER::STATE

	CASE "OE_REGHEADER::ZIP"
		TEXTVALUE$ = OE_REGHEADER::ZIP

	CASE "OE_REGHEADER::COUNTRY"
		TEXTVALUE$ = OE_REGHEADER::COUNTRY

	CASE "OE_REGHEADER:ADDLINE1"	! Substitute Ship to Address
		TEXTVALUE$ = OE_REGHEADER.ADDLINE$(1%)

	CASE "OE_REGHEADER:ADDLINE2"	! Substitute Ship to Address
		TEXTVALUE$ = OE_REGHEADER.ADDLINE$(2%)

	CASE "OE_REGHEADER:ADDLINE3"	! Substitute Ship to Address
		TEXTVALUE$ = OE_REGHEADER.ADDLINE$(3%)

	CASE "OE_REGHEADER:ADDLINE4"	! Substitute Ship to Address
		TEXTVALUE$ = OE_REGHEADER.ADDLINE$(4%)

	CASE "OE_REGHEADER::CUSTPO"
		TEXTVALUE$ = OE_REGHEADER::CUSTPO

	CASE "OE_REGHEADER::SHIPVIA"
		TEXTVALUE$ = OE_REGHEADER::SHIPVIA

	CASE "OE_REGHEADER::TERMS"
		TEXTVALUE$ = OE_REGHEADER::TERMS

	CASE "OE_REGHEADER::DISC"
		REALVALUE  = OE_REGHEADER::DISC

	CASE "OE_REGHEADER::TAXCODE"
		TEXTVALUE$ = OE_REGHEADER::TAXCODE

	CASE "OE_REGHEADER::TAXFLAG"
		TEXTVALUE$ = OE_REGHEADER::TAXFLAG

	CASE "OE_REGHEADER::LOCATION"
		TEXTVALUE$ = OE_REGHEADER::LOCATION

	CASE "OE_REGHEADER::COMMAMT"
		REALVALUE  = OE_REGHEADER::COMMAMT

	CASE "OE_REGHEADER::SALESMAN1"
		TEXTVALUE$ = OE_REGHEADER::SALESMAN

	CASE "OE_REGHEADER::SALESMAN2"
		TEXTVALUE$ = OE_REGHEADER::OPERATOR

	CASE "OE_REGHEADER::SALCOMM1"
		REALVALUE  = OE_REGHEADER::SALCOMM

	CASE "OE_REGHEADER::SHIPNO"
		TEXTVALUE$ = OE_REGHEADER::SHIPNO

	!************************************************************
	! Fields for the Order Entry Register Line file
	!************************************************************

	CASE "OE_REGLINE::ORDNUM"
		TEXTVALUE$ = OE_REGLINE_READ::ORDNUM

	CASE "OE_REGLINE::LIN"
		TEXTVALUE$ = OE_REGLINE_READ::LIN

	CASE "OE_REGLINE::TRANTYPE"
		TEXTVALUE$ = OE_REGLINE_READ::TRANTYPE

	CASE "OE_REGLINE::PRODUCT"
		TEXTVALUE$ = OE_REGLINE_READ::PRODUCT

	CASE "OE_REGLINE::QTY"
		REALVALUE  = OE_REGLINE_READ::QTY

	CASE "OE_REGLINE::TDATE"
		TEXTVALUE$ = PRNT_DATE(OE_REGLINE_READ::TDATE, 8%)

	CASE "OE_REGLINE::PRICE"
		REALVALUE  = OE_REGLINE_READ::PRICE

	CASE "OE_REGLINE::DISCOUNT"
		REALVALUE  = OE_REGLINE_READ::DISCOUNT

	CASE "OE_REGLINE::COST"
		REALVALUE  = OE_REGLINE_READ::COST

	CASE "OE_REGLINE::POSTDATE"
		TEXTVALUE$ = PRNT_DATE(OE_REGLINE_READ::POSTDATE, 8%)

	CASE "OE_REGLINE::POSTTIME"
		TEXTVALUE$ = PRNT_TIME(OE_REGLINE_READ::POSTTIME, 2%)

	CASE "OE_REGLINE::BATCH"
		TEXTVALUE$ = OE_REGLINE_READ::BATCH

	!************************************************************
	! Fields for the Shipto file
	!************************************************************

	CASE "OE_SHIPTO::NOTES1"
		TEXTVALUE$ = OE_SHIPTO::NOTES(0%)

	CASE "OE_SHIPTO::NOTES2"
		TEXTVALUE$ = OE_SHIPTO::NOTES(1%)

	CASE "OE_SHIPTO::NOTES3"
		TEXTVALUE$ = OE_SHIPTO::NOTES(2%)

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

	CASE "HEADER_NET"
		REALVALUE = HEADER_NET

	CASE "HEADER_QTY"
		REALVALUE = HEADER_QTY

	CASE "LINE_TOTAL"
		REALVALUE = LINE_TOTAL

	CASE "LINE_DISC"
		REALVALUE = LINE_DISC

	CASE "DISC_TOTAL"
		REALVALUE = DISC_TOTAL

	CASE "DISC_NET"
		REALVALUE = DISC_NET

	CASE "NET_PRICE"
		REALVALUE = NET_PRICE

	CASE "ORD_QTY"
		REALVALUE = ORD_QTY

	CASE "ORIGORD_QTY"
		REALVALUE = ORIGORD_QTY

	CASE "BALANCE"
		REALVALUE = BALANCE

	CASE "STATEMENT_DATE"
		TEXTVALUE$ = PRNT_DATE(STATEMENT_DATE$, 6%)

	CASE "PAGE_NUMBER"
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "XOUT_ASSIGN"
		TEXTVALUE$ = XOUTASSIGN$
	END SELECT

	END SUB
	!+-+-+
	!++
	! Abstract:FLD07
	!	.x Line Sequence>Order Entry Order Form
	!	^*(07) Line Sequence\*
	!	.b
	!	.lm +5
	!	The ^*Line Sequence\* code causes the
	!	form to print in line sequence or
	!	bin location sequence.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*L\* - Line Sequence
	!	.te
	!	^*B\* - Bin Location Sequence
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry Order Form>Line Sequence
	!
	!--
