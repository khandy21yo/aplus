1	%TITLE "Print Requisition Form"
	%SBTTL "WP_FORM_REQJOUR_LL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	! ID:WPREQJ
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Requisition Form\* option in the Material Requisition
	!	prints the Material Requisition Forms.
	!	.lm -5
	!
	! Index:
	!	.x Print>Requisition>Form
	!	.x Material Requisition Journal>Form>Print
	!
	! Option:
	!
	!	WP_REPORT$JOUR
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_FORM_REQJOUR_LL/LINE
	!	$ LINK/EXECUTABLE=WP_EXE:*.EXE WP_FORM_REQJOUR_LL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_FORM_REQJOUR_LL.OBJ;*
	!
	! Author:
	!
	!	11/17/2000 - Kevin Handy
	!		Based on WP_FORM_REQJOUR
	!
	! Modification history:
	!
	!	12/07/2000 - Kevin Handy
	!		Change references in forms from REC_COUNT% to
	!		WP_REQLINE::REQLINE so things won't all show the same
	!		line number.
	!
	!	12/07/2000 - Kevin Handy
	!		Don't reuse LOOP% variable, which causes some
	!		horrible output to occur.
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
	%INCLUDE "SOURCE:[WP.OPEN]WP_REQJOUR.HB"
	MAP (WP_REQJOUR)	WP_REQJOUR_CDD		WP_REQJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE
	DECLARE			WP_REQLINE_CDD		WP_REQLINE_NEW, &
							WP_REQLINE_OLD

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP (JC_TYPE)		JC_TYPE_CDD		JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT
	DECLARE			UTL_REPORT_CDD		UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM	FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
		STATEMENT_DATE$ = 8%, &
		PAGE_NUMBER%, &
		REC_COUNT%, &
		ONHAND.STOCK, &
		ALLOC.STOCK, &
		ONORDER.STOCK, &
		AVAIL.STOCK, &
		FREE.STOCK

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	!
	! Dimension statement
	!
	RECORD WP_SORT_CDD
		STRING SORTBY = 4%
		STRING PRODUCT = 14%
		WP_REQLINE_CDD WP_REQLINE
	END RECORD

	DECLARE INTEGER CONSTANT MAX_WP_SORT = 800%

	DIM WP_SORT_CDD WP_SORT(MAX_WP_SORT)
	DECLARE WP_SORT_CDD WP_SORT_TEMP

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "WPREQJ"

	!
	! Look up device
	!
	CALL READ_DEVICE("WP_FORM", WP_FORM.DEV$, STAT%)

	!***************************************************************
	! Open Report files
	!***************************************************************

370	!
	! Open REPORT file
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	FROM_ITEM$ = TRM$(UTL_REPORTX::OPTDEF(0%))
	FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(WP_REQJOUR::JOB))

	!++
	! Abstract:FLD01
	!	^*(01) From Job\*
	!	.b
	!	.lm +5
	!	The ^*From Job\* field in the Print Requisitions Form Report Setting Screen
	!	begins printing the requisition forms with a designated
	!	job number.
	!	.b
	!	A blank field will cause the forms to start printing with the first job in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x From Job>Material Requisition Form
	!	.x Material Requisition Form>From Job
	!
	!--

	TO_ITEM$ = TRM$(UTL_REPORTX::OPTDEF(1%))
	TO_ITEM$ = LEFT(TO_ITEM$, LEN(WP_REQJOUR::JOB))

	!++
	! Abstract:FLD02
	!	^*(02) To Job\*
	!	.b
	!	.lm +5
	!	The ^*To Job\* field in the Materials Requisition Forms Report Setting Screen
	!	causes the forms to discontinue printing with a designated
	!	job number.
	!	.b
	!	A blank field causes the forms to end printing with the last job in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Material Requisition Form
	!	.x Material Requisition Form>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects Materials Requisition Forms
	!	to be printed for designated jobs by entering a "wildcard" using the wildcarding
	!	technique.
	!	.b
	!	For information on wildcarding technique, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field selects the Materials
	!	Requisition batch number for which forms will be printed.  Each Journal File
	!	is assigned a batch number consisting of two (2) alphanumeric characters.
	!	.b
	!	An entry is required in this field.
	!	.LM -5
	!
	! Index:
	!	.x Batch Number>Material Requisition Form
	!	.x Material Requisition Form>Batch Number
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(4%))

	! Abstract:FLD05
	!	^*(05) Form\*
	!	.b
	!	.lm +5
	!	The ^*Forms\* field requires the form name which will be used in
	!	printing the Materials Requisition Forms.  Forms are created and their names
	!	established in the Edit Forms option in the Utility Section.
	!	.lm -5
	!
	! Index:
	!	.x Forms
	!
	!--

	REQUISITION_FROM$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Requisition Number\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Number\* field selects
	!	requisition number to be printed. If a value is entered, a form for the
	!	designated requisition only will print. If this field is left blank, the system
	!	will print only records having blank requisition numbers, assigning
	!	requisition numbers as each form is printed.
	!	.lm -5
	!
	! Index:
	!	.x Requisition Number>Material Requisition Form
	!	.x Material Requisition Form>Requisition Number
	!	.x Requisition Form>Requisition Number
	!
	!--

	REQUISITION_TO$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Requisition Number\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Number\* field selects a
	!	requisition number to be printed. If a value is entered, a form for the
	!	designated requisition only will print. If this field is left blank, the system
	!	will print only records having blank requisition numbers, assigning
	!	requisition numbers as each form is printed.
	!	.lm -5
	!
	! Index:
	!	.x Requisition Number>Material Requisition Form
	!	.x Material Requisition Form>Requisition Number
	!	.x Requisition Form>Requisition Number
	!
	!--

	!
	! Pad the requisition with spaces
	!
	IF REQUISITION_FROM$ <> ""
	THEN
		REQUISITION_FROM$ = SPACE$(LEN(WP_REQLINE::REQNUM) - &
			LEN(REQUISITION_FROM$)) + &
			REQUISITION_FROM$
	END IF

	IF REQUISITION_TO$ = ""
	THEN
		REQUISITION_TO$ = REQUISITION_FROM$
	ELSE
		REQUISITION_TO$ = SPACE$(LEN(WP_REQLINE::REQNUM) - &
			LEN(REQUISITION_TO$)) + &
			REQUISITION_TO$
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!***************************************************************
	! Open all other files
	!***************************************************************

600	!
	! Open WIP order journal header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQJOUR.OPN"
	USE
		FILENAME$ = "WP_REQJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

610	!
	! Open WIP order journal line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.PST"
	USE
		FILENAME$ = "WP_REQLINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

620	!
	! Open Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.UPD"
	USE
		FILENAME$ = "WP_CONTROL"
		CONTINUE HelpError
	END WHEN

630	!
	! Open Subaccounts file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

640	!
	! Open Location description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		CONTINUE 650 IF ERR = 5%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

650	!
	! Open Job Type description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.OPN"
	USE
		CONTINUE 660 IF ERR = 5%
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

660	!
	! Open Job Class description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.OPN"
	USE
		CONTINUE 670 IF ERR = 5%
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
	END WHEN

670	!
	! Open Requisition Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
	USE
		CONTINUE 680 IF ERR = 5%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

680	!
	! Open product description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE 690 IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

690	!
	! Open binmap
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		CONTINUE 700 IF ERR = 5%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

700	!
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
			RESET #WP_REQJOUR.CH%
		ELSE
			FIND #WP_REQJOUR.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE 3000 IF ERR = 155%
		FILENAME$ = "WP_REQJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	!
	! Get next record
	!
	WHEN ERROR IN
		GET #WP_REQJOUR.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "WP_REQJOUR"
		CONTINUE HelpError
	END WHEN

	GOTO 3000 IF (WP_REQJOUR::JOB > TO_ITEM$) &
		AND TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" AND &
		COMP_STRING(EDIT$(WP_REQJOUR::JOB, -1%), &
		WLDCRD$) = 0%

2100	!
	! Get the records that match in the tables
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, &
			KEY #0% EQ "J" + WP_REQJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE 2150 IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

2110	!
	! Location
	!
	UTL_LOCATION::LOCNAME = ""
	UTL_LOCATION::ADDRESS1 = ""
	UTL_LOCATION::ADDRESS2 = ""
	UTL_LOCATION::CITY = ""
	UTL_LOCATION::STATE = ""
	UTL_LOCATION::ZIP = ""

	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, KEY #0% EQ JC_JOB::LOCATION, REGARDLESS
	USE
		CONTINUE 2120 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

2120	!
	! Type
	!
	WHEN ERROR IN
		GET #JC_TYPE.CH%, KEY #0% EQ JC_JOB::TTYPE, REGARDLESS
	USE
		JC_TYPE::DESCR = ""
		CONTINUE 2130 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

2130	!
	! Class
	!
	WHEN ERROR IN
		GET #JC_CLASS.CH%, KEY #0% EQ JC_JOB::CLASS, REGARDLESS
	USE
		JC_CLASS::DESCR = ""
		CONTINUE 2150 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
	END WHEN

2150	GOSUB PrintStmt

2190	GOTO ExitProgram IF UTL_REPORTX::STAT

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

	LINE_COUNT%  = 0%
	BODY_COUNT%  = 0%
	PAGE_NUMBER% = 1%

	CURRENT_LINE% = 0%

	WP_SORT_COUNT% = 0%

18030	!
	! Need to get a line so we have the operation number
	!
	WHEN ERROR IN
		FIND #WP_REQLINE.CH%, &
			KEY #0% EQ WP_REQJOUR::JOB + WP_REQJOUR::LLINE, &
			REGARDLESS
	USE
		CONTINUE 18090 IF ERR = 155%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

 ReadLine:
18040	WHEN ERROR IN
		GET #WP_REQLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 18090 IF ERR = 11%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Make sure the line record matches the header record
	!
	GOTO 18090 IF WP_REQLINE::JOB <> WP_REQJOUR::JOB OR &
		WP_REQLINE::LLINE <> WP_REQJOUR::LLINE

	!
	! Check the requisiton field
	!
	IF REQUISITION_FROM$ = ""
	THEN
		GOTO ReadLine IF EDIT$(WP_REQLINE::REQNUM, -1%) <> ""
	ELSE
		GOTO ReadLine IF WP_REQLINE::REQNUM < REQUISITION_FROM$ OR &
			WP_REQLINE::REQNUM > REQUISITION_TO$
	END IF

	!
	! Have we changed operation numbers?
	!
	IF CURRENT_LINE% AND &
		WP_REQLINE::OPERATION <> WP_REQLINE_OLD::OPERATION
	THEN
		WP_REQLINE_NEW = WP_REQLINE
		GOSUB DumpLines
		WP_REQLINE = WP_REQLINE_OLD
		GOSUB PrintBottom
		WP_REQLINE = WP_REQLINE_NEW
	END IF

	!
	! Assign a requisition number if we don't already have one
	!
	GOSUB CheckReqNum IF WP_REQLINE::REQNUM = ""

18060	REC_COUNT% = REC_COUNT% + 1%

	CURRENT_LINE% = CURRENT_LINE% + 1%

	WP_REQLINE::REQLINE = FORMAT$(REC_COUNT%, "<0>###")

	WHEN ERROR IN
		UPDATE #WP_REQLINE.CH%
	USE
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	WP_REQLINE_OLD = WP_REQLINE

18080	IF REC_COUNT% = 1%
	THEN
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

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ WP_REQLINE::PRODUCT, REGARDLESS
	USE
		PD_PRODUCT::CATEGORY = ""
	END WHEN

	WP_SORT_COUNT% = WP_SORT_COUNT% + 1%
	WP_SORT(WP_SORT_COUNT%)::SORTBY = PD_PRODUCT::CATEGORY
	WP_SORT(WP_SORT_COUNT%)::PRODUCT = WP_REQLINE::PRODUCT
	WP_SORT(WP_SORT_COUNT%)::WP_REQLINE = WP_REQLINE

	GOTO ReadLine

18090	GOSUB DumpLines

	GOSUB PrintBottom IF CURRENT_LINE%

18100	!
	! Do the next group
	!
	RETURN

 DumpLines:
18200	!*******************************************************************
	! Dump all collected lines to invoice
	!*******************************************************************

	!
	! Slow sort, but easy to debug
	!
	FOR LOOP1% = 1% TO WP_SORT_COUNT%

		FOR LOOP2% = LOOP1% + 1% TO WP_SORT_COUNT%

			IF WP_SORT(LOOP1%)::SORTBY + WP_SORT(LOOP1%)::PRODUCT > &
				WP_SORT(LOOP2%)::SORTBY + WP_SORT(LOOP2%)::PRODUCT
			THEN
				WP_SORT_TEMP = WP_SORT(LOOP1%)
				WP_SORT(LOOP1%) = WP_SORT(LOOP2%)
				WP_SORT(LOOP2%) = WP_SORT_TEMP
			END IF

		NEXT LOOP2%

	NEXT LOOP1%

	FOR LOOP1% = 1% TO WP_SORT_COUNT%

		WP_REQLINE = WP_SORT(LOOP1%)::WP_REQLINE
		GOSUB 18205

	NEXT LOOP1%

	WP_SORT_COUNT% = 0%

	RETURN

18205	!
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

		PAGE_NUMBER% = PAGE_NUMBER% + 1%

		!
		! Print lines to bottom of the voucher
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

	END IF

18210	!
	! Get Product Description
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ WP_REQLINE::PRODUCT, REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = ""
		CONTINUE 18220 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

18220	!
	! Get BINMAP location
	!
	IC_BINMAP::BIN(I%) = "" FOR I% = 0% TO 3%

	WHEN ERROR IN
		GET #IC_BINMAP.CH%, KEY #0% EQ WP_REQLINE::PRODUCT + &
			JC_JOB::LOCATION, REGARDLESS
	USE
		CONTINUE 18230 IF ERR = 155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

18230	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,))

	ONHAND.STOCK  = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
	ALLOC.STOCK   = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)
	ONORDER.STOCK = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)

	AVAIL.STOCK = ONHAND.STOCK + ALLOC.STOCK
	FREE.STOCK  = AVAIL.STOCK + ONORDER.STOCK - IC_BINMAP::SAFETY

	!
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

 PrintBottom:
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

	PAGE_NUMBER% = PAGE_NUMBER% + 1%

	!
	! Print lines to bottom of the voucher
	!
	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO FORM_GROUP(FRM_TOP%)::NUMBER

	LINE_COUNT%, BODY_COUNT% = 0%

	REC_COUNT%, CURRENT_LINE% = 0%

	RETURN

	%PAGE

 CheckReqNum:
	GOTO 18330 IF CURRENT_LINE%

18300	WHEN ERROR IN
		GET #WP_CONTROL.CH%, RECORD 1%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		FILENAME$ = "WP_CONTROL"
		CONTINUE HelpError
	END WHEN

 AssignReqNum:
	V% = FUNC_INCREMENT(WP_CONTROL::REQNUM)

	!
	! Make sure that this requisition number has never been used
	!
18310	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, &
			KEY #1% GE WP_CONTROL::REQNUM, &
			REGARDLESS
	USE
		CONTINUE 18320 IF ERR = 155%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO AssignReqNum IF WP_REQREGISTER::REQNUM = WP_CONTROL::REQNUM

18320	WHEN ERROR IN
		UPDATE #WP_CONTROL.CH%
		UNLOCK #WP_CONTROL.CH%
	USE
		FILENAME$ = "WP_CONTROL"
		CONTINUE HelpError
	END WHEN

18330	WP_REQLINE::REQNUM = WP_CONTROL::REQNUM

	RETURN

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
	%INCLUDE "SOURCE:[WP.OPEN]WP_REQJOUR.HB"
	MAP (WP_REQJOUR)	WP_REQJOUR_CDD		WP_REQJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE
	DECLARE			WP_REQLINE_CDD		WP_REQLINE_OLD

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP (JC_TYPE)		JC_TYPE_CDD		JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT
	DECLARE			UTL_REPORT_CDD		UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM	FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
		STATEMENT_DATE$ = 8%, &
		PAGE_NUMBER%, &
		REC_COUNT%, &
		ONHAND.STOCK, &
		ALLOC.STOCK, &
		ONORDER.STOCK, &
		AVAIL.STOCK, &
		FREE.STOCK

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
	! Fields for the Requistion Journal Header file
	!************************************************************

	CASE "WP_REQJOUR::JOB"
		TEXTVALUE$ = WP_REQJOUR::JOB

	CASE "WP_REQJOUR::LLINE"
		TEXTVALUE$ = WP_REQJOUR::LLINE

	CASE "WP_REQJOUR::REQDATE"
		TEXTVALUE$ = PRNT_DATE(WP_REQJOUR::REQDATE, 8%)

	CASE "WP_REQJOUR::NOTE1"
		TEXTVALUE$ = WP_REQJOUR::NOTES(0%)

	CASE "WP_REQJOUR::NOTE2"
		TEXTVALUE$ = WP_REQJOUR::NOTES(1%)

	CASE "WP_REQJOUR::OPERATOR"
		TEXTVALUE$ = WP_REQJOUR::OPERATOR

	!************************************************************
	! Fields for the Requisition Entry Line file
	!************************************************************

	CASE "WP_REQLINE::JOB"
		TEXTVALUE$ = WP_REQLINE::JOB

	CASE "WP_REQLINE::ITEMCODE"
		TEXTVALUE$ = WP_REQLINE::LLINE

	CASE "WP_REQLINE::REQNUM"
		TEXTVALUE$ = CONV_STRING(WP_REQLINE::REQNUM, CMC$_LEFT)

	CASE "WP_REQLINE::OPERATION"
		TEXTVALUE$ = WP_REQLINE::OPERATION

	CASE "WP_REQLINE::PRODUCT"
		TEXTVALUE$ = WP_REQLINE::PRODUCT

	CASE "WP_REQLINE::QTY"
		REALVALUE = WP_REQLINE::QTY

	!************************************************************
	! Fields for the IC_BINMAP file
	!************************************************************

	CASE "IC_BINMAP::PRODUCT"
		TEXTVALUE$ = IC_BINMAP::PRODUCT

	CASE "IC_BINMAP::LOCATION"
		TEXTVALUE$ = IC_BINMAP::LOCATION

	CASE "IC_BINMAP::BIN1"
		TEXTVALUE$ = IC_BINMAP::BIN(0%)

	CASE "IC_BINMAP::BIN2"
		TEXTVALUE$ = IC_BINMAP::BIN(1%)

	CASE "IC_BINMAP::BIN3"
		TEXTVALUE$ = IC_BINMAP::BIN(2%)

	CASE "IC_BINMAP::BIN4"
		TEXTVALUE$ = IC_BINMAP::BIN(3%)

	CASE "IC_BINMAP::SAFETY"
		REALVALUE = IC_BINMAP::SAFETY

	CASE "IC_BINMAP::MAXLEVEL"
		REALVALUE = IC_BINMAP::MAXLEVEL

	CASE "IC_BINMAP::ABC"
		TEXTVALUE$ = IC_BINMAP::ABC

	CASE "IC_BINMAP::CYCLEMAP"
		TEXTVALUE$ = IC_BINMAP::CYCLEMAP

	!************************************************************
	! Fields for the Job Journal Header file
	!************************************************************

	CASE "JC_JOB::JOB"
		TEXTVALUE$ = JC_JOB::JOB

	CASE "JC_JOB::BDATE"
		TEXTVALUE$ = PRNT_DATE(JC_JOB::BDATE, 8%)

	CASE "JC_JOB::TTYPE"
		TEXTVALUE$ = JC_JOB::TTYPE

	CASE "JC_JOB::CLASS"
		TEXTVALUE$ = JC_JOB::CLASS

	CASE "JC_JOB::LOCATION"
		TEXTVALUE$ = JC_JOB::LOCATION

	CASE "JC_JOB::OPERATOR"
		TEXTVALUE$ = JC_JOB::OPERATOR

	CASE "JC_JOB::REFNO"
		TEXTVALUE$ = JC_JOB::REFNO

	CASE "JC_JOB::DESCR"
		TEXTVALUE$ = JC_JOB::DESCR

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

	CASE "PD_PRODUCT::BOMUOM"
		TEXTVALUE$ = PD_PRODUCT::BOMUOM

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

	CASE "PD_PRODUCT::WEIGHT"
		REALVALUE = PD_PRODUCT::WEIGHT

	CASE "PD_PRODUCT::PRODUCT_FACTOR"
		REALVALUE = PD_PRODUCT::PRODUCT_FACTOR

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

	CASE "STATEMENT_DATE" ! Report Date for Form
		TEXTVALUE$ = PRNT_DATE(STATEMENT_DATE$, 6%)

	CASE "PAGE_NUMBER" ! Page Number of Form
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "REC_COUNT" ! Requisition count (line)

		WHEN ERROR IN
			REALVALUE = VAL%(WP_REQLINE::REQLINE)
		USE
			REALVALUE = 0.0
		END WHEN
		TEXTVALUE$ = WP_REQLINE::REQLINE

	CASE "ONHAND.STOCK"
		REALVALUE = ONHAND.STOCK

	CASE "ALLOC.STOCK"
		REALVALUE = ALLOC.STOCK

	CASE "ONORDER.STOCK"
		REALVALUE = ONORDER.STOCK

	CASE "AVAIL.STOCK"
		REALVALUE = AVAIL.STOCK

	CASE "FREE.STOCK"
		REALVALUE = FREE.STOCK

	END SELECT

	END SUB
