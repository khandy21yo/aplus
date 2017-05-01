1	%TITLE "Print Work Order Form from a Journal"
	%SBTTL "WP_OUTP_JOB"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_OUTP_JOB(STRING JOB, STRING BATCH, LONG FLAG)

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
	! ID:WPJOB
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints a WP Job form resulting from a print
	!	request from within a journal maintenance program.
	!	.lm -5
	!
	! Index:
	!	.x Print>Work Order>Form
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_OUTP_JOB/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_OUTP_JOB
	!	$ DELETE WP_OUTP_JOB.OBJ;*
	!
	! Author:
	!
	!	04/05/93 - Dan Perkins
	!
	! Modification history:
	!
	!	04/07/93 - Dan Perkins
	!		Added code to print the requisitions at the same time.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/26/95 - Kevin Handy
	!		Change SMG_BLANK to SMG_BLANK%.
	!
	!	10/31/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/07/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external defintions
	!
	!	05/21/98 - Kevin Handy
	!		Fix file names in error messages.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/16/98 - Kevin Handy
	!		Added ::DEPOSIT field
	!
	!	07/26/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE
	DECLARE			WP_REQLINE_CDD		WP_REQLINE_NEW, &
							WP_REQLINE_OLD

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP (JC_TYPE)		JC_TYPE_CDD		JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(10%)

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
		MAX_LINE%, &
		HEADER_TOTAL, &
		LINE_TOTAL, &
		ROLLCOUNT$ = 4%, &
		PAGE_NUMBER%, &
		REC_COUNT%, &
		ONHAND_STOCK, &
		ALLOC_STOCK, &
		ONORDER_STOCK, &
		AVAIL_STOCK, &
		FREE_STOCK

	COM (CH_IC_BINMAP)	IC_BINMAP.CH%

	COM (CH_JC_CLASS)	JC_CLASS.CH%
	COM (CH_JC_TYPE)	JC_TYPE.CH%

	COM (CH_PD_PRODUCT)	PD_PRODUCT.CH%

	COM (CH_WP_ORDERLINE)	WP_ORDERLINE.CH%
	COM (CH_WP_REGLINE)	WP_REGLINE.CH%
	COM (CH_WP_REQLINE)	WP_REQLINE.CH%
	COM (CH_WP_REQREGISTER)	WP_REQREGISTER.CH%

	COM (CH_UTL_LOCATION)	UTL_LOCATION.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION OUTP_INITFORM
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT

	%PAGE

	ON ERROR GOTO 19000

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	SELECT FLAG

	CASE 0%, 1%
		REPORT$ = "WPJOBS"

	END SELECT

	!
	! Look up device
	!
	CALL READ_DEVICE("WP_FORM", WP_FORM.DEV$, STAT%)


500	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	!
	! store original values for the help message
	!
	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM

	!
	! Plug the passed order number and batch number into the report file
	! also default the sortby sequence to Order number "O"
	!
	BATCH_NO$ = BATCH
	JOB = CONV_STRING(JOB, CMC$_LEFT)

	SETVALUE$ = "00D,01" + JOB + ",02" + JOB + ",03"

	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, SETVALUE$) <> CMC$_NORMAL


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by\*
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
	!	^*T\* - Sales Type
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*C\* - Sales Category
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the forms to print beginning with
	!	the number entered. The value must be in agreement with the value
	!	entered in field (01) Sort by.
	!	.b
	!	A blank field begins printing with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.B
	!	.LM +5
	!	The ^*To Item\* field ends printing
	!	with a selected item.  The value must be in agreement with
	!	field (01) Sort by.
	!	.B
	!	A blank value in this field causes the printing to end
	!	with the last item in the file.
	!	.LM -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.B
	!	.LM +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a "wildcard" using the wildcarding technique. The value
	!	entered must be in agreement with field (01) Sort by.
	!	.B
	!	For information on "wildcarding", refer to Appendix B.
	!	.LM -5
	!
	! Index:
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(4%))

	! Abstract:FLD05
	!	^*(05) Form Name\*
	!	.B
	!	.LM +5
	!	The ^*Form Name\* field enters the form name which
	!	will be used when printing. The format(s) for the forms are created in the
	!	Edit Forms option in the Utility Section of the menu.
	!	.lm -5
	!
	! Index:
	!
	!--

	SELECT SORTBY$

	CASE "D"
		K_NUM% = 0%
		FROM_ITEM$ = SPACE$(LEN(JOB) - LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$   = SPACE$(LEN(JOB) - LEN(TO_ITEM$)) + TO_ITEM$

	CASE "T"
		K_NUM% = 1%

	CASE "N"
		K_NUM% = 2%

	CASE "C"
		K_NUM% = 3%

	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!***************************************************************
	! Open all other files
	!***************************************************************

	!
	! Open WIP order journal header file
	!
600	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.OPN"
	USE
		FILENAME$ = "OE_ORDERJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

	!
	! Open WIP order journal line file
	!
610	IF WP_ORDERLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.OPN"
		USE
			FILENAME$ = "WP_ORDERLINE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open Control file
	!
620	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.UPD"
	USE
		FILENAME$ = "WP_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Open WIP order journal line file
	!
630	IF WP_REQLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.CRE"
		USE
			CONTINUE 640 IF ERR = 5%
			FILENAME$ = "WP_REQLINE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open Requisition Register file
	!
640	IF WP_REQREGISTER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
		USE
			CONTINUE 650 IF ERR = 5%
			FILENAME$ = "WP_REQREGISTER"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open product description  file
	!
650	IF PD_PRODUCT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
		USE
			CONTINUE 660 IF ERR = 5%
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open WIP register line file
	!
660	IF WP_REGLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
		USE
			CONTINUE 670 IF ERR = 5%
			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open Location description file
	!
670	IF UTL_LOCATION.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
		USE
			CONTINUE 680 IF ERR = 5%
			FILENAME$ = "UTL_LOCATION"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open Job Type description file
	!
680	IF JC_TYPE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.OPN"
		USE
			CONTINUE 690 IF ERR = 5%
			FILENAME$ = "JC_TYPE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open Job Class description file
	!
690	IF JC_CLASS.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.OPN"
		USE
			CONTINUE 700 IF ERR = 5%
			FILENAME$ = "JC_CLASS"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open binmap
	!
700	IF IC_BINMAP.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
		USE
			CONTINUE EndOpen IF ERR = 5%
			FILENAME$ = "IC_BINMAP"
			CONTINUE HelpError
		END WHEN
	END IF

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
	GOSUB Alignment IF (FLAG AND 1%) = 0%

	%PAGE

2000	!*******************************************************************
	! Read through header file
	!*******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_ORDERJOUR.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_ORDERJOUR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE 3000 IF ERR = 155%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	!
	! Get next record
	!
	WHEN ERROR IN
		GET #OE_ORDERJOUR.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	SELECT SORTBY$

	CASE "D"
		GOTO 3000 IF (OE_ORDERJOUR::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDNUM, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO 3000 IF (OE_ORDERJOUR::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDTYPE, -1%), &
			WLDCRD$) = 0%

	CASE "C"
		GOTO 3000 IF (OE_ORDERJOUR::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDCAT, -1%), &
			WLDCRD$) = 0%

	CASE "N"
		GOTO 3000 IF (OE_ORDERJOUR::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_ORDERJOUR::CUSNUM, -1%), &
			WLDCRD$) = 0%

	END SELECT

	!
	! Set up the WP_JOB record
	!
	WP_JOB::JOB		= EDIT$(OE_ORDERJOUR::ORDNUM, -1%)
	WP_JOB::BDATE		= OE_ORDERJOUR::ORDDATE
	WP_JOB::TTYPE		= OE_ORDERJOUR::ORDTYPE
	WP_JOB::CLASS		= OE_ORDERJOUR::ORDCAT
	WP_JOB::LOCATION	= OE_ORDERJOUR::LOCATION
	WP_JOB::OPERATOR	= OE_ORDERJOUR::OPERATOR
	WP_JOB::NOTES(0%)	= OE_ORDERJOUR::NOTES(0%)
	WP_JOB::NOTES(1%)	= OE_ORDERJOUR::NOTES(1%)
	WP_JOB::REFNO		= OE_ORDERJOUR::CUSNUM

2100	!
	! Get the records that match in the tables
	!
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, KEY #0% EQ WP_JOB::LOCATION, REGARDLESS
	USE
		UTL_LOCATION::LOCNAME = ""
		UTL_LOCATION::ADDRESS1 = ""
		UTL_LOCATION::ADDRESS2 = ""
		UTL_LOCATION::CITY = ""
		UTL_LOCATION::STATE = ""
		UTL_LOCATION::ZIP = ""

		CONTINUE 2120 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

2120	WHEN ERROR IN
		GET #JC_TYPE.CH%, KEY #0% EQ WP_JOB::TTYPE, REGARDLESS
	USE
		JC_TYPE::DESCR = ""

		CONTINUE 2130 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

2130	WHEN ERROR IN
		GET #JC_CLASS.CH%, KEY #0% EQ WP_JOB::CLASS
	USE
		JC_CLASS::DESCR = ""

		CONTINUE 2150 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
	END WHEN

2150	GOSUB PrintStmt
	GOSUB PrintReq

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

	CLOSE OE_ORDERJOUR.CH%
	CALL ASSG_FREECHANNEL(OE_ORDERJOUR.CH%)

	CLOSE WP_CONTROL.CH%
	CALL ASSG_FREECHANNEL(WP_CONTROL.CH%)

	CLOSE UTL_REPORT.CH%
	CALL ASSG_FREECHANNEL(UTL_REPORT.CH%)

	EXIT FUNCTION

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
18040	WHEN ERROR IN
		GET #WP_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE 18090 IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	IF (WP_ORDERLINE::JOB = WP_JOB::JOB)
	THEN
		CURRENT_LINE% = CURRENT_LINE% + 1
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
	GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER

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

	RETURN

	%PAGE

 PrintReq:
18500	!***************************************************************
	! Print the Statement now
	!***************************************************************

	LINE_COUNT%  = 0%
	BODY_COUNT%  = 0%
	PAGE_NUMBER% = 1%

	CURRENT_LINE% = 0%

18530	!
	! Need to get a line so we have the operation number
	!
	WHEN ERROR IN
		FIND #WP_REQLINE.CH%, KEY #0% EQ WP_JOB::JOB
	USE
		CONTINUE 18590 IF ERR = 155% OR ERR = 5%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

 ReadReqLine:
18540	WHEN ERROR IN
		GET #WP_REQLINE.CH%
	USE
		CONTINUE 18590 IF ERR = 11%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Make sure the line record matches the header record
	!
	GOTO 18590 IF WP_REQLINE::JOB <> WP_JOB::JOB

	!
	! Have we changed line or operation numbers?
	!
	IF CURRENT_LINE% AND WP_REQLINE::LLINE + WP_REQLINE::OPERATION <> &
		WP_REQLINE_OLD::LLINE + WP_REQLINE_OLD::OPERATION
	THEN
		WP_REQLINE_NEW = WP_REQLINE
		WP_REQLINE = WP_REQLINE_OLD

		GOSUB PrintReqBottom

		WP_REQLINE = WP_REQLINE_NEW
		PAGE_NUMBER% = 1%
	END IF

	!
	! Assign a requisition number if we don't already have one
	!
	GOSUB CheckReqNum IF WP_REQLINE::REQNUM = ""

18560	REC_COUNT% = REC_COUNT% + 1%

	CURRENT_LINE% = CURRENT_LINE% + 1%

	WP_REQLINE::REQLINE = FORMAT$(REC_COUNT%, "<0>###")

	WHEN ERROR IN
		UPDATE #WP_REQLINE.CH%
	USE
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	WP_REQLINE_OLD = WP_REQLINE

	IF REC_COUNT% = 1%
	THEN
		!
		! Print the top of statement
		!
		LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_REQTOP%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

	GOSUB DumpReqLines

	GOTO ReadReqLine

18590	GOSUB PrintReqBottom IF CURRENT_LINE%

	RETURN

 DumpReqLines:
18700	!*******************************************************************
	! Dump all collected lines to invoice
	!*******************************************************************

	!
	! Print all the lines
	!
	!
	! Skip to a new page if necessary
	!
	GOSUB NewReqPage IF BODY_COUNT% >= FORM_GROUP(FRM_REQBODY%)::NUMBER

18710	!
	! Get Product Description
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ WP_REQLINE::PRODUCT, REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = ""

		CONTINUE 18720 IF ERR = 155% OR ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

18720	!
	! Get BINMAP location
	!
	WHEN ERROR IN
		GET #IC_BINMAP.CH%, KEY #0% EQ WP_REQLINE::PRODUCT + &
			WP_JOB::LOCATION, REGARDLESS
	USE
		IC_BINMAP::BIN(I%) = "" FOR I% = 0% TO 3%

		CONTINUE 18730 IF ERR = 155% OR ERR = 5%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

18730	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,))

	ONHAND_STOCK  = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
	ALLOC_STOCK   = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)
	ONORDER_STOCK = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)

	AVAIL_STOCK = ONHAND_STOCK + ALLOC_STOCK
	FREE_STOCK  = AVAIL_STOCK + ONORDER_STOCK - IC_BINMAP::SAFETY

	!
	! Print one line
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_REQBODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

18790	RETURN

	%PAGE

 PrintReqBottom:
	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_REQBODY%)::NUMBER

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_REQBODY%)::NUMBER

	!
	! Print the bottom of statement
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_REQBOTTOM%, &
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

	LINE_COUNT%, BODY_COUNT%  = 0%

	REC_COUNT%, CURRENT_LINE% = 0%

	RETURN

	%PAGE

 CheckReqNum:
	GOTO 18830 IF CURRENT_LINE%

18800	WHEN ERROR IN
		GET #WP_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "WP_CONTROL"
		CONTINUE HelpError
	END WHEN

 AssignReqNum:
	V% = FUNC_INCREMENT(WP_CONTROL::REQNUM)

	!
	! Make sure that this requisition number has never been used
	!
18810	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, KEY #1% GE WP_CONTROL::REQNUM, REGARDLESS
	USE
		CONTINUE 18820 IF ERR = 155% OR ERR = 5%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO  AssignReqNum IF WP_REQREGISTER::REQNUM = WP_CONTROL::REQNUM

18820	WHEN ERROR IN
		UPDATE #WP_CONTROL.CH%
		UNLOCK #WP_CONTROL.CH%
	USE
		FILENAME$ = "WP_CONTROL"
		CONTINUE HelpError
	END WHEN

18830	WP_REQLINE::REQNUM = WP_CONTROL::REQNUM

	RETURN

	%PAGE

 NewReqPage:
	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_REQBODY%)::NUMBER

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_REQBODY%)::NUMBER

	!
	! Print the bottom of statement
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_REQSUBBOTTOM%, &
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
		FRM_REQTOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

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
		CALL HELP_34MESSAGE(SCOPE, "Job form is missing", "E", &
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
	FRM_REQTOP% = 0%
	FRM_REQBODY% = 0%
	FRM_REQBOTTOM% = 0%
	FRM_REQSUBBOTTOM% = 0%

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

		CASE "FRM-REQTOP"
			FRM_REQTOP% = I%

		CASE "FRM-REQBODY"
			FRM_REQBODY% = I%

		CASE "FRM-REQBOTTOM"
			FRM_REQBOTTOM% = I%

		CASE "FRM-REQSUBBOTTOM"
			FRM_REQSUBBOTTOM% = I%

		END SELECT

	NEXT I%

	FRM_SUBBOTTOM%    = FRM_BOTTOM% IF FORM_SUBBOTTOM% = 0%
	FRM_REQSUBBOTTOM% = FRM_REQBOTTOM% IF FORM_REQSUBBOTTOM% = 0%

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
	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", "Do you want an alignment form?  " + &
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

	END FUNCTION



20000	SUB WP_OUTP_JOB_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP (JC_TYPE)		JC_TYPE_CDD		JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
		MAX_LINE%, &
		HEADER_TOTAL, &
		LINE_TOTAL, &
		ROLLCOUNT$ = 4%, &
		PAGE_NUMBER%, &
		REC_COUNT%, &
		ONHAND_STOCK, &
		ALLOC_STOCK, &
		ONORDER_STOCK, &
		AVAIL_STOCK, &
		FREE_STOCK

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

	CASE "OE_ORDERJOUR::ORDNUM"
		TEXTVALUE$ = OE_ORDERJOUR::ORDNUM

	CASE "OE_ORDERJOUR::ORDDATE"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::ORDDATE, 8%)

	CASE "OE_ORDERJOUR::ORDDATE6"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::ORDDATE, 6%)

	CASE "OE_ORDERJOUR::ORDTYPE"
		TEXTVALUE$ = OE_ORDERJOUR::ORDTYPE

	CASE "OE_ORDERJOUR::ORDCAT"
		TEXTVALUE$ = OE_ORDERJOUR::ORDCAT

	CASE "OE_ORDERJOUR::CUSNUM"
		TEXTVALUE$ = OE_ORDERJOUR::CUSNUM

	CASE "OE_ORDERJOUR::DISC"
		REALVALUE = OE_ORDERJOUR::DISC

	CASE "OE_ORDERJOUR::MISC"
		REALVALUE = OE_ORDERJOUR::MISC

	CASE "OE_ORDERJOUR::SHIPNAM"
		TEXTVALUE$ = OE_ORDERJOUR::SHIPNAM

	CASE "OE_ORDERJOUR::ADD1"
		TEXTVALUE$ = OE_ORDERJOUR::ADD1

	CASE "OE_ORDERJOUR::ADD2"
		TEXTVALUE$ = OE_ORDERJOUR::ADD2

	CASE "OE_ORDERJOUR::ADD3"
		TEXTVALUE$ = OE_ORDERJOUR::ADD3

	CASE "OE_ORDERJOUR::CITY"
		TEXTVALUE$ = OE_ORDERJOUR::CITY

	CASE "OE_ORDERJOUR::STATE"
		TEXTVALUE$ = OE_ORDERJOUR::STATE

	CASE "OE_ORDERJOUR::ZIP"
		TEXTVALUE$ = OE_ORDERJOUR::ZIP

	CASE "OE_ORDERJOUR::COUNTRY"
		TEXTVALUE$ = OE_ORDERJOUR::COUNTRY

	CASE "OE_ORDERJOUR:ADDLINE1"	! Substitute Ship to Address
		TEXTVALUE$ = OE_ORDERJOUR.ADDLINE$(1%)

	CASE "OE_ORDERJOUR:ADDLINE2"	! Substitute Ship to Address
		TEXTVALUE$ = OE_ORDERJOUR.ADDLINE$(2%)

	CASE "OE_ORDERJOUR:ADDLINE3"	! Substitute Ship to Address
		TEXTVALUE$ = OE_ORDERJOUR.ADDLINE$(3%)

	CASE "OE_ORDERJOUR:ADDLINE4"	! Substitute Ship to Address
		TEXTVALUE$ = OE_ORDERJOUR.ADDLINE$(4%)

	CASE "OE_ORDERJOUR::CUSTPO"
		TEXTVALUE$ = OE_ORDERJOUR::CUSTPO

	CASE "OE_ORDERJOUR::SHIPDATE"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::SHIPDATE, 8%)

	CASE "OE_ORDERJOUR::SHIPDATE6"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::SHIPDATE, 6%)

	CASE "OE_ORDERJOUR::SHIPVIA"
		TEXTVALUE$ = OE_ORDERJOUR::SHIPVIA

	CASE "OE_ORDERJOUR::TERMS"
		TEXTVALUE$ = OE_ORDERJOUR::TERMS

	CASE "OE_ORDERJOUR::SALESTAX"
		REALVALUE = OE_ORDERJOUR::SALESTAX

	CASE "OE_ORDERJOUR::LOCATION"
		TEXTVALUE$ = OE_ORDERJOUR::LOCATION

	CASE "OE_ORDERJOUR::OPERATOR"
		TEXTVALUE$ = OE_ORDERJOUR::OPERATOR

	CASE "OE_ORDERJOUR::COMMAMT"
		REALVALUE = OE_ORDERJOUR::COMMAMT

	CASE "OE_ORDERJOUR::COMMPERC"
		REALVALUE = OE_ORDERJOUR::COMMPERC

	CASE "OE_ORDERJOUR::SALESMAN"
		TEXTVALUE$ = OE_ORDERJOUR::SALESMAN

	CASE "OE_ORDERJOUR::CREASON"
		TEXTVALUE$ = OE_ORDERJOUR::CREASON

	CASE "OE_ORDERJOUR::SALCOMM"
		REALVALUE = OE_ORDERJOUR::SALCOMM

	CASE "OE_ORDERJOUR::HANDLING"
		REALVALUE = OE_ORDERJOUR::HANDLING

	CASE "OE_ORDERJOUR::AMTPAID"
		REALVALUE = OE_ORDERJOUR::AMTPAID

	CASE "OE_ORDERJOUR::CHECK"
		TEXTVALUE$ = OE_ORDERJOUR::CHECK

	CASE "OE_ORDERJOUR::DEPOSIT"
		TEXTVALUE$ = OE_ORDERJOUR::DEPOSIT

	CASE "OE_ORDERJOUR::NOTE1"
		TEXTVALUE$ = OE_ORDERJOUR::NOTES(0%)

	CASE "OE_ORDERJOUR::NOTE2"
		TEXTVALUE$ = OE_ORDERJOUR::NOTES(1%)

	CASE "OE_ORDERJOUR::NOTE3"
		TEXTVALUE$ = OE_ORDERJOUR::NOTES(2%)

	CASE "OE_ORDERJOUR::MISCACCT"
		TEXTVALUE$ = OE_ORDERJOUR::MISCACCT

	CASE "OE_ORDERJOUR::TRANDATE"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::TRANDATE, 8%)

	CASE "OE_ORDERJOUR::TRANDATE6"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::TRANDATE, 6%)

	CASE "OE_ORDERJOUR::TRANTIME"
		TEXTVALUE$ = PRNT_TIME(OE_ORDERJOUR::TRANTIME, 2%)

	CASE "OE_ORDERJOUR::INVNUM"
		TEXTVALUE$ = OE_ORDERJOUR::INVNUM

	CASE "OE_ORDERJOUR::FREIGHT"
		REALVALUE = OE_ORDERJOUR::FREIGHT

	CASE "OE_ORDERJOUR::TAXCODE"
		TEXTVALUE$ = OE_ORDERJOUR::TAXCODE

	CASE "OE_ORDERJOUR::TAXFLAG"
		TEXTVALUE$ = OE_ORDERJOUR::TAXFLAG

	CASE "OE_ORDERJOUR::SHIPLIN"
		TEXTVALUE$ = OE_ORDERJOUR::SHIPLIN

	CASE "OE_ORDERJOUR::PAYMNT"
		REALVALUE = OE_ORDERJOUR::PAYMNT

	!************************************************************
	! Fields for the Order Entery Order Journal Header file
	!************************************************************

	CASE "WP_JOB::JOB"
		TEXTVALUE$ = WP_JOB::JOB

	CASE "WP_JOB::BDATE"
		TEXTVALUE$ = PRNT_DATE(WP_JOB::BDATE, 8%)

	CASE "WP_JOB::BDATE6"
		TEXTVALUE$ = PRNT_DATE(WP_JOB::BDATE, 6%)

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

	CASE "WP_ORDERLINE::START_DATE6"
		TEXTVALUE$ = PRNT_DATE(WP_ORDERLINE::START_DATE, 6%)

	CASE "WP_ORDERLINE::COMP_DATE"
		TEXTVALUE$ = PRNT_DATE(WP_ORDERLINE::COMP_DATE, 8%)

	CASE "WP_ORDERLINE::COMP_DATE6"
		TEXTVALUE$ = PRNT_DATE(WP_ORDERLINE::COMP_DATE, 6%)

	CASE "WP_ORDERLINE::DESCR"
		TEXTVALUE$ = WP_ORDERLINE::DESCR

	CASE "WP_ORDERLINE::TTYPE"
		TEXTVALUE$ = WP_ORDERLINE::TTYPE

	!************************************************************
	! Fields for the Requisition Entry Line file
	!************************************************************

	CASE "WP_REQLINE::JOB"
		TEXTVALUE$ = WP_REQLINE::JOB

	CASE "WP_REQLINE::LLINE"
		TEXTVALUE$ = WP_REQLINE::LLINE

	CASE "WP_REQLINE::REQNUM"
		TEXTVALUE$ = CONV_STRING(WP_REQLINE::REQNUM, CMC$_LEFT)

	CASE "WP_REQLINE::OPERATION"
		TEXTVALUE$ = WP_REQLINE::OPERATION

	CASE "WP_REQLINE::PRODUCT"
		TEXTVALUE$ = WP_REQLINE::PRODUCT

	CASE "WP_REQLINE::QTY"
		REALVALUE  = WP_REQLINE::QTY

	CASE "WP_REQLINE::REQLINE"
		TEXTVALUE$  = WP_REQLINE::REQLINE

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

	CASE "PD_PRODUCT::LABEL"
		TEXTVALUE$ = PD_PRODUCT::LABEL

	CASE "PD_PRODUCT::METHOD"
		TEXTVALUE$ = PD_PRODUCT::METHOD

	CASE "PD_PRODUCT::BDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT::BDATE, 8%)

	CASE "PD_PRODUCT::BDATE6"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT::BDATE, 6%)

	CASE "PD_PRODUCT::SSTATUS"
		TEXTVALUE$ = PD_PRODUCT::SSTATUS

	CASE "PD_PRODUCT::EDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT::EDATE, 8%)

	CASE "PD_PRODUCT::EDATE6"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT::EDATE, 6%)

	CASE "PD_PRODUCT::SECONDARY_CODE"
		TEXTVALUE$ = PD_PRODUCT::SECONDARY_CODE

	CASE "PD_PRODUCT::WEIGHT"
		REALVALUE = PD_PRODUCT::WEIGHT

	CASE "PD_PRODUCT::BOMUOM"
		TEXTVALUE$ = PD_PRODUCT::BOMUOM

	CASE "PD_PRODUCT::PRODUCT_FACTOR"
		REALVALUE = PD_PRODUCT::PRODUCT_FACTOR

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

	CASE "PAGE_NUMBER" ! Page Number of Form
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "ROLLCOUNT" ! Job Line number (assigned when printed)
		TEXTVALUE$ = ROLLCOUNT$

	CASE "REC_COUNT" ! Requisition count (line)
		REALVALUE = REC_COUNT%
		TEXTVALUE$ = NUM1$(REC_COUNT%)

	CASE "ONHAND_STOCK"
		REALVALUE = ONHAND_STOCK

	CASE "ALLOC_STOCK"
		REALVALUE = ALLOC_STOCK

	CASE "ONORDER_STOCK"
		REALVALUE = ONORDER_STOCK

	CASE "AVAIL_STOCK"
		REALVALUE = AVAIL_STOCK

	CASE "FREE_STOCK"
		REALVALUE = FREE_STOCK

	END SELECT

	END SUB
