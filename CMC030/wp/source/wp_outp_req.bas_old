1	%TITLE "Print Requisition Form from Journals"
	%SBTTL "WP_OUTP_REQ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_OUTP_REQ(STRING JOB, STRING BATCH, LONG FLAG)

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
	! ID:WPREQ
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints a WP Requisition Form resulting from a
	!	print request within a journal maintenance program.
	!	.lm -5
	!
	! Index:
	!	.x Print>Requisition>Form
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_OUTP_REQ/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_OUTP_REQ
	!	$ DELETE WP_OUTP_REQ.OBJ;*
	!
	! Author:
	!
	!	04/05/93 - Dan Perkins
	!
	! Modification history:
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
	!	05/01/97 - Kevin Handy
	!		Use OUTP_INITFORM function.
	!
	!	08/25/97 - Kevin Handy
	!		Lose definition of FUND_FILEXISTS and LIB$SET_SYMBOL
	!		which are never used.
	!
	!	05/21/98 - Kevin Handy
	!		Fix file names in error messages.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add several REGARDLESS clauses
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
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE
	DECLARE			WP_REQLINE_CDD		WP_REQLINE_NEW, &
							WP_REQLINE_OLD

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(10%)

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
		PAGE_NUMBER%, &
		REC_COUNT%, &
		ONHAND.STOCK, &
		ALLOC.STOCK, &
		ONORDER.STOCK, &
		AVAIL.STOCK, &
		FREE.STOCK

	COM (CH_IC_BINMAP)	IC_BINMAP.CH%

	COM (CH_JC_CLASS)	JC_CLASS.CH%
	COM (CH_JC_TYPE)	JC_TYPE.CH%

	COM (CH_PD_PRODUCT)	PD_PRODUCT.CH%

	COM (CH_WP_CONTROL)	WP_CONTROL.CH%
	COM (CH_WP_REQREGISTER)	WP_REQREGISTER.CH%

	COM (CH_UTL_LOCATION)	UTL_LOCATION.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	CALL ENTR_3MESSAGE(SCOPE, "", 1%+16%)

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	SELECT FLAG

	CASE 0%, 1%
		REPORT$ = "WPREQ"

	END SELECT

	!
	! Look up device
	!
	CALL  READ_DEVICE("WP_FORM", WP_FORM.DEV$, STAT%)

500	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	!
	! Plug the passed order number and batch number into the report file
	! also default the sortby sequence to Order number "O"
	!
	INITSTR$ = "00" + BATCH + ",01O,02" + JOB + ",03" + JOB + ",04*"

	!
	! Initialize form
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, INITSTR$) <> CMC$_NORMAL

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field in the Order Journal Forms Report Setting Screen
	!	provides the means to enter the batch number for which form(s) are to be
	!	printed.
	!	.b
	!	Each journal file is assigned two (2) alphanumeric characters as a batch
	!	number.
	!	.lm -5
	!
	! Index:
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field permits the user to select the order in which the
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
	!
	!--

	FROM.ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field contents will cause the forms to print beginning with
	!	the number entered. The value must be in agreement with the value entered in
	!	field (02) Sort by.
	!	.b
	!	A blank field will cause the forms to print beginning with the first item in
	!	the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO.ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.B
	!	.LM +5
	!	The ^*To Item\* field contents will cause the printing of the forms to end
	!	with item designated.  The value must be in agreement with the value entered
	!	in field (02) Sort by.
	!	.B
	!	A blank value in this field will cause the printing of the forms to continue
	!	until the last item in the file has been printed.
	!	.LM -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.B
	!	.LM +5
	!	The ^*Wildcard\* field provides the means to select designated items to be
	!	printed by entering a "wildcard" using the wildcarding technique. The value
	!	entered must be in agreement with the value entered in field (02) Sort by.
	!	.B
	!	For information on "wildcarding", refer to Appendix B.
	!	.LM -5
	!
	! Index:
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
	!
	!--

	REQUISITION$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Requisition Number\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Number\* field provides the means to enter a selected
	!	requisition number to be printed. If a value is entered, a form for the
	!	designated requisition only will print. If this field is left blank, the system
	!	will print only records having blank requisition numbers, assigning
	!	requisition numbers as each form is printed.
	!	.lm -5
	!
	! Index:
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

	!
	! Open WIP order journal header file
	!
600	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.CRE"
	USE
		FILENAME$ = "WP_JOB_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

	!
	! Open WIP order journal line file
	!
610	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.PST"
	USE
		FILENAME$ = "WP_REQLINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

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
	! Open Location description file
	!
640	IF UTL_LOCATION.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
		USE
			CONTINUE 650 IF ERR = 5%
			FILENAME$ = "UTL_LOCATION"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open Job Type description file
	!
650	IF JC_TYPE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.OPN"
		USE
			CONTINUE 660 IF ERR = 5%
			FILENAME$ = "JC_TYPE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open Job Class description file
	!
660	IF JC_CLASS.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.OPN"
		USE
			CONTINUE 670 IF ERR = 5%
			FILENAME$ = "JC_CLASS"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open Requisition Register file
	!
670	IF WP_REQREGISTER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
		USE
			CONTINUE 680 IF ERR = 5%
			FILENAME$ = "WP_REQREGISTER"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open product description  file
	!
680	IF PD_PRODUCT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
		USE
			CONTINUE 690 IF ERR = 5%
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open binmap
	!
690	IF IC_BINMAP.CH% <= 0%
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
		IF FROM.ITEM$ = ""
		THEN
			RESET #WP_JOB.CH%
		ELSE
			FIND #WP_JOB.CH%, KEY#0% GE FROM.ITEM$
		END IF
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 3000 IF ERR = 155%
		FILENAME$ = "WP_JOB"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	!
	! Get next record
	!
	WHEN ERROR IN
		GET #WP_JOB.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "WP_JOB"
		CONTINUE HelpError
	END WHEN

	SELECT SORTBY$

	CASE "O"
		GOTO 3000 IF (WP_JOB::JOB > TO.ITEM$) &
			AND TO.ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(WP_JOB::JOB, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO 3000 IF (WP_JOB::TTYPE > TO.ITEM$) &
			AND TO.ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(WP_JOB::TTYPE, -1%), &
			WLDCRD$) = 0%

	CASE "C"
		GOTO 3000 IF (WP_JOB::CLASS > TO.ITEM$) &
			AND TO.ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(WP_JOB::CLASS, -1%), &
			WLDCRD$) = 0%

	END SELECT

2110	!
	! Location
	!
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, KEY#0% EQ WP_JOB::LOCATION, REGARDLESS
	USE
		UTL_LOCATION::LOCNAME  = ""
		UTL_LOCATION::ADDRESS1 = ""
		UTL_LOCATION::ADDRESS2 = ""
		UTL_LOCATION::CITY     = ""
		UTL_LOCATION::STATE    = ""
		UTL_LOCATION::ZIP      = ""

		CONTINUE 2120 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

2120	!
	! Type
	!
	WHEN ERROR IN
		GET #JC_TYPE.CH%, KEY#0% EQ WP_JOB::TTYPE, REGARDLESS
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
		GET #JC_CLASS.CH%, KEY#0% EQ WP_JOB::CLASS, REGARDLESS
	USE
		JC_CLASS::DESCR = ""

		CONTINUE 2150 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
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

	CLOSE IC_BINMAP.CH%
	CALL ASSG_FREECHANNEL(IC_BINMAP.CH%)

	CLOSE JC_CLASS.CH%
	CALL ASSG_FREECHANNEL(JC_CLASS.CH%)

	CLOSE JC_TYPE.CH%
	CALL ASSG_FREECHANNEL(JC_TYPE.CH%)

	CLOSE PD_PRODUCT.CH%
	CALL ASSG_FREECHANNEL(PD_PRODUCT.CH%)

	CLOSE WP_CONTROL.CH%
	CALL ASSG_FREECHANNEL(WP_CONTROL.CH%)

	CLOSE WP_REQREGISTER.CH%
	CALL ASSG_FREECHANNEL(WP_REQREGISTER.CH%)

	CLOSE UTL_LOCATION.CH%
	CALL ASSG_FREECHANNEL(UTL_LOCATION.CH%)

	EXIT FUNCTION

	%PAGE

 PrintStmt:
18000	!***************************************************************
	! Print the Statement now
	!***************************************************************

	LINE_COUNT%  = 0%
	BODY_COUNT%  = 0%
	PAGE_NUMBER% = 1%

	CURRENT_LINE% = 0%

18030	!
	! Need to get a line so we have the operation number
	!
	WHEN ERROR IN
		FIND #WP_REQLINE.CH%, KEY#0% EQ WP_JOB::JOB
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

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
	GOTO 18090 IF WP_REQLINE::JOB <> WP_JOB::JOB

	!
	! Check the requisiton field
	!
	IF REQUISITION$ = ""
	THEN
		GOTO ReadLine IF EDIT$(WP_REQLINE::REQNUM, -1%) <> ""
	ELSE
		GOTO ReadLine IF WP_REQLINE::REQNUM <> REQUISITION$
	END IF

	!
	! Have we changed operation numbers?
	!
	IF CURRENT_LINE% AND WP_REQLINE::OPERATION <> &
		WP_REQLINE_OLD::OPERATION
	THEN
		WP_REQLINE_NEW = WP_REQLINE
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

	GOSUB DumpLines

	GOTO ReadLine

18090	GOSUB PrintBottom IF CURRENT_LINE%

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

18210	!
	! Get Product Description
	!
	PD_PRODUCT::DESCRIPTION = ""

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY#0% EQ WP_REQLINE::PRODUCT, REGARDLESS
	USE
		CONTINUE 18220 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

18220	!
	! Get BINMAP location
	!
	WHEN ERROR IN
		GET #IC_BINMAP.CH%, KEY#0% EQ WP_REQLINE::PRODUCT + &
			WP_JOB::LOCATION, REGARDLESS
	USE
		IC_BINMAP::BIN(I%) = "" FOR I% = 0% TO 3%

		CONTINUE 18230 IF ERR = 155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

18230	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,))

	ONHAND.STOCK  = BALANCE(1%,1%) + BALANCE(1%,2%) + BALANCE(1%,3%)
	ALLOC.STOCK   = BALANCE(2%,1%) + BALANCE(2%,2%) + BALANCE(2%,3%)
	ONORDER.STOCK = BALANCE(3%,1%) + BALANCE(3%,2%) + BALANCE(3%,3%)

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

	LINE_COUNT%, BODY_COUNT%  = 0%

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
		GET #WP_REQREGISTER.CH%, KEY#1% GE WP_CONTROL::REQNUM, REGARDLESS
	USE
		CONTINUE 18320 IF ERR = 155%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO  AssignReqNum IF WP_REQREGISTER::REQNUM = WP_CONTROL::REQNUM

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
		CALL HELP_34MESSAGE(SCOPE, "Requisition form is missing","E", &
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

	FRM_SBUBOTTOM% = FRM_BOTTOM% IF FRM_SUBBOTTOM% = 0%

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

	END FUNCTION


20000	SUB WP_OUTP_REQ_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

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
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB

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

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	MAP (JOUR_FORM) &
		CURRENT_LINE%, &
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

	CASE "PAGE_NUMBER" ! Page Number of Form
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "REC_COUNT" ! Requisition count (line)
		REALVALUE = REC_COUNT%
		TEXTVALUE$ = NUM1$(REC_COUNT%)

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
