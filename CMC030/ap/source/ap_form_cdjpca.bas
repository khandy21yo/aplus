1	%TITLE "Print Accounts Payable Check Attachment Using a Form"
	%SBTTL "AP_FORM_CDJPCA"
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
	! ID:APATTH
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Attachments\* option prints
	!	a supplemental remittance advice to be attached to checks when
	!	the number of items paid are too numerous to be printed on the
	!	voucher stub of the check. This attachment includes the following information:
	!	.table 3,25
	!	.te
	!	Invoice
	!	.te
	!	Date
	!	.te
	!	Description
	!	.te
	!	Gross Amount
	!	.te
	!	Discount Amount
	!	.te
	!	Net Amount
	!	.end table
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_FORM_CDJPCA/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_FORM_CDJPCA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_FORM_CDJPCA.OBJ;*
	!
	! Author:
	!
	!	10/21/87 - Robert Peterson
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	11/30/88 - Frank Starman
	!		Replace using of RFA
	!
	!	07/05/90 - Kevin Handy
	!		Modified to use formatted PO number.
	!
	!	06/04/91 - J. Shad Rydalch
	!		Added form fields for address that will keep
	!		post office happy.
	!
	!	03/05/92 - Dan Perkins
	!		Use CONV_STRING instead of PRNT_PO.
	!
	!	06/09/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM.
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/12/93 - Kevin Handy
	!		Fixed bug where would get stuck if there was
	!		a duplicated check number.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	03/20/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/20/96 - Kevin Handy
	!		Fix handling of discount amount.
	!
	!	06/04/96 - Kevin Handy
	!		Added batch number to AP_CDJ.
	!
	!	06/07/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/09/97 - Kevin Handy
	!		Lose PRNT.CH% variable
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/98 - Kevin Handy
	!		Don't bother eraseing SMG_SCREEN_DATA, which
	!		is never created.
	!
	!	05/10/99 - Kevin Handy
	!		Allow for a form name for the attachments.
	!		If blank, work like it used to.
	!
	!	07/07/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ)	AP_CDJ_CDD	AP_CDJ

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	MAP (ATTACHMENT_FORM) &
		TOTAL_AMT, &
		TOTAL_DISC, &
		TOTAL_NET, &
		PAGE_COUNT, &
		AP_VENDOR.ADDLINE$(3%) = 50%, &
		AP_VENDOR.POADDLINE$(3%) = 50%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "APATTH"

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_FORM", AP_FORM.DEV$, STAT%)

	!*******************************************************************
	! Initilize Attachment form
	!*******************************************************************

	!
	! Get form from the AP form library
	!
	SMG_STATUS% = OUTP_FORMINIT(AP_FORM.DEV$ + "AP_FORM", "ATTACH", &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "Attachment form is missing", &
			"E", SCOPE::PRG_PROGRAM, REPORT$, NUM1$(SMG_STATUS%))
		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_TOP% = 0%
	FRM_VOUCHER% = 0%
	FRM_BOT% = 0%
	FRM_BODY% = 0%
	FRM_LENGTH% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-VOUCHER"
			FRM_VOUCHER% = I%

		CASE "FRM-BOT"
			FRM_BOT% = I%

		CASE "FRM-BODY"
			FRM_BODY% = I%

		CASE "FRM-LENGTH"
			FRM_LENGTH% = I%

		END SELECT

	NEXT I%

	!***************************************************************
	! Open all of the files
	!***************************************************************

300	!
	! Open vendor master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

340	!
	! Get Report
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	CDJ_BATCH$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)
	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	This field specifies which batch to print.
	!
	! Index:
	!	.x Batch Number>Print Checks
	!	.x Print Checks>Batch Number
	!
	!--

	FORM_NAME$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	IF FORM_NAME$ = ""
	THEN
		REPORT$ = "APATTH"
	ELSE
		REPORT$ = "APATTH$" + FORM_NAME$
	END IF

	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	This field specifies which batch to print.
	!
	! Index:
	!	.x Batch Number>Print Checks
	!	.x Print Checks>Batch Number
	!
	!--

400	!
	! Open cash dispersments file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.OPN"
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	!
	! GOTO aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through CDJ file
	!*******************************************************************

	VENNUM$ = ""

	WHEN ERROR IN
		RESET #AP_CDJ.CH%, KEY #1%
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

2010	WHEN ERROR IN
		GET #AP_CDJ.CH%, REGARDLESS
	USE
		CONTINUE 3000
	END WHEN

	IF AP_CDJ::CKNUM = ""
	THEN
		GOTO 2010
	END IF

2020	!
	! Check for new vendor number
	!
	IF (CKNUM$ <> AP_CDJ::CKNUM) OR (VENNUM$ <> AP_CDJ::VENNUM)
	THEN
		IF VENNUM$ <> ""
		THEN
			IF TRAN_COUNT% > FORM_GROUP(FRM_VOUCHER%)::NUMBER OR &
				FUNC_ROUND(TOTAL_NET, 2%) = 0.0
			THEN
				GOSUB PrintAttachment

				GOTO ExitProgram IF UTL_REPORTX::STAT
			END IF
		END IF

		TOTAL_AMT, TOTAL_DISC, TOTAL_NET = 0.0
		TRAN_COUNT% = 0%
	END IF

	CKNUM$ = AP_CDJ::CKNUM + ""
	VENNUM$ = AP_CDJ::VENNUM + ""

	TRAN_COUNT% = TRAN_COUNT% + 1%

	TOTAL_AMT = FUNC_ROUND(TOTAL_AMT + AP_CDJ::CKAMT + &
		(AP_CDJ::DISAMT - AP_CDJ::DISC_LOST_AMT), 2%)
	TOTAL_DISC = FUNC_ROUND(TOTAL_DISC + AP_CDJ::DISAMT - &
		AP_CDJ::DISC_LOST_AMT, 2%)
	TOTAL_NET = FUNC_ROUND(TOTAL_NET + AP_CDJ::CKAMT, 2%)

	GOTO 2010

3000	!*******************************************************************
	! Found the end of the CDJ file
	!*******************************************************************

	IF VENNUM$<> ""
	THEN
		IF TRAN_COUNT% > FORM_GROUP(FRM_VOUCHER%)::NUMBER OR &
			FUNC_ROUND(TOTAL_NET, 2%) = 0.0
		THEN
			GOSUB PrintAttachment
		END IF
	END IF

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	CLOSE AP_CDJ.CH%

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

 PrintAttachment:
18000	!***************************************************************
	! Print the Attachment now
	!***************************************************************

	UTL_REPORTX::LINENO = 0%
	UTL_REPORTX::PAGENO = 0%
	PAGE_COUNT = 1%
	TEST_COUNT% = 0%

	WHEN ERROR IN
		GET #AP_CDJ.CH%, KEY #1% EQ CKNUM$ + VENNUM$, REGARDLESS
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

18010	!
	! Find vendor for this CDJ
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ AP_CDJ::VENNUM, REGARDLESS
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(AP_VENDOR::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.ADDLINE$(I%) = &
			EDIT$(AP_VENDOR::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AP_VENDOR::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.ADDLINE$(I%) = &
			EDIT$(AP_VENDOR::ADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	AP_VENDOR.ADDLINE$(I%) = EDIT$(EDIT$(AP_VENDOR::CITY, 128%) + ", " + &
		AP_VENDOR::STATE + " " + AP_VENDOR::ZIP + " " + &
		AP_VENDOR::COUNTRY, 8% + 16% + 32% + 128%)

	AP_VENDOR.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

	I% = 0%

	IF EDIT$(AP_VENDOR::POADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.POADDLINE$(I%) = &
			EDIT$(AP_VENDOR::POADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AP_VENDOR::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.POADDLINE$(I%) = &
			EDIT$(AP_VENDOR::POADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	AP_VENDOR.POADDLINE$(I%) = &
		EDIT$(EDIT$(AP_VENDOR::POCITY, 128%) + ", " + &
		AP_VENDOR::POSTATE + " " + AP_VENDOR::POZIP + " " + &
		AP_VENDOR::POCOUNTRY, 8% + 16% + 32% + 128%)

	! Blank rest of 'em out
	AP_VENDOR.POADDLINE$(LOOP%) = "" FOR LOOP% = I% + 1% TO 3%

	!
	! Print the top of the attachment
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

18020	!
	! See if at end of the group for one vendor
	!
	GOTO 18090 IF AP_CDJ::CKNUM <> CKNUM$ OR AP_CDJ::VENNUM <> VENNUM$

	!
	! Increment body counter, check to see if need to page
	!
	IF TEST_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER
	THEN
		!
		! Print to the bottom of the form
		!
		LINE_COUNT% = UTL_REPORTX::LINENO
		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
			FOR LOOP% = LINE_COUNT% + 1% TO &
			FORM_GROUP(FRM_LENGTH%)::NUMBER

		PAGE_COUNT = PAGE_COUNT + 1%
		UTL_REPORTX::LINENO = 0%
		TEST_COUNT% = 0%

		!
		! Print the top of the attachment
		!
		SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_TOP%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

	!
	! Print the lines
	!
	TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_VOUCHER%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_CDJ.CH%
	USE
		CONTINUE 18090
	END WHEN

	GOTO 18020

18090	!
	! Print to the bottom of the body
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = TRAN_COUNT% + 1% TO &
		FORM_GROUP(FRM_BODY%)::NUMBER

	!
	! Print the bottom of the form
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOT%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Print to the bottom of the form
	!
	LINE_COUNT% = UTL_REPORTX::LINENO
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO &
		FORM_GROUP(FRM_LENGTH%)::NUMBER

18100	!
	! Do the next group
	!
	RETURN

	%PAGE

 Alignment:
	!*******************************************************************
	! Print alignment form, if desireable
	!*******************************************************************

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	UTL_REPORTX::LINENO = 0%
	UTL_REPORTX::PAGENO = 0%

	SCOPE::PRG_ITEM = "ALIGNMENT"
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

	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	!
	! Display three line
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_VOUCHER%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%) &
		FOR I% = 1% TO 3%

	!
	! Print lines to botton of the voucher
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = 4% TO FORM_GROUP(FRM_BODY%)::NUMBER

	!
	! Print the bottom of the form
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOT%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	!
	! Print to the bottom of the form
	!
	LINE_COUNT% = UTL_REPORTX::LINENO
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO &
		FORM_GROUP(FRM_LENGTH%)::NUMBER

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
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ)	AP_CDJ_CDD	AP_CDJ

	MAP (ATTACHMENT_FORM) &
		TOTAL_AMT, &
		TOTAL_DISC, &
		TOTAL_NET, &
		PAGE_COUNT, &
		AP_VENDOR.ADDLINE$(3%) = 50%, &
		AP_VENDOR.POADDLINE$(3%) = 50%

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
	! Fields for the AP_VENDOR file
	!************************************************************

	CASE "AP_VENDOR::VENNUM"
		TEXTVALUE$ = AP_VENDOR::VENNUM

	CASE "AP_VENDOR::VENNAM"
		TEXTVALUE$ = AP_VENDOR::VENNAM

	CASE "AP_VENDOR::ADD1"
		TEXTVALUE$ = AP_VENDOR::ADD1

	CASE "AP_VENDOR::ADD2"
		TEXTVALUE$ = AP_VENDOR::ADD2

	CASE "AP_VENDOR::CITY"
		TEXTVALUE$ = AP_VENDOR::CITY

	CASE "AP_VENDOR::STATE"
		TEXTVALUE$ = AP_VENDOR::STATE

	CASE "AP_VENDOR::ZIP"
		TEXTVALUE$ = AP_VENDOR::ZIP

	CASE "AP_VENDOR::COUNTRY"
		TEXTVALUE$ = AP_VENDOR::COUNTRY

	CASE "AP_VENDOR.ADDLINE1"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(1%)

	CASE "AP_VENDOR.ADDLINE2"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(2%)

	CASE "AP_VENDOR.ADDLINE3"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(3%)

	CASE "AP_VENDOR::PHONE"
		TEXTVALUE$ = AP_VENDOR::PHONE

	CASE "AP_VENDOR::POADD1"
		TEXTVALUE$ = AP_VENDOR::POADD1

	CASE "AP_VENDOR::POADD2"
		TEXTVALUE$ = AP_VENDOR::POADD2

	CASE "AP_VENDOR::POCITY"
		TEXTVALUE$ = AP_VENDOR::POCITY

	CASE "AP_VENDOR::POSTATE"
		TEXTVALUE$ = AP_VENDOR::POSTATE

	CASE "AP_VENDOR::POZIP"
		TEXTVALUE$ = AP_VENDOR::POZIP

	CASE "AP_VENDOR::POCOUNTRY"
		TEXTVALUE$ = AP_VENDOR::POCOUNTRY

	CASE "AP_VENDOR.POADDLINE1"	! Substitute Purchase Order Address
		TEXTVALUE$ = ADDLINE$(1%)

	CASE "AP_VENDOR.POADDLINE2"	! Substitute Purchase Order Address
		TEXTVALUE$ = ADDLINE$(2%)

	CASE "AP_VENDOR.POADDLINE3"	! Substitute Purchase Order Address
		TEXTVALUE$ = ADDLINE$(3%)

	CASE "AP_VENDOR::POPHONE"
		TEXTVALUE$ = AP_VENDOR::POPHONE

	CASE "AP_VENDOR::PURGE"
		TEXTVALUE$ = AP_VENDOR::PURGE

	CASE "AP_VENDOR::FEDID"
		TEXTVALUE$ = AP_VENDOR::FEDID

	CASE "AP_VENDOR::FLG1099"
		TEXTVALUE$ = AP_VENDOR::FLG1099

	CASE "AP_VENDOR::DUEDAYS"
		REALVALUE = AP_VENDOR::DUEDAYS
		TEXTVALUE$ = NUM1$(AP_VENDOR::DUEDAYS)

	CASE "AP_VENDOR::DUEDAT"
		TEXTVALUE$ = AP_VENDOR::DUEDATE

	CASE "AP_VENDOR::DISDAYS"
		REALVALUE = AP_VENDOR::DISDAYS
		TEXTVALUE$ = NUM1$(AP_VENDOR::DISDAYS)

	CASE "AP_VENDOR::DISDATE"
		TEXTVALUE$ = AP_VENDOR::DISDATE

	CASE "AP_VENDOR::DISCPER"
		REALVALUE = AP_VENDOR::DISCPER / 100.0

	CASE "AP_VENDOR::ALPSRT"
		TEXTVALUE$ = AP_VENDOR::ALPSRT

	!************************************************************
	! Fields for the AP_CDJ file
	!************************************************************

	CASE "AP_CDJ::VENNUM"
		TEXTVALUE$ = AP_CDJ::VENNUM

	CASE "AP_CDJ::TRANKEY"
		TEXTVALUE$ = AP_CDJ::TRANKEY

	CASE "AP_CDJ::INVNUM"
		TEXTVALUE$ = AP_CDJ::INVNUM

	CASE "AP_CDJ::INVDAT"
		TEXTVALUE$ = PRNT_DATE(AP_CDJ::INVDAT, 8%)

	CASE "AP_CDJ::INVAMT"
		REALVALUE = AP_CDJ::INVAMT

	CASE "AP_CDJ::CKAMT"
		REALVALUE = AP_CDJ::CKAMT

	CASE "AP_CDJ::CKNUM"
		TEXTVALUE$ = AP_CDJ::CKNUM

	CASE "AP_CDJ::CKDAT"
		TEXTVALUE$ = PRNT_DATE(AP_CDJ::CKDAT, 8%)

	CASE "AP_CDJ::CKDESC"
		TEXTVALUE$ = AP_CDJ::CKDESC

	CASE "AP_CDJ::DISCDAT"
		TEXTVALUE$ = PRNT_DATE(AP_CDJ::DISCDAT, 8%)

	CASE "AP_CDJ::DISAMT"
		REALVALUE = AP_CDJ::DISAMT

	CASE "AP_CDJ::DISC_LOST_AMT"
		REALVALUE = AP_CDJ::DISC_LOST_AMT

	CASE "AP_CDJ::DISCLOST_ACCT"
		TEXTVALUE$ = AP_CDJ::DISCLOST_ACCT

	CASE "AP_CDJ::DUEDAT"
		TEXTVALUE$ = PRNT_DATE(AP_CDJ::DUEDAT, 8%)

	CASE "AP_CDJ::PONUM"
		TEXTVALUE$ = CONV_STRING(AP_CDJ::PONUM, CMC$_LEFT)

	CASE "AP_CDJ::AP_ACCT"
		TEXTVALUE$ = AP_CDJ::AP_ACCT

	CASE "AP_CDJ::CASH_ACCT"
		TEXTVALUE$ = AP_CDJ::CASH_ACCT

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "TOTAL_AMT"
		REALVALUE = TOTAL_AMT

	CASE "TOTAL_DISC"
		REALVALUE = TOTAL_DISC

	CASE "TOTAL_NET", "AMOUNT"
		REALVALUE = TOTAL_NET

	CASE "PAGE"
		REALVALUE = PAGE_COUNT
		TEXTVALUE$ = NUM1$(PAGE_COUNT)
	END SELECT

	END SUB
