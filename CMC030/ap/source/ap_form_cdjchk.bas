1	%TITLE "Print Checks"
	%SBTTL "AP_FORM_CDJCHK"
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
	! ID:APCHEK
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Checks\* option prints a check
	!	for each vendor having records in the Cash Disbursements Journal.
	!	Remittance advice information is printed on the voucher stub of
	!	each check, displaying the invoice number of each item being paid,
	!	the gross amount, discount and net amount being paid.  Optional
	!	information which may be printed on a check stub includes the transaction
	!	number, the invoice date and a description of the invoice.
	!	.b
	!	Remittance advice information which contains too many lines
	!	to be printed on a voucher check stub will be printed on a separate
	!	check attachment. When such is the case, the message, "See Check
	!	Attachment", will be printed on the voucher check stub.
	!	.b
	!	The Print Attachments option must be executed in order to
	!	print the supplementary remittance advice attachments.
	!	.lm -5
	!
	! Index:
	!	.x Print Checks
	!	.x Cash Disbursements>Print Checks
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_FORM_CDJCHK/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_FORM_CDJCHK, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_FORM_CDJCHK.OBJ;*
	!
	! Author:
	!
	!	10/08/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	02/14/90 - Frank F. Starman
	!		Do not take discount again while printing checks.
	!
	!	07/05/90 - Kevin Handy
	!		Modified for formatted PO number.
	!
	!	09/21/90 - Kevin Handy
	!		Some minor clean up while looking for a bug.
	!
	!	05/21/90 - J. Shad Rydalch
	!		Added feild to select form.
	!
	!	05/31/90 - J. Shad Rydalch
	!		Added form fields for address that will keep
	!		post office happy.
	!
	!	11/25/91 - Kevin Handy
	!		NOTICE: There is a bug in several peoples forms
	!		that causes the stub to show the check - discount
	!		amount, instead of just the check amount.  This is
	!		a bug in that users form, not this program.
	!
	!	03/05/92 - Dan Perkins
	!		Use CONV_STRING in place of PRNT_PO.
	!
	!	03/12/92 - Kevin Handy
	!		Removed duplicate error trapping (check)
	!
	!	06/11/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM function.
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	01/22/93 - Kevin Handy
	!		Modified to make sure the TOTAL_NET starts
	!		out as zero when starting new vendor.
	!
	!	09/23/93 - Kevin Handy
	!		Removed JJ$ definition and READ_SYSJOB.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/26/95 - Kevin Handy
	!		Lost line that deleted SMG_BLANK display, which
	!		was never created.
	!
	!	06/04/96 - Kevin Handy
	!		Reformat Source Code.
	!		Added batch number to AP_CDJ.
	!
	!	05/09/97 - Kevin Handy
	!		Lost the PRNT.CH% variable
	!
	!	05/12/97 - Kevin Handy
	!		Reformat Source Code.
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/98 - Kevin Handy
	!		Don't bother erasing SMG_SCREEN_DATA%, which
	!		is never created
	!
	!	07/07/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/24/2001 - Kevin Handy
	!		Zero out the TOTAL_NET amount when a break check
	!		occurs.
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT) UTL_REPORT_CDD UTL_REPORT
	DECLARE UTL_REPORT_CDD UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	MAP (CHECK_FORM) &
		TOTAL_AMT, &
		TOTAL_DISC, &
		TOTAL_DISC_LOST, &
		TOTAL_NET, &
		CHECK_DATE$, &
		LAST_CKNUM, &
		AP_VENDOR.ADDLINE$(3%) = 50%, &
		AP_VENDOR.POADDLINE$(3%) = 50%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$, REPORT1$ = "APCHEK"

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_FORM", AP_FORM.DEV$, STAT%)

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

320	!
	! Open AP_CONTROL file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.MOD"
		GET #AP_CONTROL.CH%, REGARDLESS
		UNLOCK #AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	AP_CONTROL::LAST_CKNUM = XLATE(AP_CONTROL::LAST_CKNUM, &
		STRING$(48%, 0%) + "0123456789")

340	!
	! Set check number if break check has been reached
	!
	IF BREAK_FLAG%
	THEN
		SETCHECK$ = "00" + NUM1$(LAST_CKNUM)
	ELSE
		SETCHECK$ = "00" + AP_CONTROL::LAST_CKNUM
	END IF

	!
	! Get Report
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT1$, &
		SETCHECK$) <> CMC$_NORMAL

	GOTO ExitProgram IF UTL_REPORTX::STAT


	LAST_CKNUM = VAL(XLATE(UTL_REPORTX::OPTDEF(0%), &
		STRING$(48%, 0%) + "0123456789"))
	!++
	! Abstract:FLD01
	!	^*(01) Start Check _#\*
	!	.b
	!	.lm +5
	!	The ^*Start Check _#\* field
	!	enters the number of the first
	!	check which is to be printed during a check printing process.
	!	.lm -5
	!
	! Index:
	!	.x Print Checks>Start Check
	!	.x Start Check>Print Checks
	!
	!--

	BREAK_CKNUM = VAL(XLATE(UTL_REPORTX::OPTDEF(1%), &
		STRING$(48%, 0%) + "0123456789"))

	!++
	! Abstract:FLD02
	!	^*(02) Break Check _#\*
	!	.b
	!	.lm +5
	!	If there should be a break in the continuous form checks,
	!	enter in the ^*Break Check _#\* field the number of the check
	!	^&before\& the check form where the break actually occurs.  The
	!	printer would then discontinue printing with one unused check
	!	form in the printer. The "out of forms" switch on some printers
	!	would prevent the last form from being printed completely.
	!	.b
	!	When a "break" occurs, the system will return to the report
	!	settings screen, providing for the continuation of check printing
	!	after loading more check forms in the printer.
	!	.lm -5
	!
	! Index:
	!	.x Break Check>Print Checks
	!	.x Print Checks>Break Check
	!
	!--

	CHECK_DATE$ = UTL_REPORTX::OPTDEF(2%)
	!++
	! Abstract:FLD03
	!	^*(03) Check Date\*
	!	.b
	!	.lm +5
	!	The ^*Check Date\* field
	!	enters the date, usually
	!	the current date, which will print on all checks to be printed
	!	during the check printing procedure.
	!	.B
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Check Date>Print Checks
	!	.x Print Checks>Check Date
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	!++
	! Abstract:FLD04
	!	^*(04) Sort Order\*
	!	.b
	!	.lm +5
	!	The ^*Sort Order\* field determines
	!	how the report is to be printed.
	!	.b
	!	Valid choices are:
	!	.table 3,25
	!	.te
	!	^*NU\* - Vendor Number Order
	!	.te
	!	^*NA\* - Vendor Name
	!	.te
	!	^*S\*# - Alphabetical Sort
	!	.end table
	!	A blank setting will cause the report to print in numerical order.
	!	.lm -5
	!
	! Index:
	!	.x Sort Order>Print Checks
	!	.x Print Checks>Sort Order
	!
	!--

	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%

	CASE "NA"
		K_NUM% = 1%

	CASE ELSE
		K_NUM% = 2%
	END SELECT

	REPORT$ = REPORT1$ + "$" + TRM$(UTL_REPORTX::OPTDEF(4%))

	!++
	! Abstract:FLD05
	!	^*(05) Form Name\*
	!	.b
	!	.lm +5
	!	The ^*Form Name\* field enters the form which will be
	!	used for printing.
	!	.lm -5
	!
	! Index:
	!	.x Form Name
	!
	!--

	CDJ_BATCH$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)
	!++
	! Abstract:FLD06
	!	^*(06) Batch Number\*
	!	.b
	!	This field specifies which journal is to be printed.
	!
	! Index:
	!	.x Batch Number>Print Checks
	!	.x Print Checks>Batch Number
	!
	!--

	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

400	!
	! Open cash dispersments file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.UPD"
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

410	!
	! Load the Form
	!
	GOSUB LoadForm

	!
	! GOTO aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through CDJ file
	!*******************************************************************

	!
	! Jump over the reset if break check option is used
	!
	GOTO 2020 IF BREAK_FLAG%

	WHEN ERROR IN
		RESET #AP_VENDOR.CH%, KEY #K_NUM%
	USE
		CONTINUE 3000
	END WHEN

2010	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE 3000
	END WHEN

	TOTAL_NET = 0.0

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

	IF EDIT$(AP_VENDOR::POADD2, -1%) <> ""
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

	AP_VENDOR.POADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

2020	TOTAL_AMT, TOTAL_DISC, TOTAL_DISC_LOST, TOTAL_NET = 0.0

	WHEN ERROR IN
		FIND #AP_CDJ.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 2200
	END WHEN

2030	!
	! Get next cdj record
	!
	WHEN ERROR IN
		GET #AP_CDJ.CH%
	USE
		CONTINUE 2100
	END WHEN

	GOTO 2100 IF AP_CDJ::VENNUM <> AP_VENDOR::VENNUM

	IF AP_CDJ::CKNUM <> ""
	THEN
		GOTO 2030
	END IF

	TRAN_COUNT% = TRAN_COUNT% + 1%

	TOTAL_AMT = FUNC_ROUND(TOTAL_AMT + AP_CDJ::CKAMT, 2%)
	TOTAL_DISC = FUNC_ROUND(TOTAL_DISC + AP_CDJ::DISAMT, 2%)
	TOTAL_DISC_LOST = FUNC_ROUND(TOTAL_DISC_LOST + &
		AP_CDJ::DISC_LOST_AMT, 2%)
	TOTAL_NET = FUNC_ROUND(TOTAL_NET + AP_CDJ::CKAMT + &
		AP_CDJ::DISC_LOST_AMT, 2%)

	GOTO 2030

2100	!
	! Check for a break check
	!
	IF FUNC_ROUND(TOTAL_NET, 2%) <> 0.0
	THEN
		IF LAST_CKNUM > BREAK_CKNUM AND BREAK_CKNUM > 0.0
		THEN
			!
			! Erase Display
			!
			SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
			SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

			!
			! Change the width
			!
			SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS( &
				SCOPE::SMG_PBID, 80%)

			SCOPE::PRG_ITEM = "BREAK_CHECK"
			CALL ENTR_3MESSAGE(SCOPE, &
				"Break check has been reached", 0%)

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
				GOTO 2100

			END SELECT

			BREAK_FLAG% = -1%

			GOTO 340
		END IF
	END IF

	!
	! Is there anything to print
	!
	IF TRAN_COUNT% > 0%
	THEN
		GOSUB PrintCheck

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	TOTAL_AMT, TOTAL_DISC, TOTAL_DISC_LOST, TOTAL_NET = 0.0
	TRAN_COUNT% = 0%

2200	!
	! Go get the next vendor record
	!
	GOTO 2010

3000	!*******************************************************************
	! Found the end of the vendor file
	!*******************************************************************

	%PAGE

4000	!********************************************************************
	! Get control record and update check number
	!********************************************************************
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	AP_CONTROL::LAST_CKNUM = NUM1$(LAST_CKNUM)

4010	WHEN ERROR IN
		UPDATE #AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

4030	!
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

 PrintCheck:
18000	!***************************************************************
	! Print the check now
	!***************************************************************

	GOTO 18100 IF FUNC_ROUND(TOTAL_NET, 2%) < 0.0

	WHEN ERROR IN
		FIND #AP_CDJ.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

18010	!
	! Find vendor for this CDJ
	!
	IF FUNC_ROUND(TOTAL_NET, 2%) <> 0.0
	THEN
		!
		! Print the check if a top check
		!
		IF FRM_CHECK% < FRM_VOUCHER%
		THEN
			SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
				FRM_CHECK%, &
				FORM_TEXT$, &
				FORM_GROUPS%, &
				FORM_GROUP(), &
				0%)
		END IF

		!
		! Print the top of the stub
		!
		SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_TOPSTUB%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

18020	!
	! Get a record
	!
	WHEN ERROR IN
		GET #AP_CDJ.CH%
	USE
		CONTINUE 18090
	END WHEN

	!
	! See if at end of the group for one vendor
	!
	GOTO 18090 IF AP_CDJ::VENNUM <> AP_VENDOR::VENNUM

	GOTO 18080 IF AP_CDJ::CKNUM <> ""

	IF FUNC_ROUND(TOTAL_NET, 2%) > 0.0
	THEN
		IF TRAN_COUNT% <= FORM_GROUP(FRM_VOUCHER%)::NUMBER
		THEN
			!
			! Print the lines
			!
			SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
				FRM_VOUCHER%, &
				FORM_TEXT$, &
				FORM_GROUPS%, &
				FORM_GROUP(), &
				0%)
		END IF
	END IF

18030	IF FUNC_ROUND(TOTAL_NET, 2%) > 0.0
	THEN
		AP_CDJ::CKNUM = NUM1$(LAST_CKNUM)
	ELSE
		AP_CDJ::CKNUM = "000000"
	END IF

	AP_CDJ::CKDAT = TRM$(RIGHT(CHECK_DATE$, 7%)) + &
		LEFT(CHECK_DATE$, 2%) + &
		MID(CHECK_DATE$, 4%, 2%)

	!******************************************************************
	! Update check number
	!******************************************************************

	WHEN ERROR IN
		UPDATE #AP_CDJ.CH%
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

18080	!
	! Get next record
	!
	GOTO 18020

18090	IF FUNC_ROUND(TOTAL_NET, 2%) > 0.0
	THEN
		IF TRAN_COUNT% > FORM_GROUP(FRM_VOUCHER%)::NUMBER
		THEN
			CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, &
				SPACE$(33%) + "SEE ATTACHMENT", 0%)
			TRAN_COUNT% = 1%
		END IF

		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
			FOR LOOP% = TRAN_COUNT% + 1% TO &
				FORM_GROUP(FRM_VOUCHER%)::NUMBER
		!
		! Print the bottom of the form
		!
		SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BOTSTUB%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		!
		! Print the check if a bottom check
		!
		IF FRM_CHECK% > FRM_VOUCHER%
		THEN
			SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
				FRM_CHECK%, &
				FORM_TEXT$, &
				FORM_GROUPS%, &
				FORM_GROUP(), &
				0%)
		END IF
	END IF

	!
	! Increment check number only if the check is greater than 0.0
	!
	IF FUNC_ROUND(TOTAL_NET, 2%) > 0.0
	THEN
		LAST_CKNUM = LAST_CKNUM + 1.0
	END IF

18100	!
	! Do the next group
	!
	RETURN

 LoadForm:
	!*******************************************************************
	! Initilize check form
	!*******************************************************************

	!
	! Get form from the AP form library
	!
	SMG_STATUS% = OUTP_FORMINIT(AP_FORM.DEV$ + "AP_FORM", REPORT$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "Check form is missing", &
			"E", SCOPE::PRG_PROGRAM, REPORT$, NUM1$(SMG_STATUS%))
		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_CHECK% = 0%
	FRM_TOPSTUB% = 0%
	FRM_VOUCHER% = 0%
	FRM_BOTSTUB% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-CHECK"
			FRM_CHECK% = I%

		CASE "FRM-TOPSTUB"
			FRM_TOPSTUB% = I%

		CASE "FRM-VOUCHER"
			FRM_VOUCHER% = I%

		CASE "FRM-BOTSTUB"
			FRM_BOTSTUB% = I%

		END SELECT

	NEXT I%

	RETURN

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

	!
	! Print the check if a top check
	!
	IF FRM_CHECK% < FRM_VOUCHER%
	THEN
		SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_CHECK%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			1%)
	END IF

	!
	! Print the top of the stub
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOPSTUB%, &
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
		FOR LOOP% = 4% TO FORM_GROUP(FRM_VOUCHER%)::NUMBER

	!
	! Print the bottom of the form
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTSTUB%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	!
	! Print the check if a bottom check
	!
	IF FRM_CHECK% > FRM_VOUCHER%
	THEN
		SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_CHECK%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			1%)
	END IF

	!
	! Increment check number
	!
	LAST_CKNUM = LAST_CKNUM + 1.0

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

	MAP (CHECK_FORM) &
		TOTAL_AMT, &
		TOTAL_DISC, &
		TOTAL_DISC_LOST, &
		TOTAL_NET, &
		CHECK_DATE$, &
		LAST_CKNUM, &
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

	CASE "ADDLINE1"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(1%)

	CASE "ADDLINE2"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(2%)

	CASE "ADDLINE3"	! Substitute Vendor Address
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
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(1%)

	CASE "AP_VENDOR.POADDLINE2"	! Substitute Purchase Order Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(2%)

	CASE "AP_VENDOR.POADDLINE3"	! Substitute Purchase Order Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(3%)

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

	CASE "TOTAL_DISC_LOST"
		REALVALUE = TOTAL_DISC_LOST

	CASE "TOTAL_NET", "AMOUNT"
		REALVALUE = TOTAL_NET

	CASE "CHECK_DATE"
		TEXTVALUE$ = CHECK_DATE$

	CASE "CHECK_NUMBER"
		REALVALUE = LAST_CKNUM
		TEXTVALUE$ = NUM1$(LAST_CKNUM)

	END SELECT

	END SUB
