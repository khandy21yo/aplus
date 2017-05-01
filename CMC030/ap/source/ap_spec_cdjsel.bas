1	%TITLE "Select AP Transactions to Pay"
	%SBTTL "AP_SPEC_CDJSEL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Select AP Transactions to Pay\* option
	!	selects items
	!	for payment. The selection process can be
	!	made on the basis of due date,
	!	discount date, of invoices for individual
	!	vendors or any combination thereof.
	!	.lm -5
	!
	! Index:
	!	.x Select>Transaction to Pay
	!	.x Transaction to Pay>Select
	!
	! Option:
	!
	!	AP_SPEC_CDJSEL$CLEAR
	!	AP_SPEC_CDJSEL$CONFIRM
	!	AP_SPEC_CDJSEL$DISCOUNT
	!	AP_SPEC_CDJSEL$DUE
	!	AP_SPEC_CDJSEL$END_DATE
	!	AP_SPEC_CDJSEL$SELECT
	!	AP_SPEC_CDJSEL$START_DATE
	!	AP_SPEC_CDJSEL$VENDOR
	!	AP_SPEC_CDJSEL$VENNUM
	!	AP_SPEC_CDJSEL$WARN_CHECK
	!
	! Author:
	!
	!	10/02/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_CDJSEL/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_SPEC_CDJSEL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_CDJSEL.OBJ;*
	!
	! Modification history:
	!
	!	10/21/87 - Robert Peterson
	!		Add interrupt menu during create of work file
	!
	!	08/20/88 - Kevin Handy
	!		Re-worked so that the last item can be selected.
	!
	!	08/29/88 - Kevin Handy
	!		Fixed problem with selecting by due date.
	!		The date range was being entered in six character
	!		format, but in the CDJ file was eight character
	!		format.
	!
	!	09/13/88 - Kevin Handy
	!		Changed select by vendor to use a list instead
	!		of only showing one at a time.
	!
	!	10/13/88 - Kevin Handy
	!		Fixed bug in select by vendor where selecting
	!		the last vendor in the open file hung the program
	!		in an eternal loop.
	!
	!	03/20/89 - Kevin Handy
	!		Removed requirement that select is the only
	!		program that may be using the AP_OPEN or the
	!		AP_CONTROL file.
	!
	!	06/08/89 - Kevin Handy
	!		Fixed bug where discount was being counted twice
	!		for an invoice.  (Most people only get the discount
	!		once).
	!
	!	09/21/90 - Kevin Handy
	!		Cleaned up some of the code.  Removed unecessary
	!		statements.
	!
	!	09/26/90 - Kevin Handy
	!		Fixed bug in discounts.  Assume that discounts
	!		are/will be taken always, instead of halfway
	!		believing that they are lost on the second check.
	!
	!	01/04/93 - Kevin Handy
	!		Increased dimensions for arrays from 1000 to 2000.
	!
	!	01/04/93 - Kevin Handy
	!		Added check for array bounds being exceeded. Will
	!		now ignore too many records. (So the user will have
	!		to close)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/28/95 - Kevin Handy
	!		Format source closer to 80 columns.
	!
	!	04/28/95 - Kevin Handy
	!		Add subclasses to vendor select, to be able
	!		to select by Vendor/DueDate and Vendor/InvoiceDate.
	!
	!	05/19/95 - Kevin Handy
	!		Modified due-date sort to sort by transaction
	!		number within a given due date, and invoice number
	!		within a given invoice date.
	!
	!	06/15/95 - Kevin Handy
	!		Clean up (Check).
	!
	!	03/21/96 - Kevin Handy
	!		Modified so 'Vendor-inv' options actually sorts
	!		by invoice number instead of transaction number.
	!
	!	06/04/96 - Kevin Handy
	!		Reformat source code.
	!		Added batch number to AP_CDJ.
	!
	!	11/07/96 - Kevin Handy
	!		Force all code lines to be less than 80 columns.
	!		Modifications to add Gl-acct option, which allows
	!		the user to change the cash acct, default check
	!		number, default check date.
	!		Use "AP_CDJ.NAME$" in kill instead of just
	!		"AP_CDJ.DEV$" do batch number is included
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/20/99 - Kevin Handy
	!		Fix unsolicited input calls
	!
	!	09/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ)		AP_CDJ_CDD	AP_CDJ

	DECLARE AP_CDJ_CDD TEMP_CDJ
	DIM AP_CDJ_CDD ARRAY_CDJ(2000%)
	DIM ARRAY_CDJ_TEXT$(2000%)
	ARRAY_CDJ_MAX% = 2000%

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE SMG_SCROLL_CDD SMG_SCROLL

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL INTEGER FUNCTION DSPL_SCROLL
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	USE_CASHACCT$ = SPACE$(LEN(AP_CDJ::CASH_ACCT))
	USE_CHECK$ = SPACE$(LEN(AP_CDJ::CKNUM))
	USE_CHECKDATE$ = SPACE$(LEN(AP_CDJ::CKDAT))

	CALL READ_INITIALIZE

	ON ERROR GOTO 19000

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_CDJ", AP_CDJ.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(AP_CDJ.DEV$ + "AP_CDJ_*.JRL", JRL_FILE$(), &
		16%, "", "")

	JRL_FILE% = VAL%(JRL_FILE$(0%))

	IF JRL_FILE%
	THEN
		JRL_FILE$(LOOP%) = MID(JRL_FILE$(LOOP%), 8%, 2%) &
			FOR LOOP% = 1% TO JRL_FILE%

		X% = ENTR_3CHOICE(SCOPE, "", "", JRL_FILE$(), "", &
			0%, "CD Batch Files", "", 0%)

		IF X% > 0%
		THEN
			CDJ_BATCH$ = EDIT$(JRL_FILE$(X%), -1%)
			GOTO 309
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for batch number
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Please enter a Batch number<01> ?  01", 6%, 20%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

302	!
	! Get the journal name
	!
	CDJ_BATCH$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		6%, 55%, CDJ_BATCH$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 302
	END SELECT

	CDJ_BATCH$ = "01" IF EDIT$(CDJ_BATCH$, -1%) = ""

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	IF LEN(EDIT$(CDJ_BATCH$, -1%)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter the batch # in " + &
			"(XX) format", 0%)
		GOTO 302
	END IF

309	!
	! Set up window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, SMG_SCREEN_DATA%)

	!
	! Set up title string
	!
	TOP_TITLE$ = " Cash Disbursements Select "
	TEMP1$ = EDIT$(SCOPE::PRG_COMPANY, 4% + 8% + 128%)
	TOP_TITLE$ = TOP_TITLE$ + "for " + TEMP1$ + " " &
		IF TEMP1$ <> ""

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		LEFT(TOP_TITLE$, 78%))

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

310	!
	! Open AP_OPEN file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AP_CONTROL file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
		GET #AP_CONTROL.CH%, REGARDLESS
		CLOSE AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF AP_CONTROL::CASH_ACCT = ""
	THEN
		CALL HELP_3MESSAGE(SCOPE, "CD Select Process", &
			"ERR", "AP_ACCT", "CASH_ACCT_MISS")
		GOTO ExitProgram
	END IF

	LSET USE_CASHACCT$ = AP_CONTROL::CASH_ACCT

330	!
	! Open Vendor file
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"

340	!
	! Open AP_CDJ file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.CRE"
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

500	WHEN ERROR IN
		RESET #AP_OPEN.CH%
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Set totals to zero
	!
	TOTAL_AMT, TOTAL_SEL% = 0.0

 Menu:
1000	!****************************************************************
	! Enter desired option
	!****************************************************************

	GOSUB DisplayGlAccount

	SCOPE::PRG_ITEM = ""
	OPLIST$ = "Due dIscount Vendor-inv vendor-dUe vendor-daTe " + &
		"Clear Gl-acct Help eXit"

	!
	! Input the option
	!
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		SCOPE::PRG_ITEM = "EXIT"
		GOTO ExitProgram

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Menu

	END SELECT

	!
	! Decide what to do with the option
	!
	SELECT OPT$

	!
	! Clear Selected Records
	!
	CASE "C"
	!++
	! Abstract:CLEAR
	!	^*Clear all selections\*
	!	.b
	!	.lm +5
	!	^*Clear all Selections\* clears all selections for
	!	payment.
	!	.lm -5
	!
	! Index:
	!	.x Clear
	!
	!--
3000		WHEN ERROR IN
			RESET #AP_CDJ.CH%
		USE
			CONTINUE 1000
		END WHEN

		TEMP_ITEM$ = SCOPE::PRG_ITEM

		SCOPE::PRG_ITEM = "WARN_REMOVE"
	!++
	! Abstract:WARN_REMOVE
	!
	! Index:
	!
	!--

		TEMP$ = "Warning All data in CD file will " + &
			"be removed"

3050		WHEN ERROR IN
			GET #AP_CDJ.CH%, KEY #0% GE "0"
		USE
			IF ERR = 154%	! Locked record
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 3100
		END WHEN

		IF AP_CDJ::CKNUM <> ""
		THEN
			SCOPE::PRG_ITEM = "WARN_CHECK"

	!++
	! Abstract:WARN_CHECK
	!	^*Clear - Clear Cash Disbursements\*
	!	.b
	!	.lm +5
	!	Check the numbers which have been
	!	assigned in the cash disbursements
	!	file.  The user must be certain that there are not any checks
	!	that have not been posted in the cash disbursements file
	!	before clearing the file.
	!	.lm -5
	!
	! Index:
	!
	!--

			TEMP$ = "Warning check numbers have already been " + &
				"assigned"
		END IF

3100		CALL ENTR_3MESSAGE(SCOPE, TEMP$, 1%)

		INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_MESSAGE, "1;1", &
			"Confirm process - Then press <Do>", "N", 64%, "'", "")

3200		SCOPE::PRG_ITEM = TEMP_ITEM$

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

		IF EDIT$(INP$, -1%) = "Y"
		THEN
			CLOSE AP_CDJ.CH%

 !			KILL AP_CDJ.NAME$ WHILE(-1%)
			SMG_STATUS% = LIB$DELETE_FILE(AP_CDJ.NAME$ + ";*")

		END IF

	!
	! Due
	!
	CASE "D"
	!++
	! Abstract:DUE
	!	^*Select by due date\*
	!	.b
	!	.lm +5
	!	The ^*Select by due date\* selects the items to
	!	be paid by their due dates. The dates are searched to find those within a
	!	specified time period.
	!	.lm -5
	!
	! Index:
	!	.x Select by due date
	!
	!--
4000		WINDOW_ID$ = "DUE DATE"

		GOSUB StartEndDate

		GOTO Menu IF ABORT_FLAG%

		RESET #AP_OPEN.CH%
		GET #AP_OPEN.CH%, REGARDLESS

		END_RECORD% = 0%

		!
		! Erase option and message window
		!
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		!
		! Set up to trap interrupt
		!
		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

		RRR_FLAG% = 0%

4500		!
		! Handle any special junk in RRR_FLAG%
		!
		SELECT RRR_FLAG%

		!
		! Repaint screen
		!
		CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
			SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		!
		! Help
		!
		CASE SMG$K_TRM_HELP
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
			CALL HELP_34MESSAGE(SCOPE, "", &
				SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, &
				"", SCOPE::PRG_ITEM)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		!
		! Interupt
		!
		CASE SMG$K_TRM_F6, SMG$K_TRM_F20
			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT( &
				SCOPE::SMG_PBID)

			CALL MENU_3INTERRUPT(SCOPE)

			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT( &
				SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

		!
		! Exit keys
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
			GOTO 4540

		END SELECT

		RRR_FLAG% = 0%

		GOSUB CalBal

4530		IF AP_CDJ::DUEDAT >= START$ AND AP_CDJ::DUEDAT <= ENDING$
		THEN
			IF FUNC_ROUND(AP_CDJ::CKAMT, 2%) <> 0.0
			THEN
				GOSUB PutCDJRecord
			END IF
		END IF

		GOTO 4500 IF END_RECORD% = 0%

4540		!
		! Disable unsolicited input
		!
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!
	! Discount
	!
	CASE "I"
	!++
	! Abstract:DISCOUNT
	!	^*Select by discount date\*
	!	.b
	!	.lm +5
	!	The ^*Select by discount date\* selects the items to
	!	be paid by their discount dates. The dates are searched to find those within a
	!	specified time period.
	!	.lm -5
	!
	! Index:
	!	.x Select by discount date
	!
	!--
5000		WINDOW_ID$ = "DISCOUNT DATE"

		GOSUB StartEndDate

		GOTO Menu IF ABORT_FLAG%

		RESET #AP_OPEN.CH%
		GET #AP_OPEN.CH%, REGARDLESS

		END_RECORD% = 0%

		!
		! Erase option and message window
		!
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		!
		! Set up to trap interrupt
		!
		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

		RRR_FLAG% = 0%

5500		!
		! Handle any special junk in RRR_FLAG%
		!
		SELECT RRR_FLAG%

		!
		! Repaint screen
		!
		CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
			SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		!
		! Help
		!
		CASE SMG$K_TRM_HELP
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
			CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
				SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		!
		! Interupt
		!
		CASE SMG$K_TRM_F6, SMG$K_TRM_F20
			SMG_STATUS% = &
				SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			CALL MENU_3INTERRUPT(SCOPE)

			SMG_STATUS% = &
				SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

		!
		! Exit keys
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
			GOTO 5540

		END SELECT

		RRR_FLAG% = 0%

		GOSUB CalBal

5530		IF AP_CDJ::DISCDAT >= START$ AND AP_CDJ::DISCDAT <= ENDING$
		THEN
			IF FUNC_ROUND(AP_CDJ::CKAMT, 2%) <> 0.0
			THEN
				GOSUB PutCDJRecord
			END IF
		END IF

		GOTO 5500 IF END_RECORD% = 0%

5540		!
		! Disable unsolicited input
		!
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!
	! Vendor/Invoice
	!
	!++
	! Abstract:SELECT
	!	^*Select\*
	!	.b
	!	.lm +5
	!	The ^*Select\* function
	!	selects for payment the
	!	specific vendor's invoice at which the arrow is pointing.
	!	.b
	!	Move the arrow with the ^*up arrow key\* or ^*down arrow key\* to an invoice
	!	which is to be selected for payment.  Press ^*S\* for ^*Select\*, or highlight
	!	the ^*Select\* function with the use of the ^*right arrow key\* or ^*left arrow
	!	key\* and press ^*<Do>\*.
	!	.lm -5
	!
	! Index:
	!
	!--
	CASE "V"
		SELECT_BY$ = "Vendor/Invoice"
		SELECT_BY% = 1%
		GOSUB SelectByVendor

	!
	! Vendor/DueDate
	!
	CASE "U"
		SELECT_BY$ = "Vendor/Due Date"
		SELECT_BY% = 2%
		GOSUB SelectByVendor

	!
	! Vendor/Invoice Date
	!
	CASE "T"
		SELECT_BY$ = "Vendor/Invoice Date"
		SELECT_BY% = 3%
		GOSUB SelectByVendor

	!
	! Gl-Account
	!
	CASE "G"
		GOSUB EnterGlAccount

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE,  "", "PROG", SCOPE::PRG_PROGRAM, &
			"HELP")
		GOTO Menu

	!
	! eXit
	!
	! This option exits out of the program.
	!
	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO Menu

	%PAGE

	!*******************************************************************
	! Select by vendor
	!*******************************************************************

 SelectByVendor:
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Select by - " + &
		SELECT_BY$, 4%, 25%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Total amount selected:  ", &
		6%, 25%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Number of items selected:  ", &
		8%, 25%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(TOTAL_AMT, "##,###,###.##"), &
		6%, 52%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(TOTAL_SEL%, "        #####"), &
		8%, 52%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"  Vendor #   Tran # Invoice #       Inv Date Due Date" + &
		" PO #         Amount Due", 11%, 1%)

6000	!
	! Ask for vendor #
	!
	SCOPE::PRG_ITEM = "FLD03VENNUM"

	!++
	! Abstract:FLD03VENDOR
	!	^*VENDOR - Select by vendor number\*
	!	.B
	!	.LM +5
	!	The ^*Select by vendor number\*
	!	selects cash disbursements by the vendor number and then choose from all the
	!	vendor's transactions the desired ones for payment.
	!	.LM -5
	!
	! Index:
	!
	!--
	!++
	! Abstract:FLD03VENNUM
	!	^*Vendor Number\*
	!	.B
	!	.LM +5
	!	The ^*Vendor Number\* field contains the reference number or key used to
	!	identify a particular vendor.  When a valid ^*Vendor Number\* is entered,
	!	the name and address of that vendor will be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Vendor Number
	!
	!--
	!++
	! Abstract:VENNUM
	!	^* Vendor _#\*
	!	.b
	!	.lm +5
	!	The ^*Vendor _#\* field contains the reference number or key used to identify
	!	a particular vendor. When a valid ^*Vendor _#\* is entered, the
	!	name and address of that vendor will be displayed.
	!	.b
	!	If the ^*Vendor _#\* entered does not exist in the Vendor Master
	!	file, the message, ^*"Undefined Vendor number - ADD to VENDOR file
	!	(Y/N) ? <Yes/No>: No"\*, will appear at the bottom of the screen. The
	!	default response is ^*No\*. Pressing ^*<Ent>\* or ^*<Ret>\* will cause
	!	the system to permit a re-try to enter the correct vendor number. A
	!	^*Yes\* response to the message will cause the system to access the
	!	Vendor Master File where a record can be added for the new vendor.
	!	After completing the Add function for the new vendor, press the
	!	^*<Exit>\* key to return to the purchases Journal routine.
	!	.b
	!	Function keys which can be used are:
	!	.lm 15
	!	.b
	!	^*<List Choices>\* will list the vendor master file and allow
	!	the user to select a vendor.
	!	.b
	!	^*<F17>\* will allow editing of the vendor whose number
	!	is displayed on the screen.
	!	.lm -5
	!
	! Index:
	!	.x Vendor Number
	!
	!--
	!++
	! Abstract:VENDOR
	!	^*Select by vendor number\*
	!	.b
	!	.lm +5
	!	The ^*Select by vendor number\*
	!	selects cash disbursements by the vendor number and then choose from all the
	!	vendor's transactions the desired ones for payment.
	!	.lm -5
	!
	! Index:
	!	.x Vendor>Select by
	!	.x Select by Vendor
	!
	!--

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		SPACE$(LEN(AP_VENDOR::VENNAM)), 10%, 12%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		SPACE$(78%), &
		15%, 1%)

	VENNUM$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"10;1", "Vendor #", &
		"          ", 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO EndVendorSelect

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 6000

	END SELECT

	GOTO EndVendorSelect IF VENNUM$ = ""

	WHEN ERROR IN
		GET #AP_OPEN.CH%, KEY #0% EQ VENNUM$, REGARDLESS
	USE
		CONTINUE 6100 IF ERR = 155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	END_RECORD% = 0%

	GOTO 6400

6100	CALL ENTR_3MESSAGE(SCOPE, "Unable to find " + VENNUM$, 0%)

	GOTO 6000

6400	!
	! Find vendor for this CDJ
	!
	AP_VENDOR::VENNAM = STRING$(40%, A"?"B)

	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ VENNUM$, REGARDLESS
	USE
		CONTINUE 6450 IF ERR = 155%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

6450	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		AP_VENDOR::VENNAM, 10%, 12%)

	ARRAY_CDJ% = 0%

6500	!
	! Here if the main loop to load in all of the
	! vendors possible payment records.
	!
	GOSUB CalBal

	GOTO LastOfVendor IF (VENNUM$ <> AP_CDJ::VENNUM)

	GOTO 6525 IF FUNC_ROUND(AP_CDJ::CKAMT, 2%) = 0.0

	!
	! Skip if already have a payment in cdj for this item
	!
	WHEN ERROR IN
		FIND #AP_CDJ.CH%, KEY #0% EQ AP_CDJ::VENNUM + &
			AP_CDJ::TRANKEY, REGARDLESS
	USE
		CONTINUE 6520 IF ERR = 155%
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	GOTO 6525

6520	!
	! Only store as many as the array can hold.
	!
	IF (ARRAY_CDJ% < ARRAY_CDJ_MAX%)
	THEN
		ARRAY_CDJ% = ARRAY_CDJ% + 1%
		ARRAY_CDJ(ARRAY_CDJ%) = AP_CDJ
	END IF

6525	GOTO LastOfVendor IF END_RECORD%

	GOTO 6500

 LastOfVendor:
6530	!
	! See if we even have anything
	!
	IF ARRAY_CDJ% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Nothing found to pay!", 0%)
		GOTO EndVendorSelect
	END IF

	!
	! Sort by specified method
	!
	SELECT SELECT_BY%

	!
	! Invoice number
	!
	CASE 1%
		FOR I% = 1% TO ARRAY_CDJ%

			FOR J% = 1% TO ARRAY_CDJ% - I%

				IF ARRAY_CDJ(J%)::INVNUM > &
					ARRAY_CDJ(J% + 1%)::INVNUM
				THEN
					TEMP_CDJ = ARRAY_CDJ(J%)
					ARRAY_CDJ(J%) = ARRAY_CDJ(J% + 1%)
					ARRAY_CDJ(J% + 1%) = TEMP_CDJ
				END IF

			NEXT J%
		NEXT I%


	!
	! Due Date
	!
	CASE 2%
		FOR I% = 1% TO ARRAY_CDJ%

			FOR J% = 1% TO ARRAY_CDJ% - I%

				IF ARRAY_CDJ(J%)::DUEDAT + &
					ARRAY_CDJ(J%)::TRANKEY > &
					ARRAY_CDJ(J% + 1%)::DUEDAT + &
					ARRAY_CDJ(J% + 1%)::TRANKEY
				THEN
					TEMP_CDJ = ARRAY_CDJ(J%)
					ARRAY_CDJ(J%) = ARRAY_CDJ(J% + 1%)
					ARRAY_CDJ(J% + 1%) = TEMP_CDJ
				END IF

			NEXT J%
		NEXT I%

	!
	! Sort by Invoice date
	!
	CASE 3%

		FOR I% = 1% TO ARRAY_CDJ%

			FOR J% = 1% TO ARRAY_CDJ% - I%

				IF ARRAY_CDJ(J%)::INVDAT + &
					ARRAY_CDJ(J%)::INVNUM > &
					ARRAY_CDJ(J% + 1%)::INVDAT + &
					ARRAY_CDJ(J% + 1%)::INVNUM
				THEN
					TEMP_CDJ = ARRAY_CDJ(J%)
					ARRAY_CDJ(J%) = ARRAY_CDJ(J% + 1%)
					ARRAY_CDJ(J% + 1%) = TEMP_CDJ
				END IF

			NEXT J%
		NEXT I%

	END SELECT

	!
	! Create displayable text version for each invoice
	!
	FOR LOOP% = 1% TO ARRAY_CDJ%

		ARRAY_CDJ_TEXT$(LOOP%) = &
			ARRAY_CDJ(LOOP%)::VENNUM + " " + &
			ARRAY_CDJ(LOOP%)::TRANKEY + " " + &
			ARRAY_CDJ(LOOP%)::INVNUM + " " + &
			PRNT_DATE(ARRAY_CDJ(LOOP%)::INVDAT, 1%) + " " + &
			PRNT_DATE(ARRAY_CDJ(LOOP%)::DUEDAT, 1%) + " " + &
			ARRAY_CDJ(LOOP%)::PONUM + " " + &
			FORMAT$(ARRAY_CDJ(LOOP%)::CKAMT, "##,###,###.##")

	NEXT LOOP%

	!
	! Define scrolling region
	!
	SMG_SCROLL::WINDOW	= SMG_SCREEN_DATA%
	SMG_SCROLL::TOP_ARRAY	= 1%
	SMG_SCROLL::SCROLL_TOP	= 12%
	SMG_SCROLL::SCROLL_BOT	= 18%
	SMG_SCROLL::BEG_ELEMENT	= 1%
	SMG_SCROLL::BOT_ARRAY	= ARRAY_CDJ%
	SMG_SCROLL::END_ELEMENT	= ARRAY_CDJ%
	SMG_SCROLL::TOP_LINE	= 1%
	SMG_SCROLL::CUR_LINE	= 1%
	SMG_SCROLL::CUR_W_ROW	= 1%
	SMG_SCROLL::CUR_W_COL	= 1%
	SMG_SCROLL::FIND_LINE	= 1%
	SMG_SCROLL::SMG_FLAG	= 0%
	SMG_SCROLL::PROMPT	= "->"
	SMG_SCROLL::VIDEO_SET	= SMG$M_REVERSE
	SMG_SCROLL::VIDEO_COMP	= 0%
	SMG_SCROLL::CHARSET	= 0%
	SMG_SCROLL::DRAW_COLS	= ""

	V% = DSPL_SCROLL(SMG_SCROLL, ARRAY_CDJ_TEXT$(), 0%, "PAINT")

6550	!
	! Make sure we still have something to select
	!
	IF ARRAY_CDJ% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Nothing more found to pay!", 0%)
		GOTO EndVendorSelect
	END IF


	!
	! Enter option
	!
	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Select Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT2%, 0%)

	SELECT SCOPE::SCOPE_EXIT

		!
		! ^C
		!
		CASE SMG$K_TRM_CTRLC
			GOTO EndVendorSelect

		!
		! Next Screen, Downarrow, etc.
		!
		CASE SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_DOWN, SMG$K_TRM_UP, &
			SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_F18, &
			SMG$K_TRM_F19

			V% = DSPL_SCROLL(SMG_SCROLL, ARRAY_CDJ_TEXT$(), &
				SCOPE::SCOPE_EXIT, "PAINT")
			GOTO 6550

		!
		! Exit
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
			GOTO EndVendorSelect


		!
		! Select Key
		!
		CASE SMG$K_TRM_SELECT
			GOSUB SelectedVendor
			GOTO 6550

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 6550

	END SELECT

	SELECT OPT$

	CASE "X"
		GOTO EndVendorSelect

	CASE "S"
		GOSUB SelectedVendor

	END SELECT

	GOTO 6550

 EndVendorSelect:

	RETURN

6600	!*******************************************************************
	! Handle a selected item
	!*******************************************************************
 SelectedVendor:

	AP_CDJ = ARRAY_CDJ(SMG_SCROLL::CUR_LINE)
	PUT #AP_CDJ.CH%

	FOR LOOP% = SMG_SCROLL::CUR_LINE TO ARRAY_CDJ% - 1%

		ARRAY_CDJ(LOOP%) = ARRAY_CDJ(LOOP% + 1%)
		ARRAY_CDJ_TEXT$(LOOP%) = ARRAY_CDJ_TEXT$(LOOP% + 1%)

	NEXT LOOP%

	SMG_SCROLL::BOT_ARRAY, SMG_SCROLL::END_ELEMENT, &
		ARRAY_CDJ% = ARRAY_CDJ% - 1%
	SMG_SCROLL::CUR_LINE = ARRAY_CDJ% IF SMG_SCROLL::CUR_LINE > ARRAY_CDJ%

	TOTAL_AMT = TOTAL_AMT + AP_CDJ::CKAMT
	TOTAL_SEL% = TOTAL_SEL% + 1%

	SMG_STATUS% = SMG$PUT_CHARS( &
		SMG_SCREEN_DATA%, FORMAT$(TOTAL_AMT, "##,###,###.##"), &
		6%, 52%)
	SMG_STATUS% = SMG$PUT_CHARS( &
		SMG_SCREEN_DATA%, FORMAT$(TOTAL_SEL%, "        #####"), &
		8%, 52%)

6690	V% = DSPL_SCROLL(SMG_SCROLL, ARRAY_CDJ_TEXT$(), 0%, "PAINT")

	RETURN

	%PAGE

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 CalBal:
18000	!******************************************************************
	! Calculate balance due by trankey
	!******************************************************************

	!
	! Assume we have the first record here right now, and make the
	! best of it.
	!
	TEST_VENNUM$ = AP_OPEN::VENNUM + ""
	TRANKEY$ = AP_OPEN::TRANKEY + ""

	AP_CDJ::VENNUM		= AP_OPEN::VENNUM
	AP_CDJ::TRANKEY		= AP_OPEN::TRANKEY
	AP_CDJ::INVNUM		= AP_OPEN::INVNUM
	AP_CDJ::INVDAT		= AP_OPEN::INVDAT
	AP_CDJ::INVAMT		= AP_OPEN::INVAMT
	AP_CDJ::DISAMT		= 0.0
	AP_CDJ::CKDESC		= AP_OPEN::CKDESC
	AP_CDJ::AP_ACCT		= AP_OPEN::AP_ACCT
	AP_CDJ::PONUM		= AP_OPEN::PONUM
	AP_CDJ::DUEDAT		= AP_OPEN::DUEDAT
	AP_CDJ::DISCDAT		= AP_OPEN::DISCDAT
	AP_CDJ::DISC_LOST_AMT	= 0.0
	AP_CDJ::DISCLOST_ACCT	= AP_CONTROL::DISCLOST_ACCT
	AP_CDJ::CKNUM		= USE_CHECK$
	AP_CDJ::CKDAT		= USE_CHECKDATE$
	AP_CDJ::CKAMT		= 0.0
	AP_CDJ::CASH_ACCT	= USE_CASHACCT$

	BAL_DUE  = AP_OPEN::INVAMT - AP_OPEN::CKAMT
	CK_AMT = AP_OPEN::CKAMT
	DIS_AMT = AP_OPEN::DISAMT

18030	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		IF ERR = 11%
		THEN
			END_RECORD% = -1%
			CONTINUE 18100
		END IF
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

18040	GOTO 18100 &
		IF (TEST_VENNUM$ <> AP_OPEN::VENNUM) OR &
		(AP_OPEN::TRANKEY <> TRANKEY$)

	BAL_DUE = FUNC_ROUND(BAL_DUE + AP_OPEN::INVAMT - AP_OPEN::CKAMT, 2%)
	CK_AMT = FUNC_ROUND(CK_AMT + AP_OPEN::CKAMT, 2%)
	DIS_AMT = FUNC_ROUND(DIS_AMT + AP_OPEN::DISAMT, 2%)

	GOTO 18030

18100	AP_CDJ::CKAMT = BAL_DUE

	!
	! If the an amount has already been paid then the discount
	! amount taken will be set to zero.  Otherwise set the discount
	! amount equal to the discount and subract it from the
	! the bal_due.  Assume thaat if previous checks were written,
	! that the discount was taken.
	!
	IF CK_AMT <> 0.0
	THEN
		AP_CDJ::DISAMT = 0.0
	ELSE
		AP_CDJ::DISAMT = DIS_AMT
	END IF

	AP_CDJ::CKAMT = FUNC_ROUND(BAL_DUE - DIS_AMT, 2%)

	RETURN

 PutCDJRecord:
18200	!******************************************************************
	! Put CDJ record
	! Check for duplicate trankey in CDJ
	! Increment total selected
	!******************************************************************
	WHEN ERROR IN
		FIND #AP_CDJ.CH%, &
			KEY #0% EQ AP_CDJ::VENNUM + AP_CDJ::TRANKEY, &
			REGARDLESS
	USE
		CONTINUE 18210 IF ERR = 155%
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	GOTO 18290

18210	WHEN ERROR IN
		PUT #AP_CDJ.CH%
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	TOTAL_SEL% = TOTAL_SEL% + 1%
	TOTAL_AMT = FUNC_ROUND(TOTAL_AMT + AP_CDJ::CKAMT, 2%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(TOTAL_AMT, "##,###,###.##"), &
		6%, 52%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(TOTAL_SEL%, "        #####"), &
		8%, 52%)

18290	RETURN

 StartEndDate:
	!****************************************************************
	! Ask for start and end dates
	!****************************************************************
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Select by - " + &
		WINDOW_ID$, 4%, 25%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Total amount selected:  ", &
		6%, 25%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Number of items selected:  ", &
		8%, 25%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(TOTAL_AMT, "##,###,###.##"), &
		6%, 52%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(TOTAL_SEL%, "        #####"), &
		8%, 52%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Starting date ", 14%, 10%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Ending date ", 14%, 40%)

	START$, ENDING$ = ""
	ABORT_FLAG% = 0%

 StartDate:
	SCOPE::PRG_ITEM = "FLD04START_DATE"
	!++
	! Abstract:FLD04START_DATE
	!	^*Start Date\*
	!	.b
	!	.lm +5
	!	The ^*Start Date\* refers to the first date that will be considered in searching
	!	for transactions to pay by due dates.
	!	.lm -5
	!
	! Index:
	!	.x Start Date
	!
	!--
	!++
	! Abstract:START_DATE
	!	^*Start Date\*
	!	.b
	!	.lm +5
	!	The ^*Start Date\* refers to the first date that will be considered in searching
	!	for transactions to pay by due dates.
	!	.lm -5
	!
	! Index:
	!	.x Start Date
	!
	!--

	START$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, &
		"14;24", "Starting date", &
		"00010101", 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		ABORT_FLAG% = -1%
		RETURN

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO StartDate

	END SELECT

 EndDate:
	SCOPE::PRG_ITEM = "FLD05END_DATE"
	!++
	! Abstract:FLD05END_DATE
	!	^*End Date\*
	!	.b
	!	.lm +5
	!	The ^*End Date\* refers to the last date that will be considered in searching
	!	for transactions to pay by due dates.
	!	.lm -5
	!
	! Index:
	!	.x End Date>Select Transactions to Pay
	!	.x Select Transactions to Pay>End Date
	!
	!--
	!++
	! Abstract:END_DATE
	!	^*End Date\*
	!	.b
	!	.lm +5
	!	The ^*End Date\* refers to the last date that will be considered in searching
	!	for transactions to pay by due dates.
	!	.lm -5
	!
	! Index:
	!	.x End Date
	!
	!--

	ENDING$ = ENTR_3DATE(SCOPE, SMG_SCREEN_DATA%, "14;52", "Ending date", &
		"99991231", 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		ABORT_FLAG% = -1%
		RETURN

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EndDate

	END SELECT

 Confirm:
	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!	^*Confirm\*
	!	.b
	!	.lm +5
	!	^*Confirm\* asks for user confirmation to the dates for the search.
	!	The user may confirm or abandon the dates.
	!	.lm -5
	!
	! Index:
	!	.x Confirm
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE,  SCOPE::SMG_MESSAGE, "1;1", &
		"Confirm dates - Then press <Do>", "N", 64%, "'", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		ABORT_FLAG% = -1%
		RETURN

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Confirm

	END SELECT

	GOTO StartDate IF INP$ <> "Y"

	RETURN

	%Page

	!*******************************************************************
	! Display the GL AAccount information across the top of the screen
	!*******************************************************************
 DisplayGlAccount:

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Cash Acct:                    Check#:                 Check Date:", &
		2%, 1%)

	GLFlag% = 1%
	GOSUB DoGlAccount FOR GLITEM% = 1% TO 3%

	RETURN

 EnterGlAccount:

	GLFLAG% = 0%
	GLITEM% = 1%

 EnterGlAccountOne:
	GOSUB DoGlAccount

	GLITEM% = GLITEM% + 1%
	GOTO EnterGlAccountOne IF GLITEM% <= 3%

	RETURN

 DoGlAccount:

	SELECT GLITEM%
	CASE 1%
		USE_CASHACCT$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"2;12", "Cash Account", &
			USE_CASHACCT$, GLFLAG%, "'E", "")

	CASE 2%
		USE_CHECK$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"2;39", "Default Check Number", &
			USE_CHECK$, GLFLAG%, "'E", "")

	CASE 3%
		USE_CHECKDATE$ = ENTR_3DATE(SCOPE, SMG_SCREEN_DATA%, &
			"2;67", "Default Check Date", &
			USE_CHECKDATE$, GLFLAG%, "'E", "")
	END SELECT

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
