1	%TITLE "Maintain Sales Journal"
	%SBTTL "AR_JOUR_SJ01"
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
	!	.B
	!	.LM +5
	!	The ^*Maintain Sales Journal\* option
	!	maintains regular sales and service charge journals.
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!	AR_MAIN_SJ01$HELP
	!	AR_MAIN_SJ01_LINE$HELP
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_JOUR_SJ01/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_JOUR_SJ01,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_JOUR_SJ01.OBJ;*
	!
	! Author:
	!
	!	02/26/93 - Kevin Handy
	!		Taken from AR_JOUR_SJ
	!
	! Modification history:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 source format.
	!		Change last param entr_3choice from "" to 0%
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	!
	! Common statements
	!
	COM (CH_AR_CONTROL) &
		AR_CONTROL.CH%

	COM (TT_AR_SJ) &
		BATCH_NO$ = 2%

	!
	! Dimension statements
	!
	DIM AR_SJH_FILE$(100%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

200	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpFile
	END WHEN

300	!******************************************************************
	! Get batch number
	!******************************************************************

	!
	! Look up device
	!
	CALL READ_DEVICE("AR_SJH", AR_SJH.DEV$, STAT%)

	!
	! Find out what is already there
	!
	CALL FIND_FILE(AR_SJH.DEV$ + "AR_SJH_*.JRL", AR_SJH_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	!
	! If any files are in the list, then do a query screen
	!
	AR_SJH_FILE% = VAL%(AR_SJH_FILE$(0%))

	IF AR_SJH_FILE%
	THEN
		!
		! Get ONLY the batch number of the file
		!
		FOR LOOP% = 1% TO AR_SJH_FILE%
			TEMP$ = RIGHT(AR_SJH_FILE$(LOOP%), 8%)
			I% = INSTR(1%, TEMP$, ".")
			I% = LEN(TEMP$) + 1% IF I% = 0%
			AR_SJH_FILE$(LOOP%) = LEFT(TEMP$, I% - 1%)
		NEXT LOOP%

		!
		! Query the user
		!
		X% = ENTR_3CHOICE(SCOPE, "", "", AR_SJH_FILE$(), "", &
			0%, "AR Sales Journal Files", "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(AR_SJH_FILE$(X%), -1%)
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit key ?
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram
	END SELECT

	!
	! Ask user for period
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

310	!
	! Print background
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"AR Sales Journal Maintenance", 10%, 24%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Batch:   " + BATCH_NO$, &
		12%, 31%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

320	!
	! Query user for batch number
	!
	BATCH_NO$ = ""
	SCOPE::PRG_ITEM = "FLD01BATCH"
	!++
	! Abstract:FLD01BATCH
	!
	! Index:
	!
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		12%, 39%, BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE SMG$K_TRM_DOWN
		GOTO 320

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	IF LEN(TRM$(BATCH_NO$)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the BATCH number in XX format", 0%)
		GOTO 320
	END IF

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)


	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AR_MAIN_SJ01.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

 HelpFile:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	RESUME ExitProgram

 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	RESUME ExitProgram

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"

	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION AR_MAIN_SJ01
	EXTERNAL LONG FUNCTION AR_MAIN_SJ01_LINE
	EXTERNAL LONG FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION AR_MAIN_CONTACT
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID
		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_SJ01.ID
		MAINT_GROUP = AR_MAIN_SJ01(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_SJ01_LINE.ID
		MAINT_GROUP = AR_MAIN_SJ01_LINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CUSTYPE.ID
		MAINT_GROUP = AR_MAIN_CUSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_CATEGORY.ID
		MAINT_GROUP = OE_MAIN_CATEGORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CONTACT.ID
		MAINT_GROUP = AR_MAIN_CONTACT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SALESTAX.ID
		MAINT_GROUP = OE_MAIN_SALESTAX(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID
		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_COUNTRY.ID
		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_TERMS.ID
		MAINT_GROUP = UT_MAIN_TERMS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_CARRIER.ID
		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SA_MAIN_SALESMAN.ID
		MAINT_GROUP = SA_MAIN_SALESMAN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:BATCH
	!	^*BATCH\*
	!	.lm +5
	!	.b
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD001
	!	.x Sales Journal>Invoice Number
	!	^*(01) Invoice\*
	!	.b
	!	.lm +5
	!	The ^*Invoice\* field enters the number or
	!	reference for a particular document.
	!	.b
	!	The field will accept up to six (06) characters.
	!	.lm -5
	!
	! Index:
	!	.x Invoice>Number in Sales Journal
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD002
	!	^*(02) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* field is provided to enter a customer code which
	!	should have been entered in the Master Name Address file for
	!	a particular customer.
	!	.b
	!	If the code entered is valid, the customer's name and
	!	address information will appear.
	!	.b
	!	Pressing ^*List Choices\* will cause a list of valid customer
	!	codes to be displayed.
	!	.b
	!	Customer codes may be added by pressing the ^*F17\* key.  After the
	!	appropriate master record has been entered, press ^*Exit\* and
	!	the screen will return to the
	!	Sales Journal maintenance routine.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Customer Mumber
	!	.x Customer>Customer Number in Sales Journal
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD003
	!	^*(03) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a brief
	!	description in reference to the transaction being entered.
	!	.b
	!	The field will accommodate up to twenty-six (26) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Description
	!	.x Description>Sales Journal
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD004
	!	.x Transaction Date>Sales Journal
	!	^*(04) Transaction Date\*
	!	.b
	!	.lm +5
	!	The ^*Trans[action] Date\* field is provided to enter the
	!	date for a particular transaction.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	This field requires an entry and will not default to the
	!	current date.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Transaction Date
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD005
	!	.x Accounts Receivable Account>Sales Journal
	!	^*(05) Accounts Receivable/Cash\*
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable/Cash\* field is provided to enter the appropriate
	!	"Accounts Receivable" or "Cash" account number established in the General Ledger
	!	Chart of Accounts.  The selection is dependent upon whether a sale is
	!	a charge sale or a cash sale to be recorded as a transaction in
	!	a customer's file.
	!	.b
	!	The field will accommodate up to twenty (20) characters.
	!	.b
	!	Pressing ^*List Choices\* will cause a list of General Ledger
	!	Chart of Account numbers to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Cash Account>Sales Journal
	!	.x Sales Journal>Accounts Receivable Account
	!	.x Sales Journal>Cash Account
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD006
	!	.x Account Receivable>Transaction Type
	!	^*(06) Accounts Receivable Type\*
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable Type\* field is used to define the type of
	!	transaction represented by a record.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*01\* - Sale on Account
	!	.te
	!	^*02\* - Cash Sale
	!	.te
	!	^*03\* - Debit Memo
	!	.te
	!	^*04\* - Service Charge
	!	.te
	!	^*08\* - Credit Memo
	!	.end table
	!	Pressing ^*List Choices\* will cause a list of valid types
	!	to be displayed.
	!	.b
	!	^*Note:\* When adding a record, the next three fields
	!	(Receipt, Check, and Deposit) will be bypassed
	!	unless the value in field (06) is 02 (Cash) or 08
	!	(Credit memo).
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Account Receviabe Transaction Type
	!	.x Transaction Type>Sales>Sale on Account
	!	.x Sale on Account>Transaction Type
	!	.x Transaction Type>Sales>Cash Sale
	!	.x Cash Sale>Transaction Type
	!	.x Transaction Type>Debit Memo
	!	.x Debit Memo>Transation Type
	!	.x Transaction Type>Service Charge
	!	.x Service Charge>Transaction Type
	!	.x Transaction Type>Credit Memo
	!	.x Credit Memo>Transaction Type
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD007
	!	^*(07) Receipt _#\*
	!	.b
	!	.lm +5
	!	The ^*Receipt _#\* field is provided to enter the number of a
	!	receipt if issued for a cash sale.  When cash sales are accounted for
	!	using cash register closing totals, a receipt number would not
	!	necessarily be applicable. This field may be bypassed.
	!	.b
	!	The field will accommodate an entry of up to eight (8) alphanumeric
	!	characters.
	!	.b
	!	^*Note:\* During an add function, this field is bypassed unless the
	!	transaction type is either a Cash Sale or a Credit Memo.
	!	.lm -5
	!
	! Index:
	!	.x Receipt #>Sales Journal
	!	.x Sales Journal>Receipt #
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD008
	!	^*(08) Check _#\*
	!	.b
	!	.lm +5
	!	The ^*Check _#\* field is provided to enter the number of a check
	!	issued by a customer in the event of a cash sale.  If a check was
	!	not received for the sale, or if the record represents a summary of
	!	cash sales such as a register total, this field would be left blank.
	!	.b
	!	The field will accommodate up to six (6) alphanumeric characters.
	!	.b
	!	^*Note:\* During an Add function, this field is bypassed unless the
	!	transaction type represents either a Cash Sale or a Credit Memo.
	!	.lm -5
	!
	! Index:
	!	.x Check #>Sales Journal
	!	.x Sales Journal>Check #
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD009
	!	.x Deposit #>Sales Journal
	!	^*(09) Deposit _#\*
	!	.b
	!	.lm +5
	!	The ^*Deposit _#\* field enters the number
	!	of the bank deposit in which a cash receipt is included.  If CMC's
	!	Check Reconciliation is used, the deposit _# field ^&must\& contain
	!	a valid deposit number.  The use of this field is optional if the
	!	Check Reconciliation system is not used.  It is important that
	!	deposit numbers assigned be outside the parameters established for
	!	check numbers.
	!	.b
	!	The field will accommodate up to six (6) characters.
	!	.b
	!	^*Note:\* During an Add function, this field is bypassed unless the
	!	transaction type is either a Cash Sale or a Credit Memo.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Deposit #
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD010
	!	^*(10) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field in the Sales Journal is provided to enter the
	!	total invoice amount, including any freight, sales taxes, etc.
	!	.b
	!	^*Note:\* Sales tax will be a separate line item in the distribution.
	!	.b
	!	The field will accommodate an entry as large as a plus
	!	(+) or a minus (-) 999999.99.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Amount
	!	.x Amount>Sales Journal
	!
	!--
	!+-+-+
	!++
	! Abstract:LINE_ITEMS
	!	^*Line__items\*
	!	.b
	!	.lm +5
	!	The ^*Line__items\* portion of this screen indicates how
	!	the transaction is to be allocated to various accounts and sub__codes.
	!	.b
	!	This portion of the screen will scroll, allowing as many as forty
	!	(40) line item distributions to be made.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Line Items
	!	.x Line Items>Sales Journal
	!
	!--
