1	%TITLE "Print Accounts Receivable Dunning Letters Using a Form"
	%SBTTL "AR_FORM_DUNNING"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:ARDUNN
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Dunning Letters Report\* program prints Accounts Receivable Dunning
	!	Letters in an alignment form.
	!	.lm -5
	!
	! Index:
	!	.x Print Accounts Receivable Dunning Letters
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_FORM_DUNNING/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_FORM_DUNNING, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_FORM_DUNNING.OBJ;*
	!
	! Author:
	!
	!	08/01/88 - Kevin Handy
	!
	! Modification history:
	!
	!	06/04/91 - J. Shad Rydalch
	!		Added form fields for address that will keep
	!		post office happy.
	!
	!	09/24/91 - Frank Starman & Dan Perkins
	!		Cleaned up program logic,
	!		program code, and
	!		error trapping.
	!
	!	06/10/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM.
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/01/93 - Kevin Handy
	!		Moved setting up address without blank lines
	!		stuff to after deciding if the customer should
	!		be printed.  Why waste CPU time?
	!
	!	02/01/93 - Kevin Handy
	!		Added code to create AR_35CUSTOM::CONTACT field
	!		by looking for a title of "LETTER" in the contact
	!		file, and use customer name if not found.
	!
	!	02/02/93 - Kevin Handy
	!		Fixed bug (by doing a round) where it would
	!		print zero balances sometimes.
	!
	!	02/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/23/93 - Kevin Handy
	!		Removed JJ$ and READ_SYSJOB.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/04/95 - Kevin Handy
	!		Changed PTDSALES to LAST_PAID.
	!
	!	10/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/09/97 - Kevin Handy
	!		Lose PRNT.CH% variable
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/98 - Kevin Handy
	!		Don't bother erasing SMG_SCREEN_DATA, which is
	!		never created.
	!
	!	11/09/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.HB"
	MAP	(AR_CONTACT)	AR_CONTACT_CDD		AR_CONTACT

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP	(AR_OPEN)	AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP	(AR_CUSBAL)	AR_CUSBAL_CDD		AR_CUSBAL
	DIM	AR_CUSBAL_CDD	ARRAY_CUSBAL(100%)

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP	(UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT
	DECLARE			UTL_REPORT_CDD		UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP	(UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM	FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	MAP (STMT_FORM) &
		TOTAL_AMT, &
		TOTAL_DISC, &
		TOTAL_OTHER, &
		TOTAL_NET, &
		INVOICE_DATE$ = 8%, &
		BEGIN_BALANCE, &
		AR_INVOICE_TOT%, &
		AR_RECEIPT_TOT%, &
		CURRENT_LINE%, &
		INVOICE_TOTAL, &
		MAX_LINE%, &
		PAGE_NUMBER%, &
		AR_CUSTOM.ADDLINE$(5%) = 50%, &
		CONTACT$ = 30%

	COM (CH_AR_CUSBAL) AR_CUSBAL.CH%
	COM (CH_AR_OPEN) AR_OPEN.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION AR_FUNC_AGE
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "ARDUNN"

	!
	! Look up device
	!
	CALL READ_DEVICE("AR_FORM", AR_FORM.DEV$, STAT%)

	!***************************************************************
	! Open all of the files
	!***************************************************************

300	!
	! Open vendor master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

305	!
	! Open contact file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.OPN"
	USE
		CONTINUE 310 IF ERR = 5%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

310	!
	! Open cash dispersments file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AR_CONTROL file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.MOD"
		GET #AR_CONTROL.CH%, REGARDLESS
		UNLOCK #AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

325	!
	! Open customer balance file
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"

335	! Open COUNTRY file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.OPN"
	USE
		CONTINUE GetReport
	END WHEN

 GetReport:
340	!
	! Get Report
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL


	INVOICE_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%))
	!++
	! Abstract:FLD01
	!	.x Statement Date>Dunning Form
	!	^*(01) Statement Date\*
	!	.b
	!	.lm +5
	!	The ^*Statement Date\* specifies the date which will be printed on the
	!	statements.  The format for entry is MMDDYY or MMDDYYYY.
	!	.b
	!	^*Note:\* The aging information will be calculated as of this date. Any
	!	invoice entries or payments which are dated after this date will
	!	not appear on the statement (the statement date is used as the cutoff date).
	!	.lm -5
	!
	! Index:
	!	.x Dunning Form>Statement Date
	!
	!--

	GL_WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)
	!++
	! Abstract:FLD02
	!	^*(02) Account Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field specifies designated accounts
	!	receivable to be printed by entering a "Wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Account Wildcard>Dunning Form
	!	.x Dunning Form>Account Wildcard
	!
	!--
	GL_WILDCARD$ = "*"  IF GL_WILDCARD$ = ""

	CUTOFF$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!
	!
	! Index:
	!
	!
	!--

	FROM_PERIOD% = VAL%(TRM$(UTL_REPORTX::OPTDEF(3%)))
	!++
	! Abstract:FLD04
	!	.x From Aging Period>Dunning Form
	!	^*(04) From Aging Period\*
	!	.b
	!	.lm +5
	!	The ^*From Aging Period\* field specifies
	!	the selected aging period that a customer have a
	!	balance in to show on the report.
	!	.table 3,25
	!	.te
	!	^*0\*	- Current to 29
	!	.te
	!	^*1\*	- 30 to 59
	!	.te
	!	^*2\*	- 60 to 119
	!	.te
	!	^*3\*	- 120 or more
	!	.End table
	!	A blank field will cause the report to begin with the first aging period
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x Dunning Form>From Aging Period
	!
	!--

	TO_PERIOD% = VAL%(TRM$(UTL_REPORTX::OPTDEF(4%)))
	!++
	! Abstract:FLD05
	!	.x To Aging Period>Dunning Form
	!	^*(05) To Aging Period\*
	!	.b
	!	.lm +5
	!	The ^*To Aging Period\* field specifies
	!	the Aging Period which a customer must have
	!	a balance in to appear on the report.
	!	.table 3,25
	!	.te
	!	^*0\*	- Current to 29
	!	.te
	!	^*1\*	- 30 to 59
	!	.te
	!	^*2\*	- 60 to 119
	!	.te
	!	^*3\*	- 120 or more
	!	.End table
	!	A blank field will cause the report to end with the last aging period in
	!	the file.
	!	.lm -5
	!
	! Index:
	!	.x Dunning Form>To Aging Period
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)
	!++
	! Abstract:FLD06
	!	^*(06) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field specifies
	!	the beginning item to print.
	!	The value must be in agreement with the value
	!	entered in field (8) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item number.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Dunning Form
	!	.x Dunning From>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)
	!++
	! Abstract:FLD07
	!	^*(07) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field specifies
	!	the ending item to be printed. The value must be in
	!	agreement with the value entered
	!	in field (8), Sort by.
	!	.b
	!	A blank field ends the report with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Dunning Form
	!	.x Dunning Form>From Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)
	!++
	! Abstract:FLD08
	!	.x Sort by>Dunning Form
	!	^*(08) Sort By\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field specifies the order
	!	in which the form will print.
	!	.b
	!	Valid sort orders are:
	!	.table 3,25
	!	.te
	!	^*N\*	- Number
	!	.te
	!	^*T\*	- Type
	!	.te
	!	^*C\*	- Category
	!	.te
	!	^*A\*	- Alphabetical
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Dunning Form>Sort By
	!
	!--

	REPORT$ = REPORT$ + "$" + EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)
	!++
	! Abstract:FLD09
	!	^*(09) Form Name\*
	!	.b
	!	.lm +5
	!	The ^*Form Name\* field specifies the form name to use.
	!	(Forms are user defined in the Utility menu.)
	!	.lm -5
	!
	! Index:
	!	.x Form Name>Dunning Form
	!	.x Dunning From>Form Name
	!
	!--

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%

	CASE "T"
		K_NUM% = 1%

	CASE "C"
		K_NUM% = 2%

	CASE "A"
		K_NUM% = 3%

	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Init the form
	!
	GOSUB InitForm

	!
	! GOTO aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through CDJ file
	!*******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, KEY #K_NUM% GE FROM_ITEM$
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check for end item
	!
	IF TO_ITEM$ <> ""
	THEN

		SELECT SORTBY$

		CASE "N"
			GOTO ExitProgram IF AR_35CUSTOM::CUSNUM > TO_ITEM$

		CASE "T"
			GOTO ExitProgram IF AR_35CUSTOM::TTYPE > TO_ITEM$

		CASE "C"
			GOTO ExitProgram IF AR_35CUSTOM::CATEGORY > TO_ITEM$

		CASE "A"
			GOTO ExitProgram IF AR_35CUSTOM::ALPSRT > TO_ITEM$

		END SELECT

	END IF

	!
	! Age this customer
	!
	IF AR_FUNC_AGE(AR_35CUSTOM::CUSNUM, AR_35CUSTOM::METHOD, &
		INVOICE_DATE$, CUTOFF$, &
		NUM_ACCT%, ARRAY_CUSBAL())
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to age " + AR_35CUSTOM::CUSNUM, 0%)
	END IF

	!
	! Grand total the accounts
	!
	AR_CUSBAL::CREDIT = 0.0
	AR_CUSBAL::AGING(I%) = 0.0 FOR I% = 0% TO 4%
	AR_CUSBAL::FUTURE = 0.0
	AR_CUSBAL::YTDSERVICE = 0.0
	AR_CUSBAL::LAST_PAID = 0.0
	AR_CUSBAL::YTDSALES = 0.0
	AR_CUSBAL::CHARGE = 0.0

	FOR LOOP% = 1% TO NUM_ACCT%

		!
		! Only accept those that have the desired wildcard account
		!
		IF COMP_STRING(ARRAY_CUSBAL(LOOP%)::ACCT, GL_WILDCARD$) <> 0%
		THEN
			AR_CUSBAL::CREDIT = AR_CUSBAL::CREDIT + &
				ARRAY_CUSBAL(LOOP%)::CREDIT
			AR_CUSBAL::AGING(I%) = AR_CUSBAL::AGING(I%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(I%) &
				FOR I% = 0% TO 4%
			AR_CUSBAL::FUTURE = AR_CUSBAL::FUTURE + &
				ARRAY_CUSBAL(LOOP%)::FUTURE
			AR_CUSBAL::YTDSERVICE = AR_CUSBAL::YTDSERVICE + &
				ARRAY_CUSBAL(LOOP%)::YTDSERVICE
			AR_CUSBAL::LAST_PAID = AR_CUSBAL::LAST_PAID + &
				ARRAY_CUSBAL(LOOP%)::LAST_PAID
			AR_CUSBAL::YTDSALES = AR_CUSBAL::YTDSALES + &
				ARRAY_CUSBAL(LOOP%)::YTDSALES
			AR_CUSBAL::CHARGE = AR_CUSBAL::CHARGE + &
				ARRAY_CUSBAL(LOOP%)::CHARGE
		END IF
	NEXT LOOP%

	!
	! Calculate the beginning balance
	!
	BEGIN_BALANCE = AR_CUSBAL::CHARGE
	BEGIN_BALANCE = FUNC_ROUND(BEGIN_BALANCE + AR_CUSBAL::AGING(I%), 2%) &
		FOR I% = 0% TO 4%

	!
	! We need to make sure that there is something to print
	! for this customer before proceeding.  We will print
	! him if he has a beginning balance, or if he has any
	! current records in the open file.
	!

	!
	! Does he have a beginning balance?
	!
	GOTO GetNextRec IF BEGIN_BALANCE = 0.0

	!
	! Skip if has nothing in the from_PERIOD to the to_PERIOD
	! range of periods.
	!
	FOR I% = FROM_PERIOD% TO TO_PERIOD%
		GOTO 2015 IF AR_CUSBAL::AGING(I%) <> 0.0
	NEXT I%

	GOTO GetNextRec

2015	WHEN ERROR IN
		GET #UTL_COUNTRY.CH%, &
			KEY #0% EQ AR_35CUSTOM::COUNTRY, &
			REGARDLESS
	USE
		CONTINUE FixAddress
	END WHEN

 FixAddress:
	IF AR_35CUSTOM::COUNTRY = "US" OR AR_35CUSTOM::COUNTRY = ""
	THEN
		UTL_COUNTRY::DESCR = ""
	END IF

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(AR_35CUSTOM::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM::ADD2, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM::ADD3, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM::ADD3, 8% + 16% + 32% + 128%)
	END IF

	IF AR_35CUSTOM::COUNTRY = "US" OR AR_35CUSTOM::COUNTRY = ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(EDIT$(AR_35CUSTOM::CITY, 128%) + ", " + &
			AR_35CUSTOM::STATE + " " + AR_35CUSTOM::ZIP, &
			8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(UTL_COUNTRY::DESCR, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(UTL_COUNTRY::DESCR, 8% + 16% + 32% + 128%)
	END IF

	! Blank other lines out
	AR_CUSTOM.ADDLINE$(LOOP%) = "" FOR LOOP% = I% + 1% TO 5%

2020	CONTACT$ = AR_35CUSTOM::CUSNAM

	WHEN ERROR IN
		GET #AR_CONTACT.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 2030
	END WHEN

	WHILE AR_CONTACT::CUSNUM = AR_35CUSTOM::CUSNUM

		CONTACT$ = AR_CONTACT::CONTACT_NAME &
			IF AR_CONTACT::TITLE = "LETTER"

		WHEN ERROR IN
			GET #AR_CONTACT.CH%, REGARDLESS
		USE
			CONTINUE 2030
		END WHEN
	NEXT

2030	!
	! Skip if has nothing in the from_PERIOD to the to_PERIOD
	! range of periods.
	!
	FOR I% = FROM_PERIOD% TO TO_PERIOD%
		IF AR_CUSBAL::AGING(I%) <> 0.0
		THEN
			GOSUB PrintStmt
			GOTO ExitProgram IF UTL_REPORTX::STAT
			GOTO GetNextRec
		END IF
	NEXT I%

	GOTO GetNextRec

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	CLOSE AR_CONTROL.CH%
	CLOSE AR_OPEN.CH%

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
	!***************************************************************
	! Print the Statement now
	!***************************************************************

	TOTAL_AMT, TOTAL_DISC, TOTAL_NET = 0.0

	LINE_COUNT% = 0%
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
	! Print lines to botton of the statement
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO FORM_GROUP(FRM_TOP%)::NUMBER

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
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO FORM_GROUP(FRM_TOP%)::NUMBER

	!
	! Do they need another?
	!
	GOTO Alignment

 AlignmentReturn:
	RETURN

	%PAGE

	!*******************************************************************
	! Initilize Statement form
	!*******************************************************************
 InitForm:

	!
	! Get form from the AR form library
	!
	SMG_STATUS% = OUTP_FORMINIT( &
		AR_FORM.DEV$ + "AR_FORM", REPORT$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "Statement form is missing", &
			"E", SCOPE::PRG_PROGRAM, REPORT$, NUM1$(SMG_STATUS%))
		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_TOP% = 0%
	FRM_BOTTOM% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-BOTTOM"
			FRM_BOTTOM% = I%

		END SELECT

	NEXT I%

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
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)	AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL) AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)	AR_CUSBAL_CDD	AR_CUSBAL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD	UTL_COUNTRY

	MAP (STMT_FORM) &
		TOTAL_AMT, &
		TOTAL_DISC, &
		TOTAL_OTHER, &
		TOTAL_NET, &
		INVOICE_DATE$ = 8%, &
		BEGIN_BALANCE, &
		AR_INVOICE_TOT%, &
		AR_RECEIPT_TOT%, &
		CURRENT_LINE%, &
		INVOICE_TOTAL, &
		MAX_LINE%, &
		PAGE_NUMBER%, &
		AR_CUSTOM.ADDLINE$(5%) = 50%, &
		CONTACT$ = 30%

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
	! Fields for the AR_35CUSTOM file
	!************************************************************

	CASE "AR_35CUSTOM::CUSNUM", "AR_CUSTOM::CUSNUM"
		TEXTVALUE$ = AR_35CUSTOM::CUSNUM

	CASE "AR_35CUSTOM::CUSNAM", "AR_CUSTOM::CUSNAM"
		TEXTVALUE$ = AR_35CUSTOM::CUSNAM

	CASE "AR_35CUSTOM::TTYPE", "AR_CUSTOM::TTYPE"
		TEXTVALUE$ = AR_35CUSTOM::TTYPE

	CASE "AR_35CUSTOM::CATEGORY", "AR_CUSTOM::CATEGORY"
		TEXTVALUE$ = AR_35CUSTOM::CATEGORY

	CASE "AR_35CUSTOM::BDATE", "AR_CUSTOM::BDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM::BDATE, 8%)

	CASE "AR_35CUSTOM::SSTATUS", "AR_CUSTOM::SSTATUS"
		TEXTVALUE$ = AR_35CUSTOM::SSTATUS

	CASE "AR_35CUSTOM::EDATE", "AR_CUSTOM::EDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM::EDATE, 8%)

	CASE "AR_35CUSTOM::ADD1", "AR_CUSTOM::ADD1"
		TEXTVALUE$ = AR_35CUSTOM::ADD1

	CASE "AR_35CUSTOM::ADD2", "AR_CUSTOM::ADD2"
		TEXTVALUE$ = AR_35CUSTOM::ADD2

	CASE "AR_35CUSTOM::ADD3", "AR_CUSTOM::ADD3"
		TEXTVALUE$ = AR_35CUSTOM::ADD3

	CASE "AR_35CUSTOM::CITY", "AR_CUSTOM::CITY"
		TEXTVALUE$ = AR_35CUSTOM::CITY

	CASE "AR_35CUSTOM::STATE", "AR_CUSTOM::STATE"
		TEXTVALUE$ = AR_35CUSTOM::STATE

	CASE "AR_35CUSTOM::ZIP", "AR_CUSTOM::ZIP"
		TEXTVALUE$ = AR_35CUSTOM::ZIP

	CASE "AR_35CUSTOM::COUNTRY", "AR_CUSTOM::COUNTRY"
		TEXTVALUE$ = AR_35CUSTOM::COUNTRY

	CASE "AR_35CUSTOM::COUNTY", "AR_CUSTOM::COUNTY"
		TEXTVALUE$ = AR_35CUSTOM::COUNTY

	CASE "AR_CUSTOM.ADDLINE1"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(1%)

	CASE "AR_CUSTOM.ADDLINE2"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(2%)

	CASE "AR_CUSTOM.ADDLINE3"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(3%)

	CASE "AR_CUSTOM.ADDLINE4"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(4%)

	CASE "AR_CUSTOM.ADDLINE5"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(5%)

	CASE "AR_35CUSTOM::PHONE", "AR_CUSTOM::PHONE"
		TEXTVALUE$ = AR_35CUSTOM::PHONE

	CASE "AR_35CUSTOM::METHOD", "AR_CUSTOM::METHOD"
		TEXTVALUE$ = AR_35CUSTOM::METHOD

	CASE "AR_35CUSTOM::STMTFLG", "AR_CUSTOM::STMTFLG"
		TEXTVALUE$ = AR_35CUSTOM::STMTFLG

	CASE "AR_35CUSTOM::ALPSRT", "AR_CUSTOM::ALPSRT"
		TEXTVALUE$ = AR_35CUSTOM::ALPSRT

	CASE "AR_35CUSTOM::SERCHRG", "AR_CUSTOM::SERCHRG"
		TEXTVALUE$ = AR_35CUSTOM::SERCHRG

	CASE "AR_35CUSTOM::TAXCODE", "AR_CUSTOM::TAXCODE"
		TEXTVALUE$ = AR_35CUSTOM::TAXCODE

	CASE "AR_35CUSTOM::TAXEXEMP", "AR_CUSTOM::TAXEXEMP"
		TEXTVALUE$ = AR_35CUSTOM::TAXEXEMP

	CASE "AR_35CUSTOM::LOCATION", "AR_CUSTOM::LOCATION"
		TEXTVALUE$ = AR_35CUSTOM::LOCATION

	CASE "AR_35CUSTOM::TERMS", "AR_CUSTOM::TERMS"
		TEXTVALUE$ = AR_35CUSTOM::TERMS

	CASE "AR_35CUSTOM::CARRIER", "AR_CUSTOM::CARRIER"
		TEXTVALUE$ = AR_35CUSTOM::CARRIER

	CASE "AR_35CUSTOM::SALESMAN", "AR_CUSTOM::SALESMAN"
		TEXTVALUE$ = AR_35CUSTOM::SALESMAN

	CASE "AR_35CUSTOM::CREDITLIM", "AR_CUSTOM::CREDITLIM"
		REALVALUE  = AR_35CUSTOM::CREDITLIM

	CASE "AR_35CUSTOM::DISCOUNT", "AR_CUSTOM::DISCOUNT"
		REALVALUE  = AR_35CUSTOM::DISCOUNT

	CASE "AR_35CUSTOM::BACKORDER", "AR_CUSTOM::BACKORDER"
		TEXTVALUE$ = AR_35CUSTOM::BACKORDER

	CASE "AR_35CUSTOM::TAXFLAG", "AR_CUSTOM::TAXFLAG"
		TEXTVALUE$ = AR_35CUSTOM::TAXFLAG

	CASE "AR_35CUSTOM::CONTACT", "AR_CUSTOM::CONTACT"
		TEXTVALUE$ = CONTACT$

	!************************************************************
	! Country file fields
	!************************************************************
	CASE "UTL_COUNTRY::DESCR"
		TEXTVALUE$ = UTL_COUNTRY::DESCR

	!************************************************************
	! Control file fields
	!************************************************************

	CASE "AR_CONTROL::AR_ACCT"
		TEXTVALUE$ = AR_CONTROL::AR_ACCT

	CASE "AR_CONTROL::RETAIN"
		REALVALUE = AR_CONTROL::RETAIN

	CASE "AR_CONTROL::LASTPERCLOSE"
		REALVALUE = AR_CONTROL::LASTPERCLOSE

	CASE "AR_CONTROL::YEAR"
		TEXTVALUE$ = AR_CONTROL::YEAR

	CASE "AR_CONTROL::CLOSEFLAG"
		TEXTVALUE$ = AR_CONTROL::CLOSEFLAG

	CASE "AR_CONTROL::AGEPER1"
		REALVALUE = AR_CONTROL::AGEPER(0%)

	CASE "AR_CONTROL::AGEPER2"
		REALVALUE = AR_CONTROL::AGEPER(1%)

	CASE "AR_CONTROL::AGEPER3"
		REALVALUE = AR_CONTROL::AGEPER(2%)

	CASE "AR_CONTROL::AGEPER4"
		REALVALUE = AR_CONTROL::AGEPER(3%)

	CASE "AR_CONTROL::AGEPER5"
		REALVALUE = AR_CONTROL::AGEPER(4%)

	CASE "AR_CONTROL::AGENAM1"
		TEXTVALUE$ = AR_CONTROL::AGENAM(0%)

	CASE "AR_CONTROL::AGENAM2"
		TEXTVALUE$ = AR_CONTROL::AGENAM(1%)

	CASE "AR_CONTROL::AGENAM3"
		TEXTVALUE$ = AR_CONTROL::AGENAM(2%)

	CASE "AR_CONTROL::AGENAM4"
		TEXTVALUE$ = AR_CONTROL::AGENAM(3%)

	CASE "AR_CONTROL::AGENAM5"
		TEXTVALUE$ = AR_CONTROL::AGENAM(4%)

	CASE "AR_CONTROL::CTITLE"
		TEXTVALUE$ = AR_CONTROL::CTITLE

	!************************************************************
	! Customer balance
	!************************************************************

	CASE "AR_CUSBAL::CUSNUM"
		TEXTVALUE$ = AR_CUSBAL::CUSNUM

	CASE "AR_CUSBAL::ACCT"
		TEXTVALUE$ = AR_CUSBAL::ACCT

	CASE "AR_CUSBAL::CREDIT"
		REALVALUE = AR_CUSBAL::CREDIT

	CASE "AR_CUSBAL::AGING1"
		REALVALUE = AR_CUSBAL::AGING(0%)

	CASE "AR_CUSBAL::AGING2"
		REALVALUE = AR_CUSBAL::AGING(1%)

	CASE "AR_CUSBAL::AGING3"
		REALVALUE = AR_CUSBAL::AGING(2%)

	CASE "AR_CUSBAL::AGING4"
		REALVALUE = AR_CUSBAL::AGING(3%)

	CASE "AR_CUSBAL::AGING5"
		REALVALUE = AR_CUSBAL::AGING(4%)

	CASE "AR_CUSBAL::FUTURE"
		REALVALUE = AR_CUSBAL::FUTURE

	CASE "AR_CUSBAL::YTDSERVICE"
		REALVALUE = AR_CUSBAL::YTDSERVICE

	CASE "AR_CUSBAL::LAST_PAID"
		REALVALUE = AR_CUSBAL::LAST_PAID

	CASE "AR_CUSBAL::YTDSALES"
		REALVALUE = AR_CUSBAL::YTDSALES

	CASE "AR_CUSBAL::CHARGE"
		REALVALUE = AR_CUSBAL::CHARGE

	CASE "AR_CUSBAL::LAST_CHARGE"
		TEXTVALUE$ = PRNT_DATE(AR_CUSBAL::LAST_CHARGE, 6%)

	CASE "AR_CUSBAL::LAST_UPDATE"
		TEXTVALUE$ = PRNT_DATE(AR_CUSBAL::LAST_UPDATE, 6%)

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "TOTAL_DISC"		! Total discount amount
		REALVALUE = TOTAL_DISC

	CASE "TOTAL_OTHER"		! Other total
		REALVALUE = TOTAL_OTHER

	CASE "TOTAL_NET", "AMOUNT", "TOTAL_AMT"	! Total Amount Due
		REALVALUE = AR_CUSBAL::CHARGE + AR_CUSBAL::FUTURE
		REALVALUE = REALVALUE + AR_CUSBAL::AGING(I%) &
			FOR I% = 0% TO 4%

	CASE "INVOICE_DATE"		! Date of Invoice
		TEXTVALUE$ = PRNT_DATE(INVOICE_DATE$, 6%)

	CASE "BEGIN_BALANCE"		! Begining Balance
		REALVALUE = BEGIN_BALANCE

	CASE "PAGE_NUMBER"		! Page Number
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)
	END SELECT

	END SUB
