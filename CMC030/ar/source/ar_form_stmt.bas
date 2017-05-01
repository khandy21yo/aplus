1	%TITLE "Print AR Statements Using a Form"
	%SBTTL "AR_FORM_STMT"
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
	! ID:ARSTMT
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints Account Receivable Statements
	!	in an alignment form.
	!	.lm -5
	!
	! Index:
	!	.x Print Accounts Receivable Statements
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_FORM_STMT/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_FORM_STMT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_FORM_STMT.OBJ;*
	!
	! Author:
	!
	!	03/22/88 - Kevin Handy
	!
	! Modification history:
	!
	!	05/14/88 - Lance Williams
	!		Modified to clean up header.
	!
	!	09/25/89 - Kevin Handy
	!		Modified so that statement flag in the master
	!		file was handled.
	!
	!	08/10/90 - Kevin Handy
	!		Modified to move credit memo's (08) from the right
	!		hand side to the left hand side.
	!
	!	05/11/91 - Frank F. Starman, Shad Rydalch
	!		Add option to print lines by date. Fill some variables
	!		for AR_OPEN file with strings rather then leave them blank.
	!		This still must be reviewed.
	!
	!	06/04/91 - J. Shad Rydalch
	!		Added form fields for address that will keep
	!		post office happy.
	!
	!	06/17/91 - Frank F. Starman
	!		Initialize INVLOOP% variable with 1% (zero costs
	!		to print twice blank invoices)
	!
	!	06/18/91 - Craig Tanner
	!		Modified to update user UTL_REPORT file from the master
	!		every time form is run.
	!
	!	03/12/92 - Kevin Handy
	!		Removed duplicate error trap (check)
	!
	!	05/04/92 - Kevin Handy
	!		Modified to increment before printing top of additional
	!		pages, instead of waiting until after.
	!
	!	05/07/92 - Kevin Handy
	!		Modified definition of arrays in LOAD_FORMVAR to match
	!		those defined here for someones undefined chahge
	!		in the header from 500 to 800.
	!
	!	06/03/92 - Dan Perkins
	!		Modified so totals are not printed until the last
	!		page.  Use OUTP_NEWPAGE at bottom of form.
	!
	!	06/08/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM function.
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/09/92 - Dan Perkins
	!		Fixed bug in the TO_ITEM test after the get.
	!
	!	09/24/92 - Dan Perkins
	!		Trap CUSBAL open err at line 325.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/26/93 - Kevin Handy
	!		Added DUEDATE and DISCOUNTDATE to form.
	!
	!	06/28/93 - Kevin Handy
	!		Modification to fix bug where would not print
	!		an invoice if the net for customer was not zero and
	!		were printing in invoice date order.
	!
	!	09/23/93 - Kevin Handy
	!		Removed JJ$ and READ_SYSJOB.
	!
	!	01/27/94 - Kevin Handy
	!		Modified alignment form to fill in body instead of
	!		only doing three lines.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/19/95 - Kevin Handy
	!		Clean up some extra definitions.
	!
	!	12/04/95 - Kevin Handy
	!		Changed PTDSALES to LAST_PAID.
	!
	!	05/01/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/02/96 - Kevin Handy
	!		Added "AR_INVOICE::INVNUMX" field.
	!
	!	06/19/96 - Kevin Handy
	!		Added transaction type "11", adjustment.
	!		Reformat source code.
	!
	!	05/09/97 - Kevin Handy
	!		Lose PRNT.CH% variable
	!
	!	09/08/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	04/16/98 - Kevin Handy
	!		Added code to pull in UTL_PROFILE and UTL_LOCATION
	!
	!	05/04/98 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/28/98 - Kevin Handy
	!		Changed to use '%INCLUDE' instead of '%INCLUDE %FROM
	!		%CDD'.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/98 - Kevin Handy
	!		Don't bother erasing SMG_SCREEN_DATA, which is
	!		never created.
	!
	!	08/10/99 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/09/99 - Kevin Handy
	!		Introduced UTL_CUS_LOCATION stuff
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
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
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN
	COM (AR_OPEN_ARRAY)	AR_OPEN_CDD		AR_INVOICE(800%), &
							AR_RECEIPT(800%)

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DIM			AR_CUSBAL_CDD		ARRAY_CUSBAL(100%)

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE) UTL_PROFILE_CDD UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION) UTL_LOCATION_CDD UTL_LOCATION
	MAP (UTL_MAIN_LOCATION) UTL_LOCATION_CDD UTL_MAIN_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT
	DECLARE			UTL_REPORT_CDD		UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(10%)

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
		AR_CUSTOM.ADDLINE$(4%) = 50%

	COM (CH_AR_CUSBAL) AR_CUSBAL.CH%
	COM (CH_AR_OPEN) AR_OPEN.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION OUTP_INITFORM
	EXTERNAL LONG	FUNCTION AR_FUNC_AGE

	DIM INVARY$(800%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "ARSTMT"

	!
	! Look up device
	!
	CALL READ_DEVICE("AR_FORM", AR_FORM.DEV$, STAT%)

	!***************************************************************
	! Open all of the files
	!***************************************************************

300	!
	! Open AR_35CUSTOM file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

302	!
	! Get progile information
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"

		GET #UTL_PROFILE.CH%, REGARDLESS

		CLOSE #UTL_PROFILE.CH%
	USE
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

304	!
	! Get information for default location
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"

		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ UTL_PROFILE::MAINLOCATION, &
			REGARDLESS

		UTL_MAIN_LOCATION = UTL_LOCATION
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

310	!
	! Open AR_OPEN file
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
	! Open AR_CUSBAL file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

340	!
	! Get Report
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	INVOICE_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!	.x Statement Date>Statement
	!	^*(01) Statement Date\*
	!	.b
	!	.lm +5
	!	The ^*Statement date\* field specifies date to be
	!	printed on the statement.  The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	^*Note:\* The aging information is calculated as of this
	!	date. Any invoice entries or payments which are
	!	dated after this date will not appear on the
	!	statement (the statement date is used as the
	!	cutoff date).
	!	.lm -5
	!
	! Index:
	!	.x Statement>Statement Date
	!
	!--

	GL_WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Account Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Account Wildcard\* field specifies which
	!	invoices are to be printed. Only those invoices which
	!	have an account number matching the wildcard will be
	!	printed.
	!	.b
	!	A blank in this field will cause all invoices to print.
	!	.lm -5
	!
	! Index:
	!	.x Account Wildcard>Statement
	!	.x Statement>Account Wildcard
	!
	!--

	GL_WILDCARD$ = "*"  IF GL_WILDCARD$ = ""

	CUTOFF$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	.x Cutoff>Period
	!	^*(03) Cutoff Period\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Period\* specifies the last General Ledger period
	!	to include on the statements.
	!	No transactions posted beyond this period
	!	will appear on the report.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Period>Cutoff
	!
	!--

	PRINT_ZERO$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(3%), -1%), 1%)

	!++
	! Abstract:FLD04
	!	.x Zero Balance Statement
	!	^*(04) Print Zero\*
	!	.b
	!	.lm +5
	!	The ^*Print Zero\* balances field specifies if
	!	statements which have zero balances should be printed.
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	PRINT_CREDIT$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(4%), -1%), 1%)

	!++
	! Abstract:FLD05
	!	.x Credit Statement
	!	^*(04) Print Credit\*
	!	.b
	!	.lm +5
	!	The ^*Print Credit\* balance field specifies
	!	if print statements with credit balances should be printed.
	!	.B
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field specifies
	!	the to beginning item to print. The value must be
	!	in agreement with field (8) Sort by.
	!	.b
	!	A blank field will cause the statement to start with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Accounts Receivable Statements
	!	.x Accounts Receivable Statements>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies the
	!	ending item to be printed. The value
	!	must be in agreement with field (8) Sort by.
	!	.b
	!	A blank field causes the statement to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Accounts Receivable Statements
	!	.x Accounts Receivable Statements>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	.x Sort by>Statement
	!	^*(08) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field specifies the order the statements will
	!	be printed in.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer number
	!	.te
	!	^*T\* - Type
	!	.te
	!	^*C\* - Category
	!	.te
	!	^*A\* - Alpha sort
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Statement>Sort by
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

	LNSORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)

	!++
	! Abstract:FLD09
	!	.x Line Sort by>Statement
	!	^*(09) Line Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Line Sort by\* field specifies the order
	!	that the invoice lines will be printed.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*D\* - Invoice Date
	!	.te
	!	^*I\* - Invoice Number
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Statement>Line Sort by
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(9%))

	!++
	! Abstract:FLD10
	!	^*(10) Form Name\*
	!	.b
	!	.lm +5
	!	The ^*Form Name\* field specifies the
	!	form to use for printing the report.
	!	.b
	!	(For more information on form printing refer to
	!	the Utility Section, Forms Controlling.)
	!	.lm -5
	!
	! Index:
	!	.x Form
	!
	!--

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Load the form
	!
	GOSUB LoadForm

	!
	! GOTO aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through CUSTOMER file
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
	SELECT SORTBY$

	CASE "N"
		GOTO ExitProgram IF AR_35CUSTOM::CUSNUM > TO_ITEM$ &
			AND TO_ITEM$ <> ""

	CASE "T"
		GOTO ExitProgram IF AR_35CUSTOM::TTYPE > TO_ITEM$ &
			AND TO_ITEM$ <> ""

	CASE "C"
		GOTO ExitProgram IF AR_35CUSTOM::CATEGORY > TO_ITEM$ &
			AND TO_ITEM$ <> ""

	CASE "A"
		GOTO ExitProgram IF AR_35CUSTOM::ALPSRT > TO_ITEM$ &
			AND TO_ITEM$ <> ""

	END SELECT

	!
	! Skip if non-printing
	!
	GOTO 2010 IF AR_35CUSTOM::STMTFLG = "N"

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

	I% = I% + 1%

	AR_CUSTOM.ADDLINE$(I%) = EDIT$(EDIT$(AR_35CUSTOM::CITY, 128%) + ", " + &
		AR_35CUSTOM::STATE + " " + AR_35CUSTOM::ZIP + " " + &
		AR_35CUSTOM::COUNTRY, 8% + 16% + 32% + 128%)

	!
	! Blank other lines out
	!
	AR_CUSTOM.ADDLINE$(LOOP%) = "" FOR LOOP% = I% + 1% TO 4%

2020	!
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
	AR_CUSBAL::CREDIT     = 0.0
	AR_CUSBAL::AGING(I%)  = 0.0 FOR I% = 0% TO 4%
	AR_CUSBAL::FUTURE     = 0.0
	AR_CUSBAL::YTDSERVICE = 0.0
	AR_CUSBAL::LAST_PAID   = 0.0
	AR_CUSBAL::YTDSALES   = 0.0
	AR_CUSBAL::CHARGE     = 0.0

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
	! Check total to see if we want to print out statement
	!
	REALVALUE = AR_CUSBAL::CHARGE + AR_CUSBAL::FUTURE

	REALVALUE = FUNC_ROUND(REALVALUE + AR_CUSBAL::AGING(I%), 2%) &
		FOR I% = 0% TO 4%

	SELECT REALVALUE

	CASE 0.0
		GOTO 2010 IF PRINT_ZERO$ = "N"

	CASE < 0.0
		GOTO 2010 IF PRINT_CREDIT$ = "N"

	END SELECT

	!
	! Calculate the beginning balance
	!
	IF AR_35CUSTOM::METHOD = "B"
	THEN
		BEGIN_BALANCE = AR_CUSBAL::CHARGE
		BEGIN_BALANCE = BEGIN_BALANCE + AR_CUSBAL::AGING(I%) &
			FOR I% = 1% TO 4%
	ELSE
		BEGIN_BALANCE = 0.0
	END IF

2050	!
	! We need to make sure that there is something to print
	! for this customer before proceeding.  We will print
	! him if he has a beginning balance, or if he has any
	! current records in the open file.
	!

	!
	! Does he have a beginning balance?
	!
	GOTO 2100 IF (BEGIN_BALANCE <> 0.0) AND (LNSORTBY$ <> "D")

	!
	! Search the open file for a printable record.
	!
	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #0% GE AR_35CUSTOM::CUSNUM
	USE
		CONTINUE 2180 IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	INVNUM$ = STRING$(LEN(AR_OPEN::INVNUM) + 1%, A"?"B)

	INVARY% = 0%

2060	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		SELECT LNSORTBY$

		CASE "I"
			CONTINUE 2180 IF ERR = 11%

		CASE "D"
			CONTINUE 2100 IF ERR = 11%

		END SELECT

		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	SELECT LNSORTBY$

	CASE "I"
		GOTO 2180 IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM

	CASE "D"
		GOTO 2100 IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM

	END SELECT

	!
	! Not good if future
	!
	GOTO 2060 IF (AR_OPEN::UPDATED > CUTOFF$) AND (CUTOFF$ <> "")

	!
	! Only accept those that have the desired wildcard account
	! on the first record.
	!
	IF INVNUM$ <> AR_OPEN::INVNUM
	THEN
		IF COMP_STRING(AR_OPEN::ARACCT, GL_WILDCARD$)
		THEN
			SELECT LNSORTBY$

			CASE "I"
				GOTO 2100

			CASE "D"
				INVARY% = INVARY% + 1%

				INVARY$(INVARY%) = AR_OPEN::TRADAT + &
					AR_OPEN::INVNUM
			END SELECT
		END IF

		INVNUM$ = AR_OPEN::INVNUM + ""
	END IF

	!
	! Try next record
	!
	GOTO 2060

2100	!
	! Is there anything to print
	!
	SELECT LNSORTBY$

	CASE "I"
		GOSUB PrintStmt

	CASE "D"
		GOSUB PrintStmt IF INVARY% <> 0% OR BEGIN_BALANCE <> 0.0
	END SELECT

2180	GOTO ExitProgram IF UTL_REPORTX::STAT

2200	!
	! Go get the next vendor record
	!
	GOTO 2010

	%PAGE

 ExitProgram:
4010	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	CLOSE AR_CONTROL.CH%, AR_OPEN.CH%

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

	!
	! Let's try to get this customers location information
	!
	IF UTL_LOCATION::LOCATION <> AR_35CUSTOM::LOCATION
	THEN
		WHEN ERROR IN
			GET #UTL_LOCATION.CH%, &
				KEY #0% EQ AR_35CUSTOM::LOCATION, &
				REGARDLESS
		USE
			UTL_LOCATION = UTL_MAIN_LOCATION
		END WHEN
	END IF

	GET #AR_OPEN.CH%, KEY #0% GE AR_35CUSTOM::CUSNUM, REGARDLESS

	TOTAL_AMT, TOTAL_DISC, TOTAL_NET = 0.0
	INVNUM$ = ""

	LINE_COUNT%  = 0%
	BODY_COUNT%  = 0%
	PAGE_NUMBER% = 1%

	!
	! Sort the invoice list.
	! Using the bubble sort because list should be very small,
	! and this sort is very easy to debug.
	!
	FOR LOOP1% = 1% TO INVARY%

		FOR LOOP2% = 1% TO INVARY% - LOOP1%

			IF INVARY$(LOOP2%) > INVARY$(LOOP2% + 1%)
			THEN
				TEMP$ = INVARY$(LOOP2%)
				INVARY$(LOOP2%) = INVARY$(LOOP2% + 1%)
				INVARY$(LOOP2% + 1%) = TEMP$
			END IF

		NEXT LOOP2%

	NEXT LOOP1%

18010	!
	! Print the top of statement
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	INVLOOP% = 1%

18020	!
	! Start of a new invoice number
	!
	SELECT LNSORTBY$

	CASE "I"
		GOTO 18090 IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM
		INVOICE$ = AR_OPEN::INVNUM + ""

	CASE "D"
		GOTO 18090 IF INVLOOP% > INVARY%
		INVOICE$ = RIGHT(INVARY$(INVLOOP%), 9%)

		WHEN ERROR IN
			GET #AR_OPEN.CH%, &
				KEY #0% GE AR_35CUSTOM::CUSNUM + INVOICE$, &
				REGARDLESS
		USE
			CONTINUE 18090
		END WHEN

	END SELECT

	AR_INVOICE_TOT% = 0%
	AR_RECEIPT_TOT% = 0%

18030	!
	! Handle change in customer number
	!
	SELECT LNSORTBY$

	CASE "I"
		GOTO 18090 IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM

		IF (INVOICE$ <> AR_OPEN::INVNUM)
		THEN
			GOSUB DumpLines
			GOTO 18020
		END IF

	CASE "D"
		IF (INVOICE$ <> AR_OPEN::INVNUM OR &
			AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM)
		THEN
			GOSUB DumpLines
			INVLOOP% = INVLOOP% + 1%
			GOTO 18020
		END IF
	END SELECT

	!
	! Skip record if date is not in range
	!
	GOTO 18040 IF (CUTOFF$ < AR_OPEN::UPDATED) AND (CUTOFF$ <> "")

	!
	! Place the invoice in the right columns
	!
	SELECT AR_OPEN::TRATYP

	!
	! Place on both sides
	!
	CASE "02"
		AR_INVOICE_TOT% = AR_INVOICE_TOT% + 1%
		AR_INVOICE(AR_INVOICE_TOT%) = AR_OPEN
		AR_RECEIPT_TOT% = AR_RECEIPT_TOT% + 1%
		AR_RECEIPT(AR_RECEIPT_TOT%) = AR_OPEN
		AR_RECEIPT(AR_RECEIPT_TOT%)::SALAMT = -AR_OPEN::SALAMT

	!
	! Place as invoice only
	!
	CASE "  ", "01", "03", "04", "05", "06", "07", "08"
		AR_INVOICE_TOT% = AR_INVOICE_TOT% + 1%
		AR_INVOICE(AR_INVOICE_TOT%) = AR_OPEN
		AR_RECEIPT(AR_INVOICE_TOT%) = AR_OPEN
		AR_RECEIPT(AR_INVOICE_TOT%)::SALAMT = 0.0

	!
	! Otherwise it is a receipt
	!
	CASE ELSE
		SELECT LNSORTBY$

		CASE "I"
			AR_RECEIPT_TOT% = AR_RECEIPT_TOT% + 1%
			AR_RECEIPT(AR_RECEIPT_TOT%) = AR_OPEN

		CASE "D"
			AR_INVOICE_TOT% = AR_INVOICE_TOT% + 1%
			AR_RECEIPT_TOT% = AR_INVOICE_TOT%
			AR_INVOICE(AR_INVOICE_TOT%) = AR_OPEN
			AR_RECEIPT(AR_INVOICE_TOT%) = AR_OPEN
			AR_INVOICE(AR_INVOICE_TOT%)::SALAMT = 0.0
		END SELECT
	END SELECT

18040	!
	! Read in next record
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 18090
	END WHEN

	GOTO 18030

18090	!
	! Finish up last of lines if any
	!
	GOSUB DumpLines

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
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
	! Figure out total lines to print
	!
	IF AR_INVOICE_TOT% > AR_RECEIPT_TOT%
	THEN
		MAX_LINE% = AR_INVOICE_TOT%
	ELSE
		MAX_LINE% = AR_RECEIPT_TOT%
	END IF

	GOTO 18290 IF MAX_LINE% = 0%

	!
	! Handle wild-card account
	!
	AR_INVOICE(1%) = AR_RECEIPT(1%) IF AR_INVOICE_TOT% = 0%

	GOTO 18290 IF COMP_STRING(AR_INVOICE(1%)::ARACCT, GL_WILDCARD$) = 0%

	!
	! Calculate invoice total
	!
	INVOICE_TOTAL = 0.0

	INVOICE_TOTAL = INVOICE_TOTAL + AR_INVOICE(I%)::SALAMT &
		FOR I% = 1% TO AR_INVOICE_TOT%

	INVOICE_TOTAL = INVOICE_TOTAL + AR_RECEIPT(I%)::SALAMT &
		FOR I% = 1% TO AR_RECEIPT_TOT%

	!
	! Print all the lines
	!
	FOR CURRENT_LINE% = 1% TO MAX_LINE%
		!
		! Skip to a new page if necessary
		!
		IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
				FOR I% = BODY_COUNT% + 1% TO &
				FORM_GROUP(FRM_BODY%)::NUMBER

			LINE_COUNT% = LINE_COUNT% + &
				FORM_GROUP(FRM_BODY%)::NUMBER

			!
			! Print the bottom of statement
			!
			LINE_COUNT% = LINE_COUNT% + &
				OUTP_FORMPRINT(UTL_REPORTX, &
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
		END IF

		!
		! Print one line (possibly two records)
		!
		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

	NEXT CURRENT_LINE%

18290	!
	! Reset for next group
	!
	AR_INVOICE_TOT% = 0%
	AR_RECEIPT_TOT% = 0%

	RETURN

	%PAGE

 LoadForm:
	!*******************************************************************
	! Initilize Statement form
	!*******************************************************************

	!
	! Get form from the AR form library
	!
	SMG_STATUS% = OUTP_FORMINIT(AR_FORM.DEV$ + "AR_FORM", REPORT$, &
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
			FRM_SUBBOTTOM% = I% IF FRM_SUBBOTTOM% = 0%

		CASE "FRM-SUBBOTTOM"
			FRM_SUBBOTTOM% = I%

		END SELECT

	NEXT I%

	RETURN

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

	!
	! Print the top of the stub
	!
	CURRENT_LINE%, MAX_LINE% = 1%
	AR_INVOICE_TOT% = 1%
	AR_RECEIPT_TOT% = 1%

	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%) &
		FOR I% = 1% TO FORM_GROUP(FRM_BODY%)::NUMBER

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

19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
 HelpError:
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
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN
	COM (AR_OPEN_ARRAY)	AR_OPEN_CDD		AR_INVOICE(800%), &
							AR_RECEIPT(800%)

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE) UTL_PROFILE_CDD UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION) UTL_LOCATION_CDD UTL_LOCATION
	MAP (UTL_MAIN_LOCATION) UTL_LOCATION_CDD UTL_MAIN_LOCATION

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
		AR_CUSTOM.ADDLINE$(4%) = 50%

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

	CASE "AR_CUSTOM:ADDLINE1"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(1%)

	CASE "AR_CUSTOM:ADDLINE2"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(2%)

	CASE "AR_CUSTOM:ADDLINE3"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(3%)

	CASE "AR_CUSTOM:ADDLINE4"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(4%)

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

	!************************************************************
	! Fields for the AR_OPEN file
	!************************************************************

	CASE "AR_INVOICE::CUSNUM"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::CUSNUM
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::CUSNUM
		END IF

	CASE "AR_INVOICE::INVNUM"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::INVNUM
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::INVNUM
		END IF

	CASE "AR_INVOICE::INVNUM1"
		IF CURRENT_LINE% <> 1%
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_INVOICE(1%)::INVNUM
		END IF

	CASE "AR_INVOICE::INVNUMX"
		IF CURRENT_LINE% <> MAX_LINE%
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_INVOICE(1%)::INVNUM
		END IF

	CASE "AR_INVOICE::TRATYP"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::TRATYP
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::TRATYP
		END IF

	CASE "AR_INVOICE::TRADAT"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = PRNT_DATE( &
				AR_RECEIPT(CURRENT_LINE%)::TRADAT, 6%)
		ELSE
			TEXTVALUE$ = PRNT_DATE( &
				AR_INVOICE(CURRENT_LINE%)::TRADAT, 6%)
		END IF

	CASE "AR_INVOICE::SALAMT"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = ""
			REALVALUE = 0.0
		ELSE
			TEXTVALUE$ = EDIT$(FORMAT$( &
				AR_INVOICE(CURRENT_LINE%)::SALAMT, &
				"<%>#######.##"), 8%)
			REALVALUE = AR_INVOICE(CURRENT_LINE%)::SALAMT
		END IF

	CASE "AR_INVOICE::DISAMT"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = ""
			REALVALUE = 0.0
		ELSE
			TEXTVALUE$ = EDIT$(FORMAT$( &
				AR_INVOICE(CURRENT_LINE%)::DISAMT, &
				"########.##"), 8%)
			REALVALUE = AR_INVOICE(CURRENT_LINE%)::DISAMT
		END IF

	CASE "AR_INVOICE::OTHCHG"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = ""
			REALVALUE = 0.0
		ELSE
			TEXTVALUE$ = EDIT$(FORMAT$( &
				AR_INVOICE(CURRENT_LINE%)::OTHCHG, &
				"########.##"), 8%)
			REALVALUE = AR_INVOICE(CURRENT_LINE%)::OTHCHG
		END IF

	CASE "AR_INVOICE::RECNUM"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::RECNUM
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::RECNUM
		END IF

	CASE "AR_INVOICE::CHKNUM"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::CHKNUM
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::CHKNUM
		END IF

	CASE "AR_INVOICE::ARACCT"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::ARACCT
		END IF

	CASE "AR_INVOICE::SUBACC"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::SUBACC
		END IF

	CASE "AR_INVOICE::DESCR"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::DESCR
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::DESCR
		END IF

	CASE "AR_INVOICE::BATCH"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::BATCH
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::BATCH
		END IF

	CASE "AR_INVOICE::UPDATED"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::UPDATED
		ELSE
			TEXTVALUE$ = AR_INVOICE(CURRENT_LINE%)::UPDATED
		END IF

	CASE "AR_INVOICE::CLOSEDATE"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = PRNT_DATE( &
				AR_INVOICE(CURRENT_LINE%)::CLOSEDATE, 6%)
		ELSE
			TEXTVALUE$ = PRNT_DATE( &
				AR_INVOICE(CURRENT_LINE%)::CLOSEDATE, 6%)
		END IF

	CASE "AR_INVOICE::DUEDATE"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = PRNT_DATE( &
				AR_INVOICE(CURRENT_LINE%)::DUEDATE, 6%)
		END IF

	CASE "AR_INVOICE::DISCOUNTDATE"
		IF (CURRENT_LINE% > AR_INVOICE_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = PRNT_DATE( &
				AR_INVOICE(CURRENT_LINE%)::DISCOUNTDATE, 6%)
		END IF

	!
	! Receipt subset
	!
	CASE "AR_RECEIPT::CUSNUM"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::CUSNUM
		END IF

	CASE "AR_RECEIPT::INVNUM"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::INVNUM
		END IF

	CASE "AR_RECEIPT::TRATYP"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::TRATYP
		END IF

	CASE "AR_RECEIPT::TRADAT"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = PRNT_DATE( &
				AR_RECEIPT(CURRENT_LINE%)::TRADAT, 6%)
		END IF

	CASE "AR_RECEIPT::SALAMT"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
			REALVALUE = 0.0
		ELSE
			TEXTVALUE$ = EDIT$(FORMAT$( &
				-AR_RECEIPT(CURRENT_LINE%)::SALAMT, &
				"<%>#######.##"), 8%)
			REALVALUE = AR_RECEIPT(CURRENT_LINE%)::SALAMT
		END IF

	CASE "AR_RECEIPT::DISAMT"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
			REALVALUE = 0.0
		ELSE
			TEXTVALUE$ = EDIT$(FORMAT$( &
				-AR_RECEIPT(CURRENT_LINE%)::DISAMT, &
				"########.##"), 8%)
			REALVALUE = AR_RECEIPT(CURRENT_LINE%)::DISAMT
		END IF

	CASE "AR_RECEIPT::OTHCHG"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
			REALVALUE = 0.0
		ELSE
			TEXTVALUE$ = EDIT$(FORMAT$( &
				-AR_RECEIPT(CURRENT_LINE%)::OTHCHG, &
				"########.##"), 8%)
			REALVALUE = AR_RECEIPT(CURRENT_LINE%)::OTHCHG
		END IF

	CASE "AR_RECEIPT::RECNUM"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::RECNUM
		END IF

	CASE "AR_RECEIPT::CHKNUM"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::CHKNUM
		END IF

	CASE "AR_RECEIPT::ARACCT"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::ARACCT
		END IF

	CASE "AR_RECEIPT::SUBACC"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::SUBACC
		END IF

	CASE "AR_RECEIPT::DESCR"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::DESCR
		END IF

	CASE "AR_RECEIPT::BATCH"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::BATCH
		END IF

	CASE "AR_RECEIPT::UPDATED"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = AR_RECEIPT(CURRENT_LINE%)::UPDATED
		END IF

	CASE "AR_RECEIPT::CLOSEDATE"
		IF (CURRENT_LINE% > AR_RECEIPT_TOT%)
		THEN
			TEXTVALUE$ = ""
		ELSE
			TEXTVALUE$ = PRNT_DATE( &
				AR_RECEIPT(CURRENT_LINE%)::CLOSEDATE, 6%)
		END IF

	CASE "INVOICE_TOTAL"
		IF CURRENT_LINE% = MAX_LINE%
		THEN
			TEXTVALUE$ = EDIT$(FORMAT$(INVOICE_TOTAL, &
				"########.##"), 8%)
		ELSE
			TEXTVALUE$ = ""
		END IF
		REALVALUE = INVOICE_TOTAL

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

	CASE "TOTAL_DISC"
		REALVALUE = TOTAL_DISC

	CASE "TOTAL_OTHER"
		REALVALUE = TOTAL_OTHER

	CASE "TOTAL_NET", "AMOUNT", "TOTAL_AMT"
		REALVALUE = AR_CUSBAL::CHARGE + AR_CUSBAL::FUTURE
		REALVALUE = REALVALUE + AR_CUSBAL::AGING(I%) &
			FOR I% = 0% TO 4%

	CASE "INVOICE_DATE"
		TEXTVALUE$ = PRNT_DATE(INVOICE_DATE$, 6%)

	CASE "BEGIN_BALANCE"
		REALVALUE = BEGIN_BALANCE

	CASE "PAGE_NUMBER"
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "TRAN_CODE"
		SELECT AR_INVOICE(CURRENT_LINE%)::TRATYP

		CASE "01"
			TEXTVALUE$ = "I"

		CASE "02"
			TEXTVALUE$ = "C"

		CASE "03"
			TEXTVALUE$ = "DM"

		CASE "04"
			TEXTVALUE$ = "SC"

		CASE "08"
			TEXTVALUE$ = "CM"

		CASE "11"
			TEXTVALUE$ = "AJ"

		CASE ELSE
			TEXTVALUE$ = "AC"

		END SELECT

	CASE "AR_CREDIT"
		CREDIT = 0.0
		CREDIT = AR_INVOICE(CURRENT_LINE%)::SALAMT &
			IF AR_INVOICE(CURRENT_LINE%)::SALAMT < 0.0
		CREDIT = CREDIT + AR_RECEIPT(CURRENT_LINE%)::SALAMT &
			IF AR_RECEIPT(CURRENT_LINE%)::SALAMT < 0.0
		TEXTVALUE$ = EDIT$(FORMAT$(CREDIT, "<%>#######.##"), 8%)

	CASE "AR_DEBIT"
		DEBIT = 0.0
		DEBIT = AR_INVOICE(CURRENT_LINE%)::SALAMT &
			IF AR_INVOICE(CURRENT_LINE%)::SALAMT > 0.0
		DEBIT = DEBIT + AR_RECEIPT(CURRENT_LINE%)::SALAMT &
			IF AR_RECEIPT(CURRENT_LINE%)::SALAMT > 0.0
		TEXTVALUE$ = EDIT$(FORMAT$(DEBIT, "<%>#######.##"), 8%)

	!*******************************************************************
	! PROFILE
	!*******************************************************************

	CASE "UTL_PROFILE::MENU_NAME"
		TEXTVALUE$ = UTL_PROFILE::MENU_NAME

	CASE "UTL_PROFILE::REP_NAME"
		TEXTVALUE$ = UTL_PROFILE::REP_NAME

	CASE "UTL_PROFILE::MAINLOCATION"
		TEXTVALUE$ = UTL_PROFILE::MAINLOCATION

	CASE "UTL_PROFILE::DEFLOCATION"
		TEXTVALUE$ = UTL_PROFILE::DEFLOCATION

	!*******************************************************************
	! Location info
	!*******************************************************************

	CASE "UTL_LOCATION::LOCATION"
		TEXTVALUE$ = UTL_MAIN_LOCATION::LOCATION

	CASE "UTL_LOCATION::LOCNAME"
		TEXTVALUE$ = UTL_MAIN_LOCATION::LOCNAME

	CASE "UTL_LOCATION::REGION"
		TEXTVALUE$ = UTL_MAIN_LOCATION::REGION

	CASE "UTL_LOCATION::LOCGROUP"
		TEXTVALUE$ = UTL_MAIN_LOCATION::LOCGROUP

	CASE "UTL_LOCATION::ADDRESS1"
		TEXTVALUE$ = UTL_MAIN_LOCATION::ADDRESS1

	CASE "UTL_LOCATION::ADDRESS2"
		TEXTVALUE$ = UTL_MAIN_LOCATION::ADDRESS2

	CASE "UTL_LOCATION::CITY"
		TEXTVALUE$ = UTL_MAIN_LOCATION::CITY

	CASE "UTL_LOCATION::STATE"
		TEXTVALUE$ = UTL_MAIN_LOCATION::STATE

	CASE "UTL_LOCATION::ZIP"
		TEXTVALUE$ = UTL_MAIN_LOCATION::ZIP

	CASE "UTL_LOCATION::COUNTY"
		TEXTVALUE$ = UTL_MAIN_LOCATION::COUNTY

	CASE "UTL_LOCATION::COUNTRY"
		TEXTVALUE$ = UTL_MAIN_LOCATION::COUNTRY

	CASE "UTL_LOCATION::PHONE"
		TEXTVALUE$ = UTL_MAIN_LOCATION::PHONE

	CASE "UTL_LOCATION::SHPADDRESS1"
		TEXTVALUE$ = UTL_MAIN_LOCATION::SHPADDRESS1

	CASE "UTL_LOCATION::SHPADDRESS2"
		TEXTVALUE$ = UTL_MAIN_LOCATION::SHPADDRESS2

	CASE "UTL_LOCATION::SHPCITY"
		TEXTVALUE$ = UTL_MAIN_LOCATION::SHPCITY

	CASE "UTL_LOCATION::SHPSTATE"
		TEXTVALUE$ = UTL_MAIN_LOCATION::SHPSTATE

	CASE "UTL_LOCATION::SHPZIP"
		TEXTVALUE$ = UTL_MAIN_LOCATION::SHPZIP

	CASE "UTL_LOCATION::SHPCOUNTY"
		TEXTVALUE$ = UTL_MAIN_LOCATION::SHPCOUNTY

	CASE "UTL_LOCATION::SHPCOUNTRY"
		TEXTVALUE$ = UTL_MAIN_LOCATION::SHPCOUNTRY

	CASE "UTL_LOCATION::SHPPHONE"
		TEXTVALUE$ = UTL_MAIN_LOCATION::SHPPHONE

	!*******************************************************************
	! Location info
	!*******************************************************************

	CASE "UTL_CUS_LOCATION::LOCATION"
		TEXTVALUE$ = UTL_LOCATION::LOCATION

	CASE "UTL_CUS_LOCATION::LOCNAME"
		TEXTVALUE$ = UTL_LOCATION::LOCNAME

	CASE "UTL_CUS_LOCATION::REGION"
		TEXTVALUE$ = UTL_LOCATION::REGION

	CASE "UTL_CUS_LOCATION::LOCGROUP"
		TEXTVALUE$ = UTL_LOCATION::LOCGROUP

	CASE "UTL_CUS_LOCATION::ADDRESS1"
		TEXTVALUE$ = UTL_LOCATION::ADDRESS1

	CASE "UTL_CUS_LOCATION::ADDRESS2"
		TEXTVALUE$ = UTL_LOCATION::ADDRESS2

	CASE "UTL_CUS_LOCATION::CITY"
		TEXTVALUE$ = UTL_LOCATION::CITY

	CASE "UTL_CUS_LOCATION::STATE"
		TEXTVALUE$ = UTL_LOCATION::STATE

	CASE "UTL_CUS_LOCATION::ZIP"
		TEXTVALUE$ = UTL_LOCATION::ZIP

	CASE "UTL_CUS_LOCATION::COUNTY"
		TEXTVALUE$ = UTL_LOCATION::COUNTY

	CASE "UTL_CUS_LOCATION::COUNTRY"
		TEXTVALUE$ = UTL_LOCATION::COUNTRY

	CASE "UTL_CUS_LOCATION::PHONE"
		TEXTVALUE$ = UTL_LOCATION::PHONE

	CASE "UTL_CUS_LOCATION::SHPADDRESS1"
		TEXTVALUE$ = UTL_LOCATION::SHPADDRESS1

	CASE "UTL_CUS_LOCATION::SHPADDRESS2"
		TEXTVALUE$ = UTL_LOCATION::SHPADDRESS2

	CASE "UTL_CUS_LOCATION::SHPCITY"
		TEXTVALUE$ = UTL_LOCATION::SHPCITY

	CASE "UTL_CUS_LOCATION::SHPSTATE"
		TEXTVALUE$ = UTL_LOCATION::SHPSTATE

	CASE "UTL_CUS_LOCATION::SHPZIP"
		TEXTVALUE$ = UTL_LOCATION::SHPZIP

	CASE "UTL_CUS_LOCATION::SHPCOUNTY"
		TEXTVALUE$ = UTL_LOCATION::SHPCOUNTY

	CASE "UTL_CUS_LOCATION::SHPCOUNTRY"
		TEXTVALUE$ = UTL_LOCATION::SHPCOUNTRY

	CASE "UTL_CUS_LOCATION::SHPPHONE"
		TEXTVALUE$ = UTL_LOCATION::SHPPHONE

	END SELECT

	END SUB
