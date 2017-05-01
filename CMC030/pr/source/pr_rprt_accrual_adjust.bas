1	%TITLE "REPORT - Accrual Adjustments Report"
	%SBTTL "PR_RPRT_ACCRUAL_ADJUST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:PR060
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Accrual Adjustments Report\* option prints the
	!	report which includes any accruals adjustments calculated during the execution
	!	of the ^*Accrual Adjustments\* option.
	!
	! Index:
	!	.x Report>Accrual Adjustments
	!	.x Accrual Adjustments>Report
	!
	! Option:
	!
	! Author:
	!
	!	11/30/92 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_ACCRUAL_ADJUST
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_ACCRUAL_ADJUST, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_ACCRUAL_ADJUST.OBJ;*
	!
	! Modification history:
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/30/92 - Kevin Handy
	!		Fixed bug when printing by location would lock up.
	!
	!	05/05/93 - Kevin Handy
	!		Modified to print all accrual codes, by creating
	!		a sort file, and then dumpint out the sort file.
	!
	!	05/10/93 - Kevin Handy
	!		Fix total (dollar ava) so that it contains more
	!		than just the last employee printed.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/22/2000 - Kevin Handy
	!		Add EFF_DATE to PR_READ_RATE
	!
	!	07/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/16/2001 - Kevin Handy
	!		Look for a specific pay rate, before trying a generic
	!		one. (marco)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP	(PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	RECORD PR_TEMP_CDD
		STRING	CODE = 2%
		STRING	SORTBY = 30%
		STRING	EMPNUM = 10%
		STRING	EMPNAME = 30%
		REAL	HOURSUNA
		REAL	DOLLARUNA
		REAL	CHANGE_UNA
		REAL	HOURSAVA
		REAL	DOLLARAVA
		REAL	CHANGE_AVA
		STRING	DEBIT_ACCOUNT = 18%
		STRING	CREDIT_ACCOUNT = 18%
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	!
	! Dimension Statements
	!
	DIM SUBTOTAL(9%), TOTAL(9%)

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field specifies which value the
	!	report is to begin with. The item must be in agreement
	!	with the specific type of sort selected, i.e., if the selection is
	!	made to print the report in employee number order, a value in this
	!	field must be an employee number; or if the selection is made to
	!	print the report in alphabetical order, the value in this field
	!	must be a name, last name first.
	!	.b
	!	Leaving this field blank will cause the report to begin with the
	!	first record.
	!	.b
	!	Refer to field (03) Order of the Accrual Report report
	!	setting screen for more information on sort choices.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Accrual Report
	!	.x Accrual Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.lm +5
	!	.b
	!	The ^*To Item\* field specifies a value to end the report with.
	!	The item must be in agreement with the specific type of sort
	!	selected, i.e., if the selection is made to print the report in employee number
	!	order, a value in this field must be an employee number; or if the select is
	!	made to print the report in alphabetical order, the value in this field must
	!	be a name, last name first.
	!	.b
	!	Leaving this field blank will cause the report to end with the last record.
	!	.b
	!	Refer to field (03) Order of the Accrual Report report setting screen for
	!	more information on sort choices.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Accrual Adjustment Report
	!	.x Accrual Adjustment Report>To Item
	!
	!--
	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU,NA,SN,LO,SO)\*
	!	.p
	!	The ^*Sort\* field determines the order in
	!	which the report will print.
	!	.p
	!	The valid values and related sort orders are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	SN = Social Security Number
	!	.le
	!	LO = Location, Department, Work Center, Alpha
	!	.le
	!	SO = Alphabetical (last name first)
	!	.els
	!	.lm -10
	!	.p
	!	This field requires an entry. Only the codes listed above are
	!	valid.
	!
	! Index:
	!	.x Sort>Accrual Report
	!	.x Accrual Report>Sort
	!
	!--

	ACCCODE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Accrual Code Wildcard\*
	!	.lm +5
	!	.b
	!	The ^*Accrual Code Wildcard\*
	!	refers to a user defined code as recorded in the Pay
	!	Accrue Deduct Code Definition file in the Master Table menu.
	!	.b
	!	For example, if the user had defined the vacation benefit as "VA", and wanted
	!	a report which would display vacation accrual adjustments, "VA" would be
	!	entered in this field.  If all benefit accrual adjustments were to be printed,
	!	an "_*" could be entered in this field.
	!	.lm -5
	!
	! Index:
	!	.x Accrual Adjustment Report>Accrual Code
	!	.x Accrual Code>Accrual Adjustment Report
	!
	!--

	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))

	CASE "SN"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SSN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SSN))

	CASE "LO"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::LOCATION))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::LOCATION))

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))

	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

330	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
	USE
		FILENAME$ = "PR_EMP_ACCRUAL"
		CONTINUE HelpError
	END WHEN

340	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

350	!
	! Open up sort file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

	OPEN "PR_TEMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		BUFFER 32%, &
		MAP PR_TEMP, &
		PRIMARY KEY (PR_TEMP::CODE, &
			PR_TEMP::SORTBY) DUPLICATES, &
		ACCESS MODIFY, &
		ALLOW NONE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "EMPLOYEE ACCRUAL ADJUSTMENT REPORT"
	TITLE$(2%) = "Payroll System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "                                            " + &
		"----- Unavailable -----  " + &
		"  ----- Available -----"

	TITLE$(5%) = "Emp Num   Employee Name                     " + &
		"Hours  Dollars  Change  " + &
		" Hours  Dollars  Change   Debit Account      Credit Account"


	TITLE$(6%) = "."

	LYT_LINE$ = "$EMPNUM:11,$EMPNAM:42,FUNHRS:52,FUNDAY:62,FUNDOL:72," + &
		"FAVHRS:52,FAVDAY:62,FAVDOL:72,FTTHRS:52,FTTDAY:62,FTTDOL:72"
	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	!
	! Fake out some entries
	!
	BATCH_NO$ = DATE_TODAY

	WHEN ERROR IN
		RESET #PR_EMP_ACCRUAL.CH%
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	SUBITEM$ = "--------------------"

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_ACCRUAL.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	IF ACCCODE$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(PR_EMP_ACCRUAL::ATYPE, ACCCODE$) = 0%
	END IF

17025	!
	! Get employee record
	!
	IF PR_EMP_MASTER::EMPNUM <> PR_EMP_ACCRUAL::EMPNUM
	THEN
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_EMP_ACCRUAL::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 17020
		END WHEN
	END IF

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "NU"
		GOTO GetNextRec IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF (PR_EMP_MASTER::EMPNUM < FROM_ITEM$)

	CASE "NA"
		GOTO GetNextRec IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF (PR_EMP_MASTER::EMPNAME < FROM_ITEM$)

	CASE "SN"
		GOTO GetNextRec IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF (PR_EMP_MASTER::SSN < FROM_ITEM$)

	CASE "LO"
		GOTO GetNextRec IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF (PR_EMP_MASTER::LOCATION < FROM_ITEM$)

	CASE ELSE
		GOTO GetNextRec IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF (PR_EMP_MASTER::SORT < FROM_ITEM$)

	END SELECT

	!
	! Look up current rate
	!
	!
	! Try for a specific rate based on the pay code first
	!
	CALL PR_READ_RATE_CODE(PR_EMP_MASTER::EMPNUM, &
		PR_EMP_MASTER::OPER, &
		BATCH_NO$, &
		RATE_TYPE$, &
		PR_EMP_ACCRUAL::ATYPE, &
		HOUR_RATE, &
		PIECE_RATE, &
		FACTOR%, &
		STDEFF, &
		EVALDATE$, &
		EFF_DATE$)

	!
	! Try for a generic rate if a specific rate isn't found
	!
	IF HOUR_RATE = 0.0
	THEN
		CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
			PR_EMP_MASTER::OPER, &
			BATCH_NO$, &
			RATE_TYPE$, &
			RATE_CDE$, &
			HOUR_RATE, &
			PIECE_RATE, &
			FACTOR%, &
			STDEFF, &
			EVALDATE$, &
			EFF_DATE$)
	END IF

	!
	! Calculate any changes
	!
	CHANGE_UNA = FUNC_ROUND(PR_EMP_ACCRUAL::HOURSUNA * HOUR_RATE, 2%) - &
		PR_EMP_ACCRUAL::DOLLARUNA
	CHANGE_AVA = FUNC_ROUND(PR_EMP_ACCRUAL::HOURSAVA * HOUR_RATE, 2%) - &
		PR_EMP_ACCRUAL::DOLLARAVA

17050	!
	! Lookup definition of rate
	!
	IF (PR_ERNDED_DEF::ETYPE <> "P") OR &
		(PR_ERNDED_DEF::CODE <> PR_EMP_ACCRUAL::ATYPE)
	THEN
		!
		! Look up ernded definition file
		!
		WHEN ERROR IN
			GET #PR_ERNDED_DEF.CH%, &
				KEY #0% EQ "P" + PR_EMP_ACCRUAL::ATYPE, &
				REGARDLESS
		USE
			PR_ERNDED_DEF::ETYPE = "P"
			PR_ERNDED_DEF::CODE = PR_EMP_ACCRUAL::ATYPE

			PR_ERNDED_DEF::DESCR = &
				STRING$(LEN(PR_ERNDED_DEF::DESCR), 63%)
			PR_ERNDED_DEF::DRCR_ACCT = &
				STRING$(LEN(PR_ERNDED_DEF::DRCR_ACCT), 63%)
			PR_ERNDED_DEF::ACCRUAL_ACCT = &
				STRING$(LEN(PR_ERNDED_DEF::ACCRUAL_ACCT), 63%)

			CONTINUE 17060
		END WHEN
	END IF

17060	!
	! Set sortby field
	!
	SELECT SORTBY$
	CASE "NU"
		PR_TEMP::SORTBY = PR_EMP_MASTER::EMPNUM
	CASE "NA"
		PR_TEMP::SORTBY = PR_EMP_MASTER::EMPNAME
	CASE "SN"
		PR_TEMP::SORTBY = PR_EMP_MASTER::SSN
	CASE "LO"
		PR_TEMP::SORTBY = PR_EMP_MASTER::LOCATION
	CASE ELSE
		PR_TEMP::SORTBY = PR_EMP_MASTER::SORT
	END SELECT

	!
	! Calculate debit/credit accounts
	!
	CALL GL_ASSG_ACCMASK(PR_ERNDED_DEF::ACCRUAL_ACCT, &
		PR_EMP_MASTER::ACCT, &
		CREDIT_ACCOUNT$)

	CALL GL_ASSG_ACCMASK(PR_ERNDED_DEF::DRCR_ACCT, &
		PR_EMP_MASTER::ACCT, &
		DEBIT_ACCOUNT$)

	!
	! Create temp record
	!
	PR_TEMP::CODE		= PR_EMP_ACCRUAL::ATYPE
	PR_TEMP::EMPNUM		= PR_EMP_MASTER::EMPNUM
	PR_TEMP::EMPNAME	= PR_EMP_MASTER::EMPNAME
	PR_TEMP::HOURSUNA	= PR_EMP_ACCRUAL::HOURSUNA
	PR_TEMP::DOLLARUNA	= PR_EMP_ACCRUAL::DOLLARUNA
	PR_TEMP::CHANGE_UNA	= CHANGE_UNA
	PR_TEMP::HOURSAVA	= PR_EMP_ACCRUAL::HOURSAVA
	PR_TEMP::DOLLARAVA	= PR_EMP_ACCRUAL::DOLLARAVA
	PR_TEMP::CHANGE_AVA	= CHANGE_AVA
	PR_TEMP::DEBIT_ACCOUNT	= DEBIT_ACCOUNT$
	PR_TEMP::CREDIT_ACCOUNT	= CREDIT_ACCOUNT$

	PUT #PR_TEMP.CH%

	GOTO GetNextRec

	%PAGE

 ExitTotal:

17100	!
	! Dump out temp file
	!
	WHEN ERROR IN
		RESET #PR_TEMP.CH%
	USE
		CONTINUE ExitTotalFinal
	END WHEN

	CODE_LAST$ = "~~~"

17110	WHEN ERROR IN
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotalFinal
	END WHEN

	IF (CODE_LAST$ <> PR_TEMP::CODE)
	THEN
		IF (SORTBY$ = "LO") AND (SUBITEM$ <> PR_TEMP::SORTBY)
		THEN
			SUBTITLE$ = "Location " + PR_TEMP::SORTBY + " Subtotal"
			GOSUB Subtotal
			SUBITEM$ = PR_TEMP::SORTBY
		END IF

		GOSUB Total
	END IF

	IF (SORTBY$ = "LO") AND (SUBITEM$ <> PR_TEMP::SORTBY)
	THEN
		SUBTITLE$ = "Location " + PR_TEMP::SORTBY + " Subtotal"
		GOSUB Subtotal
		SUBITEM$ = PR_TEMP::SORTBY
	END IF

	!
	! Print out one line
	!
	TEXT$ = PR_TEMP::EMPNUM + " " + &
		PR_TEMP::EMPNAME + &
		FORMAT$(PR_TEMP::HOURSUNA, "######.#") + &
		FORMAT$(PR_TEMP::DOLLARUNA, "######.##") + &
		FORMAT$(PR_TEMP::CHANGE_UNA, "<%>#####.##") + &
		FORMAT$(PR_TEMP::HOURSAVA, "######.#") + &
		FORMAT$(PR_TEMP::DOLLARAVA, "######.##") + &
		FORMAT$(PR_TEMP::CHANGE_AVA, "<%>#####.## ") + &
		PR_TEMP::DEBIT_ACCOUNT + " " + &
		PR_TEMP::CREDIT_ACCOUNT

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Subtotal
	!
	SUBTOTAL(1%) = SUBTOTAL(1%) + PR_TEMP::HOURSUNA
	SUBTOTAL(2%) = SUBTOTAL(2%) + PR_TEMP::DOLLARUNA
	SUBTOTAL(3%) = SUBTOTAL(3%) + PR_TEMP::CHANGE_UNA
	SUBTOTAL(4%) = SUBTOTAL(4%) + PR_TEMP::HOURSAVA
	SUBTOTAL(5%) = SUBTOTAL(5%) + PR_TEMP::DOLLARAVA
	SUBTOTAL(6%) = SUBTOTAL(6%) + PR_TEMP::CHANGE_AVA

	!
	! Total
	!
	TOTAL(1%) = TOTAL(1%) + PR_TEMP::HOURSUNA
	TOTAL(2%) = TOTAL(2%) + PR_TEMP::DOLLARUNA
	TOTAL(3%) = TOTAL(3%) + PR_TEMP::CHANGE_UNA
	TOTAL(4%) = TOTAL(4%) + PR_TEMP::HOURSAVA
	TOTAL(5%) = TOTAL(5%) + PR_TEMP::DOLLARAVA
	TOTAL(6%) = TOTAL(6%) + PR_TEMP::CHANGE_AVA

	GOTO 17110

 ExitTotalFinal:
	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "LO"
		SUBTITLE$ = "Location " + SUBITEM$ + " Subtotal"
		GOSUB Subtotal
		SUBITEM$ = PR_EMP_MASTER::LOCATION

	END SELECT

	GOSUB Total

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 Subtotal:
	!*******************************************************************
	! Print subtotals
	!*******************************************************************

	I% = 0%
	I% = -1% IF SUBTOTAL(J%) <> 0.0 FOR J% = 1% TO 9%

	IF I%
	THEN
		TEXT$ = "          " + " " + &
			FORMAT$(SUBTITLE$, "'LLLLLLLLLLLLLLLLLLLLLLLLLLLLL") + &
			FORMAT$(SUBTOTAL(1%), "######.#") + &
			FORMAT$(SUBTOTAL(2%), "######.##") + &
			FORMAT$(SUBTOTAL(3%), "######.##") + &
			FORMAT$(SUBTOTAL(4%), "######.#") + &
			FORMAT$(SUBTOTAL(5%), "######.##") + &
			FORMAT$(SUBTOTAL(6%), "######.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	SUBTOTAL(I%) = 0.0 FOR I% = 1% TO 6%

	RETURN

 Total:
	!*******************************************************************
	! Print subtotals
	!*******************************************************************

	IF CODE_LAST$ <> "~~~"
	THEN
		TEXT$ = "           " + &
			"Grand Total for " + &
			CODE_LAST$ + &
			"            " + &
			FORMAT$(TOTAL(1%), "######.#") + &
			FORMAT$(TOTAL(2%), "######.##") + &
			FORMAT$(TOTAL(3%), "######.##") + &
			FORMAT$(TOTAL(4%), "######.#") + &
			FORMAT$(TOTAL(5%), "######.##") + &
			FORMAT$(TOTAL(6%), "######.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -2%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	CODE_LAST$ = PR_TEMP::CODE + ""

	TOTAL(I%) = 0.0 FOR I% = 1% TO 6%

	RETURN

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
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
