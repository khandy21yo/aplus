1	%TITLE "Calculate Service Charges"
	%SBTTL "AR_SPEC_SERCHRG"
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
	! ID:ARSCCL
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function calculates service charges
	!	for all designated accounts receivable customers who have past
	!	due amounts at the end of a specified month.
	!	.b
	!	The percentage of monthly service charge to calculate on the
	!	past due accounts is determined in the ^*Service Charge Table\*.
	!	This percentage must be established in order for the service
	!	charge to be calculated.
	!	.b
	!	It is recommended that before the service charges are calculated,
	!	an Accounts Receivable Register be printed making certain the
	!	register reflects the proper balances.
	!	.b
	!	^*Note:\*  Service charges must be calculated before closing the Accounts
	!	Receivable Ledger for a month.
	!	.lm -5
	!
	! Index:
	!	.x Service Charge>Calculate
	!	.x Calculate>Service Charge
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_SERCHRG/LINE
	!	$ LINK/EXE=AR_EXE: AR_SPEC_SERCHRG, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_SERCHRG.OBJ;*
	!
	!
	! Author:
	!
	!	03/08/88 - Kevin Handy
	!
	! Modification history:
	!
	!	04/25/88 - Kevin Handy
	!		Fixed problem where a customer without a country
	!		didn't get charged service charge.  Forced the
	!		country to "US" (United States Bias).
	!
	!	04/27/88 - Kevin Handy
	!		Changed from asking for CUTOFF DATE to ask for
	!		CUTOFF PERIOD, which is what it actually is.
	!
	!	04/29/88 - Kevin Handy
	!		Fixed bug in summary.
	!
	!	06/06/91 - Kevin Handy
	!		Removed junk from error trapping.
	!
	!	10/28/91 - Kevin Handy
	!		Modified to blank subaccount instead of putting
	!		the customer number in there.
	!
	!	08/26/92 - Dan Perkins
	!		Tell AR_FUNC_AGE we want to calculate service charges
	!		by passing -1% through arguement NUM_ACCT%.  Don't
	!		calculate a service charge if the balance or AMOUNT_DUE
	!		is less than 0. (zero).
	!
	!	08/31/93 - Kevin Handy
	!		Add initilization to fields DUEDATE, DISCOUNTDATE,
	!		and SUBACCT.
	!
	!	11/04/93 - Kevin Handy
	!		Modified to have a input field for INVOICE_DATE$
	!		instead of using BASE_DAY$.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/14/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	MAP (UTL_REPORTX)	UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_SERCHG.HB"
	MAP (AR_SERCHG)		AR_SERCHG_CDD		AR_SERCHG

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL

	%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.HB"
	MAP (AR_SJH)		AR_SJH_CDD		AR_SJH

	%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.HB"
	MAP (AR_SJL)		AR_SJL_CDD		AR_SJL

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	COM (CH_AR_35CUSTOM) AR_35CUSTOM.CH%
	COM (CH_AR_CUSBAL) AR_CUSBAL.CH%
	COM (CH_AR_OPEN)   AR_OPEN.CH%
	COM (CH_AR_SJH)    AR_SJH.CH%
	COM (CH_AR_SJL)    AR_SJL.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION AR_FUNC_AGE

	!
	! Dimensions
	!
	DECLARE INTEGER CONSTANT MAX_CUSBAL = 100%
	DIM AR_CUSBAL_CDD ARRAY_CUSBAL(MAX_CUSBAL)

	DIM SUMMARY_H$(MAX_CUSBAL), SUMMARY_H(MAX_CUSBAL)
	DIM SUMMARY_L$(MAX_CUSBAL), SUMMARY_L(MAX_CUSBAL)

	%PAGE

	ON ERROR GOTO 19000

	CALL READ_INITIALIZE

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	BASE_DAY$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(0%), 128%))
	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Base Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Base Date\* field refers to the first day
	!	to be considered for the service charge calculation.
	!	.lm -5
	!
	! Index:
	!	.x Base Date>Calculate Service Charge
	!	.x Calculate Service Charge>Base Date
	!
	!--

	CUTOFF$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(1%), 128%), 6%)
	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(02) Cutoff Period	YYYYPP\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Period\* field allows for entry of the last day
	!	to be considered for the service charge calculation.
	!	.lm -5
	!
	! Index:
	!	.x Cutoff Period>Service Charge
	!	.x Service Charge>Cutoff Period
	!
	!--

	REF_NO$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 128%)

	!++
	! Abstract:FLD03
	!	^*(03) Starting Reference\*
	!	.b
	!	.lm +5
	!	The ^*Starting Reference\* field defines the Service
	!	Charge Invoice number to assign to the first service charge entry
	!	made to the sales journal. This number is system incremented by one
	!	number after each invoice is created.
	!	.lm -5
	!
	! Index:
	!	.x Starting Reference>Calculate Service Charge
	!	.x Calculate Service Charge>Starting Reference
	!
	!--

	INVOICE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(3%), 128%))
	INVOICE_DATE$ = BASE_DAY$ IF INVOICE_DATE$ = ""

	!++
	! Abstract:FLD04
	!	^*(03) Invoice Date\*
	!	.b
	!	.lm +5
	!	This field defines the invoice date posted on
	!	the service charge records that are created.
	!	A blank entry here will cause the base date to be
	!	used.
	!	.lm -5
	!
	! Index:
	!	.x Starting Reference>Calculate Service Charge
	!	.x Calculate Service Charge>Starting Reference
	!
	!--

	BATCH_NO$ = "SC"

	CALL READ_DEVICE("AR_SJH", AR_SJH.DEV$, STAT%)
	CALL READ_DEVICE("AR_SJL", AR_SJL.DEV$, STAT%)

200	!
 !	KILL AR_SJH.DEV$ + "AR_SJH_" + BATCH_NO$ + ".JRL" FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(AR_SJH.DEV$ + "AR_SJH_" + &
		BATCH_NO$ + ".JRL;*")

210 !	KILL AR_SJL.DEV$ + "AR_SJL_" + BATCH_NO$ + ".JRL" FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(AR_SJL.DEV$ + "AR_SJL_" + &
		BATCH_NO$ + ".JRL;*")

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

320	!
	! Open service charge file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_SERCHG.OPN"
	USE
		FILENAME$ = "AR_SERCHG"
		CONTINUE HelpError
	END WHEN

330	!
	! Open register file
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"

350	!
	! Open customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

360	!
	! Open customer balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

370	!
	! Open sales journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.CRE"
	USE
		FILENAME$ = "AR_SJH"
		CONTINUE HelpError
	END WHEN

380	!
	! Open sales journal line item file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.CRE"
	USE
		FILENAME$ = "AR_SJL"
		CONTINUE HelpError
	END WHEN

	%PAGE

17000	!******************************************************************
	! Process
	!******************************************************************

	RESET #AR_35CUSTOM.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Calculating...", 1%)

 GetNextRec:
17020	!
	! Get a customer
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE 17600 IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Skip if flagged no service charge
	!
	GOTO 17020 IF AR_35CUSTOM::SERCHRG = "N"

	!
	! Tell them who we are working on
	!
	CALL ENTR_3MESSAGE(SCOPE, "Calculating...  " + AR_35CUSTOM::CUSNUM, 1%)

	!*******************************************************************
	! Calculate customer balances by account numbers
	!*******************************************************************

	!
	! Tell AR_FUNC_AGE that we want to calculate service charges
	!
	NUM_ACCT% = -1%

	IF AR_FUNC_AGE(AR_35CUSTOM::CUSNUM, AR_35CUSTOM::METHOD, &
		BASE_DAY$, CUTOFF$, &
		NUM_ACCT%, ARRAY_CUSBAL())
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Error aging " + AR_35CUSTOM::CUSNUM, 0%)
		GOTO ExitProgram
	END IF

	%PAGE

 CalcServ:
17400	!******************************************************************
	! Calculate service charges
	!******************************************************************

	!
	! Loop through all accumulated accounts
	!
	FOR LOOP% = 1% TO NUM_ACCT%

17410		!
		! Scan for an exact match on service charge
		!
		AR_35CUSTOM::COUNTRY = "US" IF AR_35CUSTOM::COUNTRY = ""

		WHEN ERROR IN
			GET #AR_SERCHG.CH%, &
				KEY #0% EQ AR_35CUSTOM::COUNTRY + &
				AR_35CUSTOM::STATE + &
				ARRAY_CUSBAL(LOOP%)::ACCT, &
				REGARDLESS
		USE
			CONTINUE 17415
		END WHEN

		GOTO 17440

17415		!
		! Scan for a match for country only on service charge
		!
		WHEN ERROR IN
			GET #AR_SERCHG.CH%, &
				KEY #0% EQ AR_35CUSTOM::COUNTRY + &
				"  " + &
				ARRAY_CUSBAL(LOOP%)::ACCT, &
				REGARDLESS
		USE
			CONTINUE 17490
		END WHEN

		GOTO 17440

17440		!
		! Calculate amount past due as everything except
		! the current amount.
		!
		AMOUNT_DUE = 0.0
		AMOUNT_DUE = AMOUNT_DUE + ARRAY_CUSBAL(LOOP%)::AGING(LOOP1%) &
			FOR LOOP1% = 1% TO 4%

		!
		! Forget the service charge if there is a credit
		!
		GOTO 17490 IF AMOUNT_DUE < 0.0

		!
		! Check against minimum amount
		!
		GOTO 17490 IF AMOUNT_DUE < AR_SERCHG::DOLLAR

		!
		! Calculate service charge
		!
		SC_AMT = FUNC_ROUND(AMOUNT_DUE * AR_SERCHG::SERCHG / 100.0, 2%)
		SC_AMT = AR_SERCHG::MINIMUM IF SC_AMT < AR_SERCHG::MINIMUM

		!
		! Create SJ header record for service charge
		!
		AR_SJH::INVNUM	= REF_NO$

		STAT% = FUNC_INCREMENT(REF_NO$)		! Increment invoice number

		AR_SJH::CUSNUM	= AR_35CUSTOM::CUSNUM	! Customer number
		AR_SJH::TRATYP	= "04"			! Service charge
		AR_SJH::TRADAT	= INVOICE_DATE$		! Transaction Date
		AR_SJH::AMOUNT	= SC_AMT		! Sales amount
		AR_SJH::ARACCT	= AR_SERCHG::ACCT	! GL number
		AR_SJH::RECNUM	= ""			! Receipt number
		AR_SJH::CHECK	= ""			! Check number
		AR_SJH::DEPOSIT	= ""			! Deposit number
		AR_SJH::DESCR	= "Service Charge"	! Description

		AR_SJH::DUEDATE = ""			! Due Date
		AR_SJH::DISCOUNTDATE = ""		! Discount Date
		AR_SJH::SUBACCT = ""			! Sub account

		!
		! Create SJ line item record
		!
		AR_SJL::INVNUM	= AR_SJH::INVNUM	! Invoice number
		AR_SJL::SLINE	= "001"			! Line number
		AR_SJL::ACCT	= AR_SERCHG::SCREV	! Account number
		AR_SJL::SUBACCT	= ""			! Sub Account
		AR_SJL::LTYPE	= "1"			! Invoice type
		AR_SJL::DESCR	= "Service Charge"	! Description
		AR_SJL::AMOUNT	= -SC_AMT		! Amount
		AR_SJL::QTY	= 0.0			! Quantity
		AR_SJL::TAXTYP	= ""			! Tax Type

17450		!
		! Put SJH record to file
		!
		PUT #AR_SJH.CH%

17460		!
		! Put SJL record to file
		!
		PUT #AR_SJL.CH%

17470		!
		! Store header totals by account for summary
		!
		FOR LOOP1% = 1% TO SUMMARY_H%
			GOTO 17476 IF SUMMARY_H$(LOOP1%) = AR_SERCHG::ACCT
			GOTO 17473 IF SUMMARY_H$(LOOP1%) > AR_SERCHG::ACCT
		NEXT LOOP1%
		LOOP1% = SUMMARY_H% + 1%

17473		FOR LOOP2% = SUMMARY_H% TO LOOP1% STEP -1%
			SUMMARY_H$(LOOP2% + 1%) = SUMMARY_H$(LOOP2%)
			SUMMARY_H(LOOP2% + 1%)  = SUMMARY_H(LOOP2%)
		NEXT LOOP2%

		SUMMARY_H$(LOOP1%) = AR_SERCHG::ACCT
		SUMMARY_H(LOOP1%) = 0.0
		SUMMARY_H% = SUMMARY_H% + 1%

17476		SUMMARY_H(LOOP1%) = SUMMARY_H(LOOP1%) + SC_AMT

17480		!
		! Store header totals by account for summary
		!
		FOR LOOP1% = 1% TO SUMMARY_L%
			GOTO 17486 IF SUMMARY_L$(LOOP1%) = AR_SERCHG::SCREV
			GOTO 17483 IF SUMMARY_L$(LOOP1%) > AR_SERCHG::SCREV
		NEXT LOOP1%
		LOOP1% = SUMMARY_L% + 1%

17483		FOR LOOP2% = SUMMARY_L% TO LOOP1% STEP -1%
			SUMMARY_L$(LOOP2% + 1%) = SUMMARY_L$(LOOP2%)
			SUMMARY_L(LOOP2% + 1%)  = SUMMARY_L(LOOP2%)
		NEXT LOOP2%

		SUMMARY_L$(LOOP1%) = AR_SERCHG::SCREV
		SUMMARY_L(LOOP1%) = 0.0
		SUMMARY_L% = SUMMARY_L% + 1%

17486		SUMMARY_L(LOOP1%) = SUMMARY_L(LOOP1%) + SC_AMT

17490	NEXT LOOP%

17500	!
	! Loop back around for another customer
	!
	GOTO GetNextRec

17600	!*******************************************************************
	! Print out summary
	!*******************************************************************

	!
	! Set up header
	!
	TITLE$(1%) = "Service Charge Calculation"
	TITLE$(2%) = "For the base date of " + PRNT_DATE(BASE_DAY$, 8%)
	TITLE$(3%) = "Cutoff period of " + CUTOFF$
	TITLE$(4%) = "AR System"
	TITLE$(5%) = ""
	TITLE$(6%) = "   Account            Amount"
	TITLE$(7%) = ""

	!
	! Print out header account summary
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TOTAL_BAL = 0.0
	TEXT$ = "AR Account Summary"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 10%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 1% TO SUMMARY_H%
		!
		! Print one line of header info
		!
		TEXT$ = "   " + &
			SUMMARY_H$(LOOP%) + " " + &
			FORMAT$(SUMMARY_H(LOOP%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOTAL_BAL = TOTAL_BAL + SUMMARY_H(LOOP%)
	NEXT LOOP%

	!
	! Print total of header info
	!
	TEXT$ = "     Total            " + &
		FORMAT$(TOTAL_BAL, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print out header account summary
	!
	TOTAL_BAL = 0.0
	TEXT$ = "Service Charge Account Summary"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 10%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 1% TO SUMMARY_L%
		!
		! Print one line of header info
		!
		TEXT$ = "   " + &
			SUMMARY_L$(LOOP%) + " " + &
			FORMAT$(SUMMARY_L(LOOP%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOTAL_BAL = TOTAL_BAL + SUMMARY_L(LOOP%)
	NEXT LOOP%

	!
	! Print total of header info
	!
	TEXT$ = "    Total             " + &
		FORMAT$(TOTAL_BAL, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!**********************************************************
	! Help Message for an error
	!**********************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	! if this is a report then
	UTL_REPORTX::STAT = -1%

	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
