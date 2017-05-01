1	%TITLE "Calculate STD and write variances"
	%SBTTL "WP_WRIT_VARIANCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_WRIT_VARIANCE(WP_CLOSEJOUR_CDD WP_CLOSEJOUR, &
		STRING MESSAGE)

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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints the WIP Closing Journal.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_WRIT_VARIANCE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP WP_WRIT_VARIANCE
	!	$ DELETE WP_WRIT_VARIANCE.OBJ;*
	!
	! Author:
	!
	!	10/19/92 - Frank F. Starman
	!
	! Modification History:
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/24/92 - Frank F. Starman
	!		Fixed several bugs.
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/27/92 - Frank F. Starman
	!		Calculate burden based on labor percentage if
	!		there is any.
	!
	!	01/26/93 - Frank F. Starman
	!		Round labor and calculate burder based on the cost.
	!
	!	01/27/93 - Frank F. Starman
	!		Calculate STD based on buyoff date.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/04/93 - Frank F. Starman
	!		Consider shrinkage.
	!
	!	03/03/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/12/93 - Dan Perkins
	!		Added error trapping for opens, lines 27300 - 27400.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/29/93 - Kevin Handy
	!		Trim information going into MESSAGE so that it looks
	!		better.
	!
	!	12/29/93 - Kevin Handy
	!		Added a REGARDLESS to get on BM_RELATION to reduce
	!		problems with not loading prices.
	!
	!	12/29/93 - Kevin Handy
	!		Added several comments to help trace program.
	!
	!	12/29/93 - Kevin Handy
	!		Modified all places where a FIND and a GET are under
	!		the same line number to have seperate line numbers
	!		so that a locked record will not cause a loop
	!		to restart, giving bad answers.
	!
	!	12/29/93 - Kevin Handy
	!		Alphabetized COM areas so they are easier to find.
	!
	!	03/18/94 - Kevin Handy
	!		Increased dimension from 200 to 400.
	!
	!	03/18/94 - Kevin Handy
	!		Modified error trap at 27702 to check for error
	!		number 155 instead of 11.
	!
	!	05/02/94 - Frank F. Starman
	!		Define variable SYSTEM$ and PERIOD$ to fix the problem
	!		with zero actual cost for second job.
	!
	!	10/13/94 - Kevin Handy
	!		Added a couple hundred FUNC_ROUND operations into
	!		the program to eliminate a $-.01 problem.
	!
	!	01/12/94 - Kevin Handy
	!		Modifications to handle case using a percentage
	!		instead of a burden rate.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/15/95 - Kevin Handy
	!		Lose unecessary externals.
	!
	!	09/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	01/18/96 - Kevin Handy
	!		Fix spelling errors.
	!
	!	01/19/96 - Kevin Handy
	!		Redo several IF statements so they use multiple
	!		string comparisons, instead of adding a bunch of
	!		strings together then comparings the big strings.
	!		Much easier to see what's going on.
	!
	!	01/19/96 - Kevin Handy
	!		Changed comparisons with SCRAP% to use 0%
	!		instead of 0.0. Might as well make everything
	!		work in integer mode.
	!
	!	01/19/96 - Kevin Handy
	!		Modify several places where had
	!		"xxx=aaa \ xxx=zzz if bbb=ccc" to
	!		"if bbb=ccc then xxx=zzz else xxx=aaa".
	!
	!	01/19/96 - Kevin Handy
	!		Created structures instead of using several
	!		different unrelated arrays. Maybe it'll be easier
	!		to figure out what it's doing.
	!		Changed names of several variables to cause
	!		less confusion there.
	!
	!	01/24/96 - Kevin Handy
	!		Don't open UTL_PROFILE. Never used.
	!
	!	02/02/96 - Kevin Handy
	!		Slap an ABS arount the dollar labor rule,
	!		so it doesn't screw up so badly when there are
	!		negitive's passing through.
	!
	!	02/07/96 - Kevin Handy
	!		Modified to use the location defined by
	!		JC_JOB::LOCATION instead of UTL_LOCATION::LOCATION,
	!		which is never set up.
	!
	!	02/08/96 - Kevin Handy
	!		Added large number of comments trying to trace
	!		through how this sucker works.
	!		Lose commented out code.
	!
	!	05/21/98 - Kevin Handy
	!		Increase size of batch number from 2 to 8 characters
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/07/2000 - Kevin Handy
	!		Clean up several problems when can't open control
	!		files (set up default values, etc.)
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.HB"
	MAP (WP_CLOSELINE)	WP_CLOSELINE_CDD	WP_CLOSELINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	DECLARE			BM_PRODOPER_CDD		BM_PRODOPER_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	DECLARE			PR_OPER_CDD		PR_OPER_READ

	!
	! This common area must be mapped in both the main program and
	! in WP_MAIN_CLOSEJOURLINE.
	!
	COM (BATCH_WP_CLOSEJOUR) &
		BATCH_NO$ = 8

	!
	! Common Statements
	!
	COM (CH_WP_CLOSELINE) &
		WP_CLOSELINE.CH%, &
		WP_CLOSELINE.READONLY%

	COM (CH_BM_CONTROL_READ)	BM_CONTROL.CH%
	COM (CH_BM_RELATION_READ)	BM_RELATION.CH%
	COM (CH_SB_ACCOUNT_READ)	SB_ACCOUNT.CH%
	COM (CH_SB_BALANCE_READ)	SB_BALANCE.CH%
	COM (CH_SB_CONTROL_READ) &
		SB_CONTROL.CH%, &
		SYSTEM$ = 2%, &
		PERIOD$ = 6%

	COM (CH_WP_SB_SUBACCOUNT)	SB_SUBACCOUNT.CH%
	COM (CH_WP_CLOSEJOUR)		WP_CLOSEJOUR.CH%
	COM (CH_WP_CONTROL)		WP_CONTROL.CH%
	COM (CH_WP_REGLINE_READ)	WP_REGLINE.CH%
	COM (CH_WP_REGREGISTER_READ)	WP_REGREGISTER.CH%
	COM (CH_WP_REQREGISTER_READ)	WP_REQREGISTER.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG	FUNCTION WP_READ_REGLINE
	EXTERNAL LONG	FUNCTION BM_READ_PRODOPER
	EXTERNAL LONG	FUNCTION PR_READ_OPERATION
	EXTERNAL REAL	FUNCTION PC_READ_COST

	RECORD COMPONENT_RECORD
		STRING NUMBER = 14%
		REAL   QUANTITY
	END RECORD

	RECORD BOM_RECORD
		STRING	PRODUCT = 14%
		RFA	RFA_LEVEL
		REAL	QTY
	END RECORD

	RECORD ISSUE_RECORD
		STRING	PRODUCT
		REAL	QTY
		REAL	LABOR
		REAL	HOURS
	END RECORD

	DIM COMPONENT_RECORD	COMPONENT(5000%)
	DIM BOM_RECORD		BOM(400%)
	DIM ISSUE_RECORD	ISSUE(1000%)

	DECLARE LONG EXIT_STATUS

	%PAGE

	ON ERROR GOTO 29000

	EXIT_STATUS = CMC$_NORMAL

	!
	! Initialize standard and actual to zero
	!
	WP_CLOSEJOUR::STDBURDEN = 0.0
	WP_CLOSEJOUR::STDLABOR  = 0.0
	WP_CLOSEJOUR::STDPARTS  = 0.0
	WP_CLOSEJOUR::STDRAWMAT = 0.0

	WP_CLOSEJOUR::ACTBURDEN = 0.0
	WP_CLOSEJOUR::ACTLABOR  = 0.0
	WP_CLOSEJOUR::ACTPARTS  = 0.0
	WP_CLOSEJOUR::ACTRAWMAT = 0.0

	TESTLINE$ = SPACE$(LEN(WP_REGLINE_READ::LLINE))

 ReadRegLine:
	GOTO StartClose &
		IF WP_READ_REGLINE(WP_CLOSEJOUR::JOB, &
		TESTLINE$, "GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	IF FUNC_ROUND(QTY(0%), 2%) <> 0.0
	THEN
		MESSAGE = "Rem buyoff qty " + &
			NUM1$(FUNC_ROUND(QTY(0%), 2%)) + " at job line " + &
			WP_REGLINE_READ::LLINE

		EXIT_STATUS = CMC$_TERMINATED
		GOTO ExitFunction
	END IF

	TESTLINE$, LASTLINE$ = WP_REGLINE_READ::LLINE

	REQLINE$ = SPACE$(LEN(WP_REQREGISTER_READ::REQNUM + &
		WP_REQREGISTER_READ::REQLIN))

 ReadReqLine:
	GOTO ReadRegLine &
		IF LASTLINE$ <> TESTLINE$ OR &
		WP_READ_REQREGISTER(WP_CLOSEJOUR::JOB, LASTLINE$, REQLINE$, &
		"GT", WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	IF FUNC_ROUND(QTY(0%), 2%) <> 0.0
	THEN
		MESSAGE = "Rem issue qty " + &
			NUM1$(FUNC_ROUND(QTY(0%), 2%)) + " at req line " + &
			TRM$(WP_REQREGISTER_READ::REQNUM) + "," + &
			WP_REQREGISTER_READ::REQLIN

		EXIT_STATUS = CMC$_TERMINATED
		GOTO ExitFunction
	END IF

	LASTLINE$ = WP_REQREGISTER_READ::LLINE
	REQLINE$  = WP_REQREGISTER_READ::REQNUM + WP_REQREGISTER_READ::REQLIN

	GOTO ReadReqLine

 StartClose:
	!
	! Open relation file
	!
27300	IF BM_RELATION.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
		USE
			FILENAME$ = "BM_RELATION"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open req register line file
	!
27310	IF WP_REQREGISTER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
		USE
			FILENAME$ = "WP_REQREGISTER"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open register line file
	!
27320	IF WP_REGLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
		USE
			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open WIP balance file
	!
27330	IF SB_BALANCE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
		USE
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open WIP control file
	!
27340	IF WP_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.OPN"
			GET #WP_CONTROL.CH%, RECORD 1%, REGARDLESS
			CLOSE #WP_CONTROL.CH%
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			WP_CONTROL::ORDNUM = ""
			WP_CONTROL::PURGDATE = ""
			WP_CONTROL::STATUS_FLAG = ""
			WP_CONTROL::REQNUM = ""
			WP_CONTROL::INVMATPVAR = ""
			WP_CONTROL::INVMATUVAR = ""
			WP_CONTROL::INVLABRVAR = ""
			WP_CONTROL::INVLABEVAR = ""
			WP_CONTROL::INVBURVAR = ""
			WP_CONTROL::EQMATPVAR = ""
			WP_CONTROL::EQMATUVAR = ""
			WP_CONTROL::EQLABRVAR = ""
			WP_CONTROL::EQLABEVAR = ""
			WP_CONTROL::EQBURVAR = ""

			CALL ASSG_FREECHANNEL(WP_CONTROL.CH%)
			WP_CONTROL.CH% = 0%
			FILENAME$ = "WP_CONTROL"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open BM control file
	!
27350	IF BM_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"
			GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS
			CLOSE #BM_CONTROL.CH%
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			BM_CONTROL::BURDENRATE = 0.0
			BM_CONTROL::PRODTYPE = ""
			BM_CONTROL::LABORRATE = 0.0
			BM_CONTROL::RMAT = ""
			BM_CONTROL::BURDENPERC = 0.0

			CALL ASSG_FREECHANNEL(BM_CONTROL.CH%)
			BM_CONTROL.CH% = 0%
			FILENAME$ = "BM_CONTROL"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open SB control file
	!
27360	IF SB_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
			GET #SB_CONTROL.CH%, KEY #0% EQ "JC", REGARDLESS
			CLOSE #SB_CONTROL.CH%
		USE
			IF ERR = 138%	! File Locked
			THEN
				SLEEP 5%
				RETRY
			END IF

			SB_CONTROL::SYSTEM = ""
			SB_CONTROL::PERIOD = ""
			SB_CONTROL::CONTROLFLAG = ""
			SB_CONTROL::CDATE = ""
			SB_CONTROL::CTIME = ""
			SB_CONTROL::BATCH = ""
			SB_CONTROL::SUBJECT = ""
			SB_CONTROL::DEFNUMBER = ""

			CALL ASSG_FREECHANNEL(SB_CONTROL.CH%)
			SB_CONTROL.CH% = 0%
			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN

		SYSTEM$ = SB_CONTROL::SYSTEM
		PERIOD$ = SB_CONTROL::PERIOD
	END IF

	!
	! Open WIP account file
	!
27380	IF SB_ACCOUNT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
		USE
			FILENAME$ = "SB_ACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open subaccount GL account file
	!
27390	IF SB_SUBACCOUNT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open close line file
	!
27400	IF WP_CLOSELINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.CRE"
		USE
			FILENAME$ = "WP_CLOSELINE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Delete the current records from the WP_CLOSELINE file
	!
27700	WHEN ERROR IN
		FIND #WP_CLOSELINE.CH%, KEY #0% EQ WP_CLOSEJOUR::JOB
	USE
		IF ERR = 154%	! Locked record
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 27705 IF ERR = 155%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

 NextCloseline:
27702	WHEN ERROR IN
		GET #WP_CLOSELINE.CH%, KEY #0% EQ WP_CLOSEJOUR::JOB
	USE
		IF ERR = 154%	! Locked record
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 27705 IF ERR = 155%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

	IF WP_CLOSELINE::JOB = WP_CLOSELINE::JOB
	THEN
		DELETE #WP_CLOSELINE.CH%
		GOTO NextCloseline
	END IF

	!*******************************************************************
	! Calculate ACTUAL totals, put them in WP_CLOSELINE
	! (Reads from SB_BALANCE register.
	!*******************************************************************

27705	TOTAL_LABOR = 0.0
	TOTAL_BURDEN = 0.0
	TOTAL_MAT = 0.0
	TOTAL_RMAT = 0.0
	TOTAL_LABOR_HOURS = 0.0

	WHEN ERROR IN
		FIND #SB_BALANCE.CH%, &
			KEY #1% EQ PERIOD$ + SYSTEM$ + WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE 27800 IF ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

 NextBalance:
27707	WHEN ERROR IN
		GET #SB_BALANCE.CH%, REGARDLESS
	USE
		CONTINUE 27800 IF ERR = 11%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 27800 &
		IF (SB_BALANCE::PERIOD <> PERIOD$) OR &
		(SB_BALANCE::SYSTEM <> SYSTEM$) OR &
		(SB_BALANCE::SUBACCOUNT <> WP_CLOSEJOUR::JOB)

	RESET #SB_ACCOUNT.CH%

27710	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE NextBalance IF ERR = 11% OR ERR = 9%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

27720	IF COMP_STRING(SB_BALANCE::ACCOUNT, SB_ACCOUNT::ACCOUNT)
	THEN
		SELECT SB_ACCOUNT::ACCTGROUP

		CASE "BURD", "ILAB"
			TOTAL_BURDEN = FUNC_ROUND(TOTAL_BURDEN + &
				SB_BALANCE::AMOUNT + SB_BALANCE::BEG_AMOUNT, 2%)

		CASE "DLAB"
			TOTAL_LABOR = FUNC_ROUND(TOTAL_LABOR + &
				SB_BALANCE::AMOUNT + SB_BALANCE::BEG_AMOUNT, 2%)

			TOTAL_LABOR_HOURS = FUNC_ROUND(TOTAL_LABOR_HOURS + &
				SB_BALANCE::HOURS + SB_BALANCE::BEG_HOURS, 2%)

		CASE "PMAT"
			TOTAL_MAT = FUNC_ROUND(TOTAL_MAT + &
				SB_BALANCE::AMOUNT + SB_BALANCE::BEG_AMOUNT, 2%)

		CASE "RMAT"
			TOTAL_RMAT = FUNC_ROUND(TOTAL_RMAT + &
				SB_BALANCE::AMOUNT + SB_BALANCE::BEG_AMOUNT, 2%)

		END SELECT

		WP_CLOSELINE::JOB	= WP_CLOSEJOUR::JOB
		WP_CLOSELINE::LFLAG	= "A"
		WP_CLOSELINE::VCLASS	= SB_ACCOUNT::ACCTGROUP
		WP_CLOSELINE::VACCT	= SB_BALANCE::ACCOUNT
		WP_CLOSELINE::VAMOUNT	= -FUNC_ROUND(SB_BALANCE::AMOUNT + &
			SB_BALANCE::BEG_AMOUNT, 2%)

		PUT #WP_CLOSELINE.CH%

		GOTO NextBalance
	END IF

	GOTO 27710

27800	!
	! Set up actuals in close journal
	!
	WP_CLOSEJOUR::ACTBURDEN = TOTAL_BURDEN
	WP_CLOSEJOUR::ACTLABOR  = TOTAL_LABOR
	WP_CLOSEJOUR::ACTPARTS  = TOTAL_MAT
	WP_CLOSEJOUR::ACTRAWMAT = TOTAL_RMAT

	ACT_TOTAL = WP_CLOSEJOUR::ACTBURDEN + WP_CLOSEJOUR::ACTLABOR + &
		WP_CLOSEJOUR::ACTPARTS + WP_CLOSEJOUR::ACTRAWMAT

	ISSUE% = 0%

	!*******************************************************************
	! Calculate STANDARD totals
	!*******************************************************************

	!
	! Scan through the WP_REGLINE journal, finding out which
	! products have been ordered.
	!
28000	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, &
			KEY #0% EQ WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE CalcVar IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

 NextJobLine:
28003	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE CalcVar IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO CalcVar &
		IF WP_REGLINE::JOB <> WP_CLOSEJOUR::JOB

	GOTO NextJobLine &
		IF WP_REGLINE::REC_TYPE <> "02"

	TOTAL_MAT = 0.0
	TOTAL_RMAT = 0.0
	TOTAL_LABOR = 0.0

	IF WP_REGLINE::TTYPE = "L"
	THEN
		TOTAL_LABOR = FUNC_ROUND(WP_REGLINE::COST * &
			WP_REGLINE::QTY, 2%)

		!
		! DONE WITH THE LINE
		!
		GOTO DisplayTotals
	END IF

	!
	! Create an initial component in case bill of materials
	! doesn't pull up anything.
	!
	COMPONENT% = 1%
	COMPONENT(COMPONENT%)::NUMBER	= WP_REGLINE::ITEMCODE
	COMPONENT(COMPONENT%)::QUANTITY = WP_REGLINE::QTY

	!
	! Check the BM_RELATION file to determine what
	! components are used in this product.
	!
28007	WHEN ERROR IN
		FIND #BM_RELATION.CH%, &
			KEY #0% EQ WP_REGLINE::ITEMCODE, &
			REGARDLESS
	USE
		CONTINUE 28033 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

28010	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 28033 IF ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	BOM(1%)::PRODUCT = BM_RELATION::PRODUCT

	COMPONENT% = 0%
	BOM(0%)::QTY = WP_REGLINE::QTY
	BOM% = 1%

	!
	! GoDownTree
	!
28015	BOM(BOM%)::PRODUCT	= BM_RELATION::PRODUCT
	BOM(BOM%)::QTY		= BOM(BOM% - 1%)::QTY * BM_RELATION::QUANTITY
	BOM(BOM%)::RFA_LEVEL	= GETRFA(BM_RELATION.CH%)
	SCRAP%			= BM_RELATION::SCRAP

28020	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 28500 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	GOTO CheckForLabor &
		IF SCRAP% = 0%

	!
	! try to figure out, if there is really a shrinkage
	!
	COMP_ISS_QTY = 0.0

28021	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, &
			KEY #2% EQ BM_RELATION::COMPONENT + &
			JC_JOB::LOCATION + WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE CheckForLabor IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

 NextCompRec:
28022	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE CheckForLabor IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	IF (BM_RELATION::COMPONENT = WP_REQREGISTER::PRODUCT) AND &
		(JC_JOB::LOCATION = WP_REQREGISTER::LOCATION) AND &
		(WP_CLOSEJOUR::JOB = WP_REQREGISTER::JOB)
	THEN
		IF WP_REQREGISTER::RECTYP = "02"
		THEN
			COMP_ISS_QTY = COMP_ISS_QTY + WP_REQREGISTER::QTY
		END IF

		GOTO NextCompRec
	END IF

 CheckForLabor:
	!
	! test if the higher level had been issued
	!
	FOR L% = 1% TO ISSUE%

		IF BM_RELATION::PRODUCT = ISSUE(L%)::PRODUCT
		THEN
			IF ISSUE(L%)::QTY >= BOM(BOM%)::QTY
			THEN
				ISSUE(L%)::QTY = ISSUE(L%)::QTY - &
					BOM(BOM%)::QTY

				TOTAL_LABOR = TOTAL_LABOR - &
					FUNC_ROUND(BOM(BOM%)::QTY * &
					ISSUE(L%)::LABOR, 2%)

				!
				! add product to the array
				!
				ISSUE_QTY = BOM(BOM%)::QTY
				GOSUB 28550
				GOTO 28030
			ELSE
				IF ISSUE(L%)::QTY > 0.0
				THEN
					TOTAL_LABOR = TOTAL_LABOR - &
						ISSUE(L%)::QTY * &
						ISSUE(L%)::LABOR

					BOM(BOM%)::QTY = &
						BOM(BOM%)::QTY - &
						ISSUE(L%)::QTY

					!
					! add product to the array
					!
					ISSUE_QTY = ISSUE(L%)::QTY
					GOSUB 28550
				END IF

				GOTO ContDown
			END IF
		END IF

	NEXT L%

	!
	! try to figure out, how much had been issued for a product
	!
28023	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, &
			KEY #2% EQ BM_RELATION::PRODUCT + &
			JC_JOB::LOCATION + WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE ContDown IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	ISSUE% = ISSUE% + 1%

	ISSUE(ISSUE%)::PRODUCT	= BM_RELATION::PRODUCT
	ISSUE(ISSUE%)::QTY	= 0.0
	ISSUE(ISSUE%)::LABOR	= 0.0
	ISSUE(ISSUE%)::HOURS	= 0.0

 NextProdRec:
28024	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE 28026 IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	IF (BM_RELATION::PRODUCT = WP_REQREGISTER::PRODUCT) AND &
		(JC_JOB::LOCATION = WP_REQREGISTER::LOCATION) AND &
		(WP_CLOSEJOUR::JOB = WP_REQREGISTER::JOB)
	THEN
		IF WP_REQREGISTER::RECTYP = "02"
		THEN
			ISSUE(ISSUE%)::QTY = ISSUE(ISSUE%)::QTY + &
				WP_REQREGISTER::QTY
		END IF

		GOTO NextProdRec
	END IF

	!
	! Force shrinkage
	!
28026	IF (SCRAP% <> 0%) AND (COMP_ISS_QTY = 0.0) AND &
		(ISSUE(ISSUE%)::QTY < BOM(BOM%)::QTY)
	THEN
		ISSUE(ISSUE%)::QTY = BOM(BOM%)::QTY
	END IF

	OPERATION$ = "        "

	!
	! try to figure out how much labor is needed for a product
	!
 ReadProdOper:
	IF BM_READ_PRODOPER(BM_RELATION::PRODUCT, &
		OPERATION$, "GT", WP_REGLINE::COMP_DATE, &
		BM_PRODOPER_READ) = CMC$_NORMAL
	THEN
		OPERATION$ = BM_PRODOPER_READ::OPERATION

		IF PR_READ_OPERATION(BM_PRODOPER_READ::OPERATION, &
			"", PR_OPER_READ) = CMC$_NORMAL
		THEN
			HOUR_RATE = PR_OPER_READ::HOUR_RATE
		ELSE
			HOUR_RATE = 0.0
		END IF

		!
		! dollar labor per unit
		!
		ISSUE(ISSUE%)::LABOR = FUNC_ROUND(ISSUE(ISSUE%)::LABOR + &
			HOUR_RATE * BM_PRODOPER_READ::HOURS, 2%)

		!
		! hours per unit
		!
		ISSUE(ISSUE%)::HOURS = ISSUE(ISSUE%)::HOURS + &
			BM_PRODOPER_READ::HOURS

		GOTO ReadProdOper
	END IF

	GOTO CheckForLabor

	!*******************************************************************
	! Go down another level
	!*******************************************************************
 ContDown:
	BOM% = BOM% + 1%
	GOTO 28015

	!*******************************************************************
	! Go Up One Level
	!*******************************************************************
28027	GOTO 28033 &
		IF BOM% - 1% = 0%

	BOM% = BOM% - 1%

28030	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA BOM(BOM%)::RFA_LEVEL, REGARDLESS

		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 28027 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> BOM(BOM%)::PRODUCT
	THEN
		GOTO 28027
	ELSE
		GOTO 28015
	END IF

	!*******************************************************************
	!
	!*******************************************************************

28033	FOR I% = 1% TO COMPONENT%

		V% = PD_EXAM_PRODUCT(COMPONENT(I%)::NUMBER, PD_PRODUCT_EXAM)

		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

		COST = PC_READ_COST(COMPONENT(I%)::NUMBER, &
			JC_JOB::LOCATION, WP_REGLINE::COMP_DATE, "")

		COST = COST / PD_PRODUCT_EXAM::PRODUCT_FACTOR

		IF COMP_STRING(PD_PRODUCT_EXAM::PROD_TYPE, BM_CONTROL::RMAT)
		THEN
			TOTAL_RMAT = TOTAL_RMAT + &
				FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 2%)
		ELSE
			TOTAL_MAT = TOTAL_MAT + &
				FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 2%)
		END IF

	NEXT I%

	!
	! total labor for parent
	!
	OPERATION$ = "        "
	PR_OPER_READ::PIECE_RATE = 0.0

 ReadOperation:
	IF BM_READ_PRODOPER(WP_REGLINE::ITEMCODE, &
		OPERATION$, "GT", WP_REGLINE::COMP_DATE, &
		BM_PRODOPER_READ) = CMC$_NORMAL
	THEN
		OPERATION$ = BM_PRODOPER_READ::OPERATION

		IF PR_READ_OPERATION(BM_PRODOPER_READ::OPERATION, &
			"", PR_OPER_READ) = CMC$_NORMAL
		THEN
			HOUR_RATE = PR_OPER_READ::HOUR_RATE
		ELSE
			HOUR_RATE = 0.0
		END IF

		TOTAL_LABOR = TOTAL_LABOR + &
			FUNC_ROUND(HOUR_RATE * BM_PRODOPER_READ::HOURS, 2%) * &
			WP_REGLINE::QTY

		GOTO ReadOperation
	END IF

	GOTO DisplayTotals

28500	!*******************************************************************
	! Array of terminals
	! Try to add this Bill of Material Item to the component list,
	! if it has the correct product type.
	!*******************************************************************

	FOR I% = 1% TO COMPONENT%

		IF BM_RELATION::COMPONENT = COMPONENT(I%)::NUMBER
		THEN
			COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY + &
				FUNC_ROUND(BOM(BOM%)::QTY, 3%)

			GOTO EndTerm
		END IF

	NEXT I%

	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

28510	IF COMP_STRING(PD_PRODUCT_EXAM::PROD_TYPE, BM_CONTROL::PRODTYPE)
	THEN
		COMPONENT% = COMPONENT% + 1%

		COMPONENT(COMPONENT%)::NUMBER = BM_RELATION::COMPONENT
		COMPONENT(COMPONENT%)::QUANTITY = &
			FUNC_ROUND(BOM(BOM%)::QTY, 3%)
	END IF

 EndTerm:
	GOTO 28030

28550	!*******************************************************************
	! Array of terminals from higher level
	!*******************************************************************
	FOR I% = 1% TO COMPONENT%

		IF BM_RELATION::PRODUCT = COMPONENT(I%)::NUMBER
		THEN
			COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY + &
				FUNC_ROUND(ISSUE_QTY, 3%)

			GOTO Ret28550
		END IF

	NEXT I%

	COMPONENT% = COMPONENT% + 1%

	COMPONENT(COMPONENT%)::NUMBER = BM_RELATION::PRODUCT

	COMPONENT(COMPONENT%)::QUANTITY = FUNC_ROUND(ISSUE_QTY, 3%)

 Ret28550:
	RETURN

	!*******************************************************************
	! Done with one issue.
	!*******************************************************************
 DisplayTotals:
28600	TOTAL_COST = FUNC_ROUND(PC_READ_COST(WP_REGLINE::ITEMCODE, &
		JC_JOB::LOCATION, WP_REGLINE::COMP_DATE, "") * &
		WP_REGLINE::QTY, 2%)

 !
 ! Temporary stuff to see how things calculate
 !
 ! xxxyyy$ = TRM$(WP_REGLINE::JOB) + " " + TRM$(WP_REGLINE::LLINE) + " " + &
 !	NUM1$(TOTAL_BURDEN) + " " + NUM1$(TOTAL_LABOR) + " " + &
 !	NUM1$(TOTAL_MAT) + " " + NUM1$(TOTAL_RMAT) + " " + &
 !	NUM1$(TOTAL_COST)
 ! CALL ENTR_3MESSAGE(SCOPE, XXXYYY$, 0%)

	!
	! Apply a dollar rule. No labor if it comes to less than one
	! dollar.
	!
	IF ABS(TOTAL_LABOR) < 1.0
	THEN
		TOTAL_LABOR = 0.0
		TOTAL_BURDEN = 0.0
		TOTAL_MAT = FUNC_ROUND(TOTAL_COST - TOTAL_RMAT, 2%)
	ELSE
		TOTAL_BURDEN = FUNC_ROUND(TOTAL_COST - TOTAL_LABOR - &
			TOTAL_MAT - TOTAL_RMAT, 2%)
	END IF

 !
 ! Temporary stuff to see how things calculate
 !
 ! xxxyyy$ = "After:" + TRM$(WP_REGLINE::JOB) + " " + TRM$(WP_REGLINE::LLINE) + " " + &
 !	NUM1$(TOTAL_BURDEN) + " " + NUM1$(TOTAL_LABOR) + " " + &
 !	NUM1$(TOTAL_MAT) + " " + NUM1$(TOTAL_RMAT) + " " + &
 !	NUM1$(TOTAL_COST)
 ! CALL ENTR_3MESSAGE(SCOPE, XXXYYY$, 0%)

	!
	! Set up standards in WP_CLOSEJOUR
	!
	WP_CLOSEJOUR::STDBURDEN = FUNC_ROUND(WP_CLOSEJOUR::STDBURDEN + &
		TOTAL_BURDEN, 2%)
	WP_CLOSEJOUR::STDLABOR  = FUNC_ROUND(WP_CLOSEJOUR::STDLABOR  + &
		TOTAL_LABOR, 2%)
	WP_CLOSEJOUR::STDPARTS  = FUNC_ROUND(WP_CLOSEJOUR::STDPARTS  + &
		TOTAL_MAT, 2%)
	WP_CLOSEJOUR::STDRAWMAT = FUNC_ROUND(WP_CLOSEJOUR::STDRAWMAT + &
		TOTAL_RMAT, 2%)

	STD_TOTAL = FUNC_ROUND(WP_CLOSEJOUR::STDBURDEN + &
		WP_CLOSEJOUR::STDLABOR + &
		WP_CLOSEJOUR::STDPARTS + WP_CLOSEJOUR::STDRAWMAT, 2%)

	GOTO NextJobLine

	!*******************************************************************
	! Calculate variance records
	!*******************************************************************
 CalcVar:
	GOTO ExitFunction &
		IF WP_CLOSEJOUR::VARFLAG = ""

	TOTAL_VAR = FUNC_ROUND( &
		WP_CLOSEJOUR::ACTBURDEN + &
		WP_CLOSEJOUR::ACTPARTS + &
		WP_CLOSEJOUR::ACTRAWMAT + &
		WP_CLOSEJOUR::ACTLABOR - &
 &
		WP_CLOSEJOUR::STDBURDEN - &
		WP_CLOSEJOUR::STDPARTS - &
		WP_CLOSEJOUR::STDRAWMAT - &
		WP_CLOSEJOUR::STDLABOR, 2%)

	WP_CLOSELINE::JOB = WP_CLOSEJOUR::JOB
	WP_CLOSELINE::LFLAG = "V"

	!
	! Generate BURD variance record
	!
	BURDEN_VAR = FUNC_ROUND(WP_CLOSEJOUR::ACTBURDEN - &
		WP_CLOSEJOUR::STDBURDEN, 2%)

	WP_CLOSELINE::VCLASS	= "BURD"
	IF WP_CLOSEJOUR::VARFLAG = "E"
	THEN
		WP_CLOSELINE::VACCT = WP_CONTROL::EQBURVAR
	ELSE
		WP_CLOSELINE::VACCT = WP_CONTROL::INVBURVAR
	END IF
	WP_CLOSELINE::VAMOUNT = BURDEN_VAR

	PUT #WP_CLOSELINE.CH% &
		IF BURDEN_VAR <> 0.0

	!
	! Generate MAT variance record
	!
	MAT_VAR = FUNC_ROUND(WP_CLOSEJOUR::ACTPARTS + &
		WP_CLOSEJOUR::ACTRAWMAT - &
		WP_CLOSEJOUR::STDPARTS - WP_CLOSEJOUR::STDRAWMAT, 2%)

	WP_CLOSELINE::VCLASS = "MAT"
	IF WP_CLOSEJOUR::VARFLAG = "E"
	THEN
		WP_CLOSELINE::VACCT = WP_CONTROL::EQMATUVAR
	ELSE
		WP_CLOSELINE::VACCT = WP_CONTROL::INVMATUVAR
	END IF
	WP_CLOSELINE::VAMOUNT = MAT_VAR

	PUT #WP_CLOSELINE.CH% &
		IF MAT_VAR <> 0.0


	LABE_VAR = 0.0

	IF (BM_CONTROL::BURDENRATE <> 0.0) AND (PR_OPER_READ::PIECE_RATE = 0.0)
	THEN
		LABE_VAR = FUNC_ROUND( &
			(WP_CLOSEJOUR::ACTBURDEN / BM_CONTROL::BURDENRATE) * &
			BM_CONTROL::LABORRATE - WP_CLOSEJOUR::STDLABOR, 2%)
	ELSE
		IF (BM_CONTROL::BURDENPERC = 0.0)
		THEN
			LABE_VAR = FUNC_ROUND( &
				TOTAL_VAR - MAT_VAR - BURDEN_VAR, 2%) &
				IF PR_OPER_READ::PIECE_RATE <> 0.0
		ELSE
			LABE_VAR = FUNC_ROUND( &
				(TOTAL_LABOR_HOURS * BM_CONTROL::LABORRATE) - &
				WP_CLOSEJOUR::STDLABOR, 2%)
		END IF
	END IF

	!
	! Generate LABE variance record
	!
	WP_CLOSELINE::VCLASS = "LABE"
	IF WP_CLOSEJOUR::VARFLAG = "E"
	THEN
		WP_CLOSELINE::VACCT = WP_CONTROL::EQLABEVAR
	ELSE
		WP_CLOSELINE::VACCT = WP_CONTROL::INVLABEVAR
	END IF
	WP_CLOSELINE::VAMOUNT = LABE_VAR

	PUT #WP_CLOSELINE.CH% &
		IF LABE_VAR <> 0.0

	!
	! Generate LABR variance record
	!
	LABR_VAR = FUNC_ROUND(TOTAL_VAR - MAT_VAR - BURDEN_VAR - LABE_VAR, 2%)

	WP_CLOSELINE::VCLASS	= "LABR"
	WP_CLOSELINE::VAMOUNT	= LABR_VAR

	IF WP_CLOSEJOUR::VARFLAG = "E"
	THEN
		WP_CLOSELINE::VACCT = WP_CONTROL::EQLABRVAR
	ELSE
		WP_CLOSELINE::VACCT = WP_CONTROL::INVLABRVAR
	END IF

	PUT #WP_CLOSELINE.CH% &
		IF LABR_VAR <> 0.0

 ExitFunction:
	WP_WRIT_VARIANCE = EXIT_STATUS

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

29000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
