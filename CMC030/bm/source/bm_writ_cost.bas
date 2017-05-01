1	%TITLE "Update Standard Cost"
	%SBTTL "BM_WRIT_COST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG BM_WRIT_COST(STRING PRODUCT)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho
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
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function returns the extended price for a certain
	!	order number.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_WRIT_COST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP BM_WRIT_COST
	!	$ DELETE BM_WRIT_COST.OBJ;*
	!
	! Author:
	!
	!	08/05/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/05/92 - Dan Perkins
	!		Change error on line 340 from 155 to 5.
	!		Trap error 9 on line 2039.
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/27/92 - Frank F. Starman
	!		Calculate burden as a percentage of the labor, if
	!		there is a percentage rate.
	!
	!	02/09/94 - Kevin Handy
	!		Reformat to 80 columns.
	!
	!	02/09/94 - Kevin Handy
	!		Added common area for BM_RELATION.CH% so it
	!		won't get re-opened every call.
	!
	!	03/07/94 - Kevin Handy
	!		Increased maximum number of levels from 200
	!		to 500.
	!
	!	03/07/94 - Kevin Handy
	!		Added a "REGARDLESS" to get at 2020.
	!
	!	03/11/94 - Kevin Handy
	!		Added a "Working" message so I can try to see why
	!		this thing is taking soooo loooonnnnggg.
	!		Added several comments.
	!
	!	03/22/94 - Kevin Handy
	!		Fix bug going to next product on level.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/15/95 - Kevin Handy
	!		Clean up formatting. Lose unecessary externals.
	!
	!	01/29/96 - Kevin Handy
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	05/21/96 - Kevin Handy
	!		Modified all rounding to be to 6 decimal places,
	!		instead of sometimes two and sometimes 4.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Change SMG$_BOLD to SMG$M_BOLD
	!
	!	10/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Common Statements
	!
	COM (CH_PC_COST)		PC_COST.CH%
	COM (CH_BM_RELATION)		BM_RELATION.CH%
	COM (CH_BM_PRODOPER)		BM_PRODOPER.CH%
	COM (CH_BM_STRUCTURE)		BM_STRUCTURE.CH%
	COM (CH_PR_OPER_READ)		PR_OPER.CH%
	COM (CH_BM_CONTROL)		BM_CONTROL.CH%
	COM (CH_UTL_LOCATION_READ)	UTL_LOCATION.CH%
	COM (CH_UTL_PROFILE_READ)	UTL_PROFILE.CH%

	%PAGE

	!
	! Include Scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map file
	!
	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)		PC_COST_CDD		PC_COST

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP (PR_OPER)		PR_OPER_CDD		PR_OPER
	DECLARE		PR_OPER_CDD		PR_OPER_READ

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)	UTL_PROFILE_CDD		UTL_PROFILE

	RECORD COMPONENT_RECORD
		STRING NUMBER   = 14%, &
		REAL   QUANTITY
	END RECORD

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION PR_READ_OPERATION
	EXTERNAL REAL	FUNCTION PC_READ_COST

	DECLARE LONG SYS_STATUS

	DIM COMPONENT_RECORD	COMPONENT(5000%)
	DIM RFA			RFA_LEVEL(500%)
	DIM REAL		QTY_LEVEL(500%)
	DIM STRING		TEST_PRODUCT(500%)

	! Declare constants
	!
	DECLARE LONG SMG_WIN2
	DECLARE LONG EXIT_STATUS

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Assume no errors
	!
	EXIT_STATUS = CMC$_NORMAL

300	!
	! Open relation file
	!
	IF BM_RELATION.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
		USE
			FILENAME$ = "BM_RELATION"
			CONTINUE HelpError
		END WHEN
	END IF

310	!
	! Open operation file
	!
	IF BM_PRODOPER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.CRE"
		USE
			FILENAME$ = "BM_PRODOPER"
			CONTINUE HelpError
		END WHEN
	END IF

320	!
	! Open cost file
	!
	IF PC_COST.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"
		USE
			FILENAME$ = "PC_COST"
			CONTINUE HelpError
		END WHEN
	END IF

	! Open control file
	!
350	IF BM_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"
			GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS
			CLOSE #BM_CONTROL.CH%
		USE
			FILENAME$ = "BM_CONTROL"
			CONTINUE HelpError
		END WHEN

		!CALL ASSG_FREECHANNEL(BM_CONTROL.CH%)
	END IF

360	!
	! Open Profile to get Default Location
	!
	IF UTL_PROFILE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
			GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
			CLOSE #UTL_PROFILE.CH%
		USE
			FILENAME$ = "UTL_PROFILE"
			CONTINUE HelpError
		END WHEN

		!CALL ASSG_FREECHANNEL(UTL_PROFILE.CH%)
	END IF

 Win2:
2000	!*******************************************************************
	! Set Up Second Window
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		6%, &
		60%, &
		SMG_WIN2, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN2, "Cost Update")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Material Cost", 1%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Labor Cost", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Labor Hours ", 2%, 34%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Burden Cost", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
		"------------------------------", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Total Cost", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Current Cost", 5%, 34%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Difference", 6%, 34%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WIN2, &
		SCOPE::SMG_PBID, &
		12%, &
		08% &
	)

	EFF_DATE$ = DATE_TODAY

2007	TOTAL_COST, TOTAL_MAT = PC_READ_COST(PRODUCT, &
		UTL_PROFILE::DEFLOCATION, EFF_DATE$, EFFDATE$)

	TOTAL_MAT = 0.0

	WHEN ERROR IN
		FIND #BM_RELATION.CH%, KEY #0% EQ PRODUCT, REGARDLESS
	USE
		CONTINUE DisplayTotals IF ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	!*******************************************************************
	! Main loop starts here
	!*******************************************************************

	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE DisplayTotals IF ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	ITEM_LOOP% = 0%
	QTY_LEVEL(0%), LEVEL% = 1%

	!
	! Add this item to new level
	!
 GoDownTree:
	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

2020	!
	! Try for another (deeper) level
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 2025 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	!
	! Test for eternal loop
	!
	FOR L% = 1% TO LEVEL%

		IF BM_RELATION::COMPONENT = TEST_PRODUCT(LEVEL%)
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Infinite Tree by " + &
				TRM$(BM_RELATION::PRODUCT) + " -> " + &
				TRM$(BM_RELATION::COMPONENT), 0%)

			GOTO ExitFunction
		END IF

	NEXT L%

	!
	! Item seems ok, so try to go even deeper
	!
	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

	!
	! We can't go any lower, so add this item to compoment list,
	! and back up one level.
	!
2025	GOSUB 18000
	GOTO 2030

	!
	! Climb up one level, if there is a level to climb up to.
	!
 GoUpTree:
	GOTO AddToPriceFile IF LEVEL% = 1%

	LEVEL% = LEVEL% - 1%

2030	!
	! Try for next component on this level
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE GoUpTree IF ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> TEST_PRODUCT(LEVEL%)
	THEN
		!
		! Different product, go up yet another level
		!
		GOTO GoUpTree
	ELSE
		!
		! Still on same product, go down to level under this part
		! if possible.
		!
		GOTO GoDownTree
	END IF

 AddToPriceFile:
	FOR I% = 1% TO ITEM_LOOP%

		V% = PD_EXAM_PRODUCT(COMPONENT(I%)::NUMBER, PD_PRODUCT_EXAM)

		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 IF &
			PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

		COST = PC_READ_COST(COMPONENT(I%)::NUMBER, &
			UTL_PROFILE::DEFLOCATION, EFF_DATE$, "")

		COST = COST / PD_PRODUCT_EXAM::PRODUCT_FACTOR

		TOTAL_MAT = TOTAL_MAT + &
			FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 6%)

	NEXT I%

	LAST_HOURS, LAST_LABOR, TOTAL_LABOR, TOTAL_HOURS = 0.0

	LAST_OPERATION$ = SPACE$(LEN(BM_PRODOPER::OPERATION) + 1%)

2035	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, KEY #1% EQ TEST_PRODUCT(1%), REGARDLESS
	USE
		CONTINUE DisplayTotals IF ERR = 155%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

2037	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		CONTINUE DisplayTotals IF ERR = 11%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

	GOTO DisplayTotals IF BM_PRODOPER::PRODUCT <> TEST_PRODUCT(1%)

	GOTO 2037 IF BM_PRODOPER::STAT <> "A"
	GOTO 2037 IF BM_PRODOPER::EFFDATE > EFF_DATE$

	IF BM_PRODOPER::OPERATION <> LAST_OPERATION$
	THEN
		TOTAL_HOURS = TOTAL_HOURS + BM_PRODOPER::HOURS
		LABOR = 0.0
	ELSE
		TOTAL_HOURS = TOTAL_HOURS + BM_PRODOPER::HOURS - LAST_HOURS
	END IF

	LAST_HOURS = BM_PRODOPER::HOURS
	LAST_OPERATION$ = BM_PRODOPER::OPERATION

2039	IF PR_READ_OPERATION(BM_PRODOPER::OPERATION, &
		"", PR_OPER_READ) = CMC$_NORMAL
	THEN
		HOUR_RATE = PR_OPER_READ::HOUR_RATE
	ELSE
		HOUR_RATE = 0.0
	END IF

	TOTAL_LABOR = TOTAL_LABOR + &
		FUNC_ROUND(HOUR_RATE * BM_PRODOPER::HOURS, 6%) - LABOR

	LABOR = FUNC_ROUND(HOUR_RATE * BM_PRODOPER::HOURS, 6%)

	GOTO 2037

 DisplayTotals:
	IF BM_CONTROL::BURDENPERC <> 0.0
	THEN
		TOTAL_BURDEN = FUNC_ROUND(TOTAL_LABOR * &
			BM_CONTROL::BURDENPERC / 100.0, 6%)
	ELSE
		TOTAL_BURDEN = FUNC_ROUND(TOTAL_HOURS * &
			BM_CONTROL::BURDENRATE, 6%)
	END IF

	COST = TOTAL_MAT + TOTAL_LABOR + TOTAL_BURDEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
		FORMAT$(TOTAL_MAT, "###,###.##"), &
		1%, 22%,,)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_LABOR, &
		"###,###.##"), 2%, 22%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_HOURS, &
		"###,###.###"), 2%, 49%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_BURDEN, &
		"###,###.##"), &
		3%, 22%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(COST, "###,###.##"), &
		5%, 22%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_COST, &
		"###,###.##"), 5%, 49%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_COST - COST, &
		"###,###.##"), 6%, 49%)

	IF TOTAL_COST = COST
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "", 0%)
		GOTO ExitFunction
	ELSE
		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

		SCOPE::PRG_ITEM = "CONFCOST"
		GOTO ExitFunction IF ENTR_3YESNO(SCOPE, SMG_WIN2, &
			"", "Confirm cost update - then press <Do> ", &
			"N", 0%, "", "") <> "Y"
	END IF

2040	IF EFF_DATE$ <> EFFDATE$
	THEN
		PC_COST::PRODUCT  = TEST_PRODUCT(1%)
		PC_COST::LOCATION = UTL_PROFILE::DEFLOCATION
		PC_COST::EFFDATE  = EFF_DATE$
		PC_COST::COST     = COST

		PUT #PC_COST.CH%
	ELSE
		GET #PC_COST.CH%, KEY #0% EQ TEST_PRODUCT(1%) + &
			UTL_PROFILE::DEFLOCATION + EFF_DATE$

		PC_COST::COST = COST

		UPDATE #PC_COST.CH%
	END IF

 ExitFunction:
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_WIN2, SCOPE::SMG_PBID)

	BM_WRIT_COST = EXIT_STATUS

	EXIT FUNCTION

	%PAGE

18000	!*******************************************************************
	! Array of terminals
	!*******************************************************************

	FOR I% = 1% TO ITEM_LOOP%

		IF BM_RELATION::COMPONENT = COMPONENT(I%)::NUMBER
		THEN
			COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY + &
				FUNC_ROUND(QTY_LEVEL(LEVEL%), 6%)

			GOTO EndTerm
		END IF

	NEXT I%

	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

18010	IF COMP_STRING(PD_PRODUCT_EXAM::PROD_TYPE, BM_CONTROL::PRODTYPE)
	THEN
		ITEM_LOOP% = ITEM_LOOP% + 1%
		COMPONENT(ITEM_LOOP%)::NUMBER = BM_RELATION::COMPONENT
		COMPONENT(ITEM_LOOP%)::QUANTITY = &
			FUNC_ROUND(QTY_LEVEL(LEVEL%), 6%)
	END IF

 EndTerm:
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
