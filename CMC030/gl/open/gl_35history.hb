	!
	! File Layout for: GL.GL_35HISTORY on 21-May-01
	!
	! General Ledger summary by period
	!

	RECORD GL_35HISTORY_CDD
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING PERIOD = 6
		! Element =
		!   Description = Beginning Balance for period
		GFLOAT DOLLARBEGIN
		! Element =
		!   Description = Change for period
		GFLOAT DOLLARCHANGE
		! Element =
		!   Description = Budget for period
		GFLOAT DOLLARBUDGET
		! Element =
		!   Description = Beginning balance
		GFLOAT HOURBEGIN
		! Element =
		!   Description = Change for period
		GFLOAT HOURCHANGE
		! Element =
		!   Description = Budget for period
		GFLOAT HOURBUDGET
		! Element =
		!   Description = Beginning Balance
		GFLOAT UNITBEGIN
		! Element =
		!   Description = Chenge for period
		GFLOAT UNITCHANGE
		! Element =
		!   Description = Budget for period
		GFLOAT UNITBUDGET
	END RECORD
