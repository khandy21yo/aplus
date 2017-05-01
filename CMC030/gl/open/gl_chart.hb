	!
	! File Layout for: GL.GL_CHART on 21-May-01
	!
	! Chart of Accounts
	!

	RECORD GL_CHART_CDD
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element =
		!   Description = Description
		STRING DESCR = 40
		! Element =
		!   Description = Account type
		STRING ACCTYPE = 1
		! Element =
		!   Description = Cash flow
		STRING FLOW = 4
		! Element =
		!   Description = Work capitol
		STRING WORK = 4
		! Element =
		!   Description = Financial type
		STRING FINTYPE = 10
		! Element =
		!   Description = Summary flag 1 - Detail 2 - By date 3 -
		STRING SUMMARY = 1
		! Element =
		!   Description = Period dollar totals
		GFLOAT DOLLAR(20)
		! Element =
		!   Description = Period unit totals
		GFLOAT UNIT(20)
		! Element =
		!   Description = Period hour totals
		GFLOAT HOUR(20)
		! Element =
		!   Description = Current period
		WORD CPERIOD
		! Element =
		!   Description = Running dollars
		GFLOAT RUNDOL
		! Element =
		!   Description = Running units
		GFLOAT RUNUNIT
		! Element =
		!   Description = Running hours
		GFLOAT RUNHOUR
		! Element =
		!   Description = Current dollars
		GFLOAT CURDOL
		! Element =
		!   Description = Current units
		GFLOAT CURUNIT
		! Element =
		!   Description = Current hours
		GFLOAT CURHOUR
		! Element =
		!   Description = Last batch updated
		STRING BATCH = 6
	END RECORD
