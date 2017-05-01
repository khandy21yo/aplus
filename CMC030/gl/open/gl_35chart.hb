	!
	! File Layout for: GL.GL_35CHART on 21-May-01
	!
	! CHART OF ACCOUNTS
	!

	RECORD GL_35CHART_CDD
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element =
		!   Description = DESCRIPTION
		STRING DESCR = 40
		! Element =
		!   Description = Account type
		STRING ACCTYPE = 1
		! Element =
		!   Description = Category
		STRING CATEGORY = 4
		! Element =
		!   Description = Cash flow
		STRING FLOW = 4
		! Element =
		!   Description = Work Capitol
		STRING WORK = 4
		! Element =
		!   Description = Financial type
		STRING FINTYPE = 10
		! Element =
		!   Description = Summary flag
		STRING SUMMARY = 1
		! Element =
		!   Description = System id
		STRING SYSTEM = 2
		! Element = ACCOUNT
		!   Description = Accrual Account
		STRING ACCRUAL_ACCT = 18
	END RECORD
