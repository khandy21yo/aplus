	!
	! File Layout for: PR.PR_TAX_PROFILE_FRI on 21-May-01
	!
	! Payroll Tax Fringe Expense Distribution
	!

	RECORD PR_TAX_PROFILE_FRI_CDD
		! Element = TAXTYPE
		!   Description = Tax type (FI,FU,SU)
		STRING TAX_TYPE = 2
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING LABOR_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING FRI_EX_ACCT = 18
	END RECORD
