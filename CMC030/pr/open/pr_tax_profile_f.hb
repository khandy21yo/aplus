	!
	! File Layout for: PR.PR_TAX_PROFILE_F on 20-Oct-04
	!
	! Payroll Tax Profile - Federal
	!

	RECORD PR_TAX_PROFILE_F_CDD
		! Element =
		!   Description = 'F' for federal records
		STRING AUTH = 1
		! Element =
		!   Description = ' ' for federal
		STRING CODE = 2
		! Element =
		!   Description =
		STRING REPNO = 20
		! Element = ACCOUNT
		!   Description = WH Account
		STRING WH_ACCT = 18
		! Element = ACCOUNT
		!   Description = FICA Expense Account
		STRING FICA_EX_ACCT = 18
		! Element = ACCOUNT
		!   Description = FICA Liability Account
		STRING FICA_LIA_ACCT_EMPR = 18
		! Element = ACCOUNT
		!   Description = FICA Employee Liability
		STRING FICA_LIA_ACCT_EMPE = 18
		! Element = ACCOUNT
		!   Description = FUI Expense Account
		STRING FUI_EX_ACCT = 18
		! Element = ACCOUNT
		!   Description = FUI Liability Account
		STRING FUI_LIA_ACCT = 18
		! Element =
		!   Description = FUI Percentage
		GFLOAT FUI_PCT
		! Element =
		!   Description = FUI Maximum
		GFLOAT FUI_MAX
		! Element = ACCOUNT
		!   Description = Cash Account
		STRING CASH_ACCT = 18
		! Element = ACCOUNT
		!   Description = PR Accrual Account
		STRING PR_ACCRUAL_ACCT = 18
		! Element =
		!   Description = Minimum wage
		GFLOAT MIN_WAGE
		! Element =
		!   Description = Direct Deposit Code
		STRING DIRECT = 20
		! Element =
		!   Description = Extra Space 1
		STRING EXTRA1 = 20
		! Element =
		!   Description = Extra Space 2
		STRING EXTRA2 = 20
	END RECORD
