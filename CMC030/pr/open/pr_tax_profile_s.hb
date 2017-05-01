	!
	! File Layout for: PR.PR_TAX_PROFILE_S on 21-May-01
	!
	! Payroll Tax Profile - State
	!

	RECORD PR_TAX_PROFILE_S_CDD
		! Element =
		!   Description = 'S' for state
		STRING AUTH = 1
		! Element =
		!   Description = User defined (ID, FL, ...)
		STRING CODE = 2
		! Element =
		!   Description =
		STRING REPNO = 20
		! Element = ACCOUNT
		!   Description = WH Account
		STRING WH_ACCT = 18
		! Element = ACCOUNT
		!   Description = OST Liability Account
		STRING OST_LIA_ACCT = 18
		! Element = ACCOUNT
		!   Description = SUI Expense Account
		STRING SUI_EX_ACCT = 18
		! Element = ACCOUNT
		!   Description = SUI Liability Account
		STRING SUI_LIA_ACCT = 18
		! Element =
		!   Description = SUI Percentage
		GFLOAT SUI_PCT
		! Element =
		!   Description = SUI Maximum
		GFLOAT SUI_MAX
		! Element =
		!   Description = Minimum wage
		GFLOAT MIN_WAGE
		! Element = ACCOUNT
		!   Description = OST Expense account
		STRING OST_EX_ACCT = 18
		! Element =
		!   Description = OST Percentage
		GFLOAT OST_PCT
		! Element =
		!   Description = OST Maximum
		GFLOAT OST_MAX
		! Element =
		!   Description = OST Ded maximum
		GFLOAT OST_DEDMAX
		! Element =
		!   Description = SUI Account Number
		STRING SUTANO = 20
	END RECORD
