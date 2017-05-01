	!
	! File Layout for: PR.PR_TAX_PROFILE_D on 21-May-01
	!
	! Payroll Tax Profile - County
	!

	RECORD PR_TAX_PROFILE_D_CDD
		! Element =
		!   Description = 'D' county authority
		STRING AUTH = 1
		! Element =
		!   Description = Code
		STRING CODE = 2
		! Element =
		!   Description = Report number
		STRING REPNO = 20
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING WH_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING COU_LIA_ACCT = 18
	END RECORD
