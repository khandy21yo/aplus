	!
	! File Layout for: PR.PR_TAX_PROFILE_C on 21-May-01
	!
	! Payroll Tax Profile - City
	!

	RECORD PR_TAX_PROFILE_C_CDD
		! Element =
		!   Description = 'C' for city records
		STRING AUTH = 1
		! Element =
		!   Description = User defined
		STRING CODE = 2
		! Element =
		!   Description =
		STRING REPNO = 20
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING WH_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING CITY_LIA_ACCT = 18
	END RECORD
