	!
	! File Layout for: PR.PR_TAX_PROFILE_E on 21-May-01
	!
	! Payroll Tax Profile - School District
	!

	RECORD PR_TAX_PROFILE_E_CDD
		! Element =
		!   Description = 'E' for School
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
		STRING SCH_LIA_ACCT = 18
	END RECORD
