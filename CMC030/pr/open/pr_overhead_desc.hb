	!
	! File Layout for: PR.PR_OVERHEAD_DESC on 21-May-01
	!
	! Payroll Overhead Description File
	!

	RECORD PR_OVERHEAD_DESC_CDD
		! Element =
		!   Description = Overhead key
		STRING OVH_KEY = 6
		! Element =
		!   Description = Description
		STRING DESCR = 30
		! Element =
		!   Description = Overhead rate
		GFLOAT RATE
		! Element =
		!   Description = Basis (1-hours, 2-amount)
		STRING BASIS = 1
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING PREM_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING OVRHD_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING EX_ACCT = 18
	END RECORD
