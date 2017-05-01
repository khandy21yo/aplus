	!
	! File Layout for: SB.SB_ACCOUNT on 21-May-01
	!
	! GL Accounts for Subsidiary Systems
	!

	RECORD SB_ACCOUNT_CDD
		! Element = SYSTEM
		!   Description = Software System code
		STRING SYSTEM = 2
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element =
		!   Description = Account Group
		STRING ACCTGROUP = 4
	END RECORD
