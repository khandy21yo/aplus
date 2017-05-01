	!
	! File Layout for: BT.BT_JOURNALL on 21-May-01
	!
	! Billing to Agency Journal Lines
	!

	RECORD BT_JOURNALL_CDD
		! Element = CUSTOMER
		!   Description = Guardian
		STRING CUSNUM = 10
		! Element =
		!   Description = Child
		STRING CHILD = 40
		! Element =
		!   Description = Rate
		GFLOAT RATE
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
	END RECORD
