	!
	! File Layout for: BT.BT_CUSTYP on 21-May-01
	!
	! Customer Type Definition
	!

	RECORD BT_CUSTYP_CDD
		! Element =
		!   Description = Customer Type
		STRING CUSTYP = 2
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING AR_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING REV_MASK = 18
	END RECORD
