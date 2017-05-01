	!
	! File Layout for: UTL.UTL_TRANSACCT on 21-May-01
	!
	! Inventory Transaction GL Accounts
	!

	RECORD UTL_TRANSACCT_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = TRANSTYPE
		!   Description = Transaction type code
		STRING TRANSTYPE = 2
		! Element = PRODTYPE
		!   Description = Inventory Product Type
		STRING PRODTYPE = 2
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
