	!
	! File Layout for: UTL.UTL_ACCOUNT on 21-May-01
	!
	! Transaction Account Table
	!

	RECORD UTL_ACCOUNT_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = TRANSTYPE
		!   Description = Transaction type code
		STRING TRANSTYPE = 2
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
