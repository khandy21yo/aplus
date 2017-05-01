	!
	! File Layout for: OE.OE_REASONACCT on 21-May-01
	!
	! Reason Account Table
	!

	RECORD OE_REASONACCT_CDD
		! Element =
		!   Description = Reason Code
		STRING CREASON = 2
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
