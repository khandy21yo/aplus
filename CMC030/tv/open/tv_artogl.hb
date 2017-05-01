	!
	! File Layout for: TV.TV_ARTOGL on 21-May-01
	!
	! TV Ar to Gl Conversion Table
	!

	RECORD TV_ARTOGL_CDD
		! Element =
		!   Description = Customer type
		STRING CUSTYP = 2
		! Element =
		!   Description = Description
		STRING DESCR = 20
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING AR_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING SALE_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING DISC_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING PRODUCTION_ACCT = 18
		! Element =
		!   Description = Discount Percentage
		WORD DISC_PER
	END RECORD
