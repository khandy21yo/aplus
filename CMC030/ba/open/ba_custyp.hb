	!
	! File Layout for: BA.BA_CUSTYP on 21-May-01
	!
	! Customer Type Definitions
	!

	RECORD BA_CUSTYP_CDD
		! Element =
		!   Description = Customer Type
		STRING CUSTYP = 2
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING AR_ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number Mask
		STRING REV_MASK = 18
		! Element =
		!   Description = Description
		STRING DESCR = 30
	END RECORD
