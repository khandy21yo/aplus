	!
	! File Layout for: BC.BC_CUSTYP on 21-May-01
	!
	! Billing to Customer Type Definitions
	!

	RECORD BC_CUSTYP_CDD
		! Element =
		!   Description = Customer type
		STRING CUSTYP = 2
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING AR_ACCT = 18
		! Element =
		!   Description = Description
		STRING DESCR = 30
	END RECORD
