	!
	! File Layout for: BI.BI_CONTROL on 21-May-01
	!
	! Billing to Insurance Control File
	!

	RECORD BI_CONTROL_CDD
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVOICE = 8
		! Element = ACCOUNT
		!   Description = AR General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
