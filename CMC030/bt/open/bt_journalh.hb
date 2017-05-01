	!
	! File Layout for: BT.BT_JOURNALH on 21-May-01
	!
	! Billing Tuition Journal Header
	!

	RECORD BT_JOURNALH_CDD
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = DATE
		!   Description = Transaction Date (YYYYMMDD)
		STRING TRADAT = 8
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING AR_ACCT = 18
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVNUM = 8
	END RECORD
