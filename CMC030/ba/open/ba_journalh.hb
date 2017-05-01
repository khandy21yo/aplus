	!
	! File Layout for: BA.BA_JOURNALH on 21-May-01
	!
	! Agency Journal Header
	!

	RECORD BA_JOURNALH_CDD
		! Element =
		!   Description = Billing Number
		STRING BILLNUM = 10
		! Element = CUSTOMER
		!   Description = Agency Number
		STRING CUSNUM = 10
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element =
		!   Description = Transaction Date
		STRING TRADAT = 8
		! Element = DATE
		!   Description = From payroll date (YYYYMMDD)
		STRING FROMDATE = 8
		! Element = DATE
		!   Description = To payroll date (YYYYMMDD)
		STRING TODATE = 8
		! Element =
		!   Description = Non productive operations
		STRING OPERATIONS = 20
	END RECORD
