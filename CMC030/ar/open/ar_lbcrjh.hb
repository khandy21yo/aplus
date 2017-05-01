	!
	! File Layout for: AR.AR_LBCRJH on 21-May-01
	!
	! Cash Receipts Journal
	!

	RECORD AR_LBCRJH_CDD
		! Element =
		!   Description = Receipt Number
		STRING RECNUM = 8
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = CHECK
		!   Description = Check number
		STRING CHECK = 6
		! Element = DEPOSIT
		!   Description = Deposit number
		STRING DEPOSIT = 6
		! Element =
		!   Description = Transaction Date
		STRING TRADAT = 8
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element =
		!   Description = Amount
		GFLOAT AMNT
		! Element =
		!   Description = Transaction Type
		STRING TRATYP = 2
		! Element =
		!   Description = Description
		STRING DESCR = 25
		! Element = INVOICE
		!   Description = Apply to first Invoice Number
		STRING INVOICE = 8
	END RECORD
