	!
	! File Layout for: AR.AR_CRJH on 21-May-01
	!
	! Cash Receipts Journal (Header)
	!

	RECORD AR_CRJH_CDD
		! Element =
		!   Description = Receipt number
		STRING RECNUM = 8
		! Element = CUSNUM
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Check Number
		STRING CHECK = 6
		! Element =
		!   Description = Deposit number
		STRING DEPOSIT = 6
		! Element =
		!   Description = Transaction date
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
	END RECORD
