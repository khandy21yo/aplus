	!
	! File Layout for: AR.AR_LBCRJL on 21-May-01
	!
	! Cash Receipts Journal (Line)
	!

	RECORD AR_LBCRJL_CDD
		! Element =
		!   Description = Receipt number
		STRING RECNUM = 8
		! Element =
		!   Description = Line number
		STRING LLINE = 3
		! Element =
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element =
		!   Description = Amount
		GFLOAT AMOUNT
		! Element =
		!   Description = Transaction Type
		STRING TRATYP = 1
		! Element = MATTER
		!   Description = Matter Number
		STRING MATTER_NUM = 10
		! Element =
		!   Description = Flag for Type of allocation
		STRING ALLOCATE = 1
		! Element =
		!   Description = Staff Number
		STRING STAFF = 10
	END RECORD
