	!
	! File Layout for: AR.AR_SJL on 21-May-01
	!
	! Sales Journal (Lines)
	!

	RECORD AR_SJL_CDD
		! Element = INVNUM
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element =
		!   Description = Line number
		STRING SLINE = 3
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element = SUBACC
		!   Description = Sub account (job number)
		STRING SUBACCT = 10
		! Element =
		!   Description = Amount
		GFLOAT AMOUNT
		! Element =
		!   Description = Quanity
		GFLOAT QTY
		! Element =
		!   Description = Line type
		STRING LTYPE = 1
		! Element =
		!   Description = Tax type
		STRING TAXTYP = 1
		! Element =
		!   Description = Description
		STRING DESCR = 26
	END RECORD
