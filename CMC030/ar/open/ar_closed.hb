	!
	! File Layout for: AR.AR_CLOSED on 21-May-01
	!
	! Closed Accounts Receivable Ledger
	!

	RECORD AR_CLOSED_CDD
		! Element = CUSNUM
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = INVNUM
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element =
		!   Description = Transaction Type
		STRING TRATYP = 2
		! Element =
		!   Description = Transaction date
		STRING TRADAT = 8
		! Element =
		!   Description = Sale amount
		GFLOAT SALAMT
		! Element =
		!   Description = Discount amount
		GFLOAT DISAMT
		! Element =
		!   Description = Other charges (Sales tax)
		GFLOAT OTHCHG
		! Element =
		!   Description = Receipt number
		STRING RECNUM = 8
		! Element =
		!   Description = Check number
		STRING CHKNUM = 6
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ARACCT = 18
		! Element = SUBACC
		!   Description = Sub account (job number)
		STRING SUBACC = 10
		! Element =
		!   Description = Description
		STRING DESCR = 25
		! Element =
		!   Description = Salesperson number
		STRING SALNUM = 10
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
		! Element =
		!   Description = (PPYYYY)
		STRING UPDATED = 6
		! Element =
		!   Description = (PPYYYY)
		STRING CLOSEDATE = 6
		! Element = DATE
		!   Description = Due Date (YYYYMMDD)
		STRING DUEDATE = 8
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING DISCOUNTDATE = 8
	END RECORD
