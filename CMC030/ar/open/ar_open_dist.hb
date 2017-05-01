	!
	! File Layout for: AR.AR_OPEN_DIST on 21-May-01
	!
	! Accounts Receivable Open Distribution File
	!

	RECORD AR_OPEN_DIST_CDD
		! Element = INVNUM
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
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
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING UPDATED = 6
		! Element =
		!   Description = Working Staff #
		STRING STAFF_NUM = 10
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING POST_DATE = 8
		! Element = TIME
		!   Description = Time (HHMMSS)
		STRING POST_TIME = 6
	END RECORD
