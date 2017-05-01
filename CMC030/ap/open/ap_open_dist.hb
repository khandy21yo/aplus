	!
	! File Layout for: AP.AP_OPEN_DIST on 21-May-01
	!
	! Accounts Payable Open Distribution
	!

	RECORD AP_OPEN_DIST_CDD
		! Element =
		!   Description = Transaction key
		STRING TRANKEY = 6
		! Element =
		!   Description = Must be reset
		STRING SLINE = 4
		! Element =
		!   Description = PO number
		STRING PONUM = 10
		! Element =
		!   Description = Must be reset
		STRING PO_LINE = 4
		! Element =
		!   Description = Account number
		STRING ACCT = 18
		! Element =
		!   Description = Subaccount
		STRING SUBACC = 10
		! Element =
		!   Description = Operation
		STRING OPERATION = 8
		! Element =
		!   Description = Units
		GFLOAT UNITS
		! Element =
		!   Description = Amount
		GFLOAT AMOUNT
		! Element =
		!   Description = Discount amount
		GFLOAT DISCAMT
		! Element =
		!   Description = Flag is either (y/N)
		STRING USE_TAX_FLAG = 1
		! Element =
		!   Description = Batch update number
		STRING BTHNUM = 6
	END RECORD
