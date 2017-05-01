	!
	! File Layout for: OS.OS_JOURNAL on 13-Apr-99
	!
	! Main Journal Header
	!

	RECORD OS_JOURNAL_CDD
		! Element = TRANKEY
		!   Description = Transaction key
		STRING TRANS = 6
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSTOMER = 10
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVOICE = 8
		! Element = DATE
		!   Description = Transaction Date (YYYYMMDD)
		STRING TRANDATE = 8
		! Element =
		!   Description = Sales Tax
		GFLOAT SALESTAX
		! Element =
		!   Description = Prepayment
		GFLOAT PAYMENT
		! Element = DEPOSIT
		!   Description = Deposit number
		STRING DEPOSIT = 6
	END RECORD
