	!
	! File Layout for: BC.BC_BILLH on 21-May-01
	!
	! Billing to Customer Billing Journal Header
	!

	RECORD BC_BILLH_CDD
		! Element =
		!   Description = Order number
		STRING ORDER = 8
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING SHPNUM = 10
		! Element =
		!   Description = Salesman
		STRING SALEMAN = 10
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING ORDERDATE = 8
		! Element =
		!   Description = Ship Via
		STRING SHPVIA = 20
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element =
		!   Description = Terms
		STRING TERMS = 16
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element = REFNUM
		!   Description = Reference Number
		STRING REFNUM = 20
	END RECORD
