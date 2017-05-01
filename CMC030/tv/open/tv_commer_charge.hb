	!
	! File Layout for: TV.TV_COMMER_CHARGE on 21-May-01
	!
	! Commercial Charge File
	!

	RECORD TV_COMMER_CHARGE_CDD
		! Element =
		!   Description = Form Number
		STRING FRMNUM = 8
		! Element =
		!   Description = Billable date
		STRING BILL_DATE = 8
		! Element =
		!   Description = Description
		STRING DESCR = 30
		! Element =
		!   Description = Dollar Amount
		GFLOAT AMOUNT
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCTNO = 18
		! Element =
		!   Description = GL period
		STRING PERIOD = 8
		! Element =
		!   Description = CO-OP Sponser Number
		STRING COOP = 10
		! Element =
		!   Description = Billed flag (Y/N)
		STRING BILL_FLAG = 1
		! Element =
		!   Description = Bill flag (% - percent, $ - dollars)
		STRING BILL_TYPE = 1
	END RECORD
