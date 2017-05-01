	!
	! File Layout for: PP.PP_CONTROL on 21-May-01
	!
	! Pacific Pride Control File
	!

	RECORD PP_CONTROL_CDD
		! Element = CUSTOMER
		!   Description = System Customer Number
		STRING CUSNUM = 10
		! Element = ACCOUNT
		!   Description = A/R General Ledger Account Number
		STRING AR_ACCOUNT = 18
		! Element =
		!   Description = Host Number
		STRING HOST_NUM = 4
		! Element =
		!   Description = Dsicount Days
		STRING DIS_DAYS = 2
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING INVDATE = 8
		! Element =
		!   Description = Last Invoice Number
		STRING LAST_INV = 8
	END RECORD
