	!
	! File Layout for: AR.AR_CUSBAL on 21-May-01
	!
	! Customer Balances
	!

	RECORD AR_CUSBAL_CDD
		! Element = CUSNUM
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element =
		!   Description = Credit Limit
		GFLOAT CREDIT
		! Element =
		!   Description = Aging periods
		GFLOAT AGING(4)
		! Element =
		!   Description = Future amount
		GFLOAT FUTURE
		! Element =
		!   Description = YTD Service charge
		GFLOAT YTDSERVICE
		! Element =
		!   Description = PTD Sales
		GFLOAT LAST_PAID
		! Element =
		!   Description = YTD Sales
		GFLOAT YTDSALES
		! Element =
		!   Description = Service charge
		GFLOAT CHARGE
		! Element =
		!   Description = Last Service Charge date
		STRING LAST_CHARGE = 8
		! Element =
		!   Description = Last Payment date
		STRING LAST_PAYMENT = 8
		! Element =
		!   Description = Last update
		STRING LAST_UPDATE = 8
	END RECORD
