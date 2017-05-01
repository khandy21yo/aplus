	!
	! File Layout for: AR.AR_SALTAX on 21-May-01
	!
	! Sales Tax Table
	!

	RECORD AR_SALTAX_CDD
		! Element = COUNTRY
		!   Description = Country
		STRING COUNTRY = 2
		! Element = STATE
		!   Description = State
		STRING STATE = 2
		! Element = COUNTY
		!   Description = County
		STRING COUNTY = 2
		! Element =
		!   Description = Sales Tax Percentage
		GFLOAT PERCENT
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
