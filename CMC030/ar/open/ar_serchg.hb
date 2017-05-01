	!
	! File Layout for: AR.AR_SERCHG on 21-May-01
	!
	! Service Charge Definition
	!

	RECORD AR_SERCHG_CDD
		! Element =
		!   Description = Country
		STRING COUNTRY = 2
		! Element =
		!   Description = State
		STRING STATE = 2
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING SCREV = 18
		! Element =
		!   Description = Service charge percentage
		GFLOAT SERCHG
		! Element =
		!   Description = Minimum chargeable
		GFLOAT MINIMUM
		! Element =
		!   Description = Dollar amount charge
		GFLOAT DOLLAR
		! Element =
		!   Description = Start Period for Service Charge
		WORD SPERIOD
	END RECORD
