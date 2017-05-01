	!
	! File Layout for: BT.BT_TUITION on 21-May-01
	!
	! Tuition Table
	!

	RECORD BT_TUITION_CDD
		! Element = CUSTOMER
		!   Description = Guardian
		STRING CUSNUM = 10
		! Element =
		!   Description = Child
		STRING CHILD = 40
		! Element = DATE
		!   Description = Effective from date
		STRING FROMDATE = 8
		! Element = DATE
		!   Description = Effective to date
		STRING TODATE = 8
		! Element =
		!   Description = Monthly Rate
		GFLOAT RATE
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element =
		!   Description = Monthly Rate
		GFLOAT DIAPER_RATE
	END RECORD
