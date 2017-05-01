	!
	! File Layout for: OE.OE_PROMO on 21-May-01
	!
	! Promotional Sales Master File
	!

	RECORD OE_PROMO_CDD
		! Element = REFNO
		!   Description = Reference number
		STRING REFPROMO = 16
		! Element = DATE
		!   Description = From Date (YYYYMMDD)
		STRING FROMDATE = 8
		! Element = DATE
		!   Description = To Date (YYYYMMDD)
		STRING TODATE = 8
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
	END RECORD
