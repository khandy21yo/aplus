	!
	! File Layout for: RM.RM_JOURNAL on 21-May-01
	!
	! Restaurant Journal
	!

	RECORD RM_JOURNAL_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = First date from worksheet
		STRING STARTDATE = 8
		! Element = TRANSTYPE
		!   Description = Transaction type code
		STRING TRANSTYPE = 2
		! Element = PRICETYPE
		!   Description = Price type
		STRING PRICETYPE = 2
		! Element =
		!   Description = Station man
		STRING STATIONMAN = 10
		! Element = ACCOUNT
		!   Description = Expanse account number
		STRING EXPACCOUNT = 18
	END RECORD
