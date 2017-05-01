	!
	! File Layout for: IC.IC_CYCLEJOUR on 21-May-01
	!
	! Cycle Counting Journal
	!

	RECORD IC_CYCLEJOUR_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Counting Date (YYYYMMDD)
		STRING COUNTDATE = 8
		! Element = TRANSTYPE
		!   Description = Transaction type code
		STRING TRANSTYPE = 2
		! Element = PRIMREF
		!   Description = Primary reference
		STRING PRIMREF = 8
		! Element = SECREF
		!   Description = Secondary reference
		STRING SECREF = 8
		! Element = CROSSREF
		!   Description = Cross reference number
		STRING CROSSREF = 10
		! Element = SUBACCT
		!   Description = Sub account (job number)
		STRING SUBACCT = 10
		! Element = STATIONMAN
		!   Description = Station man (operator)
		STRING STATIONMAN = 10
	END RECORD
