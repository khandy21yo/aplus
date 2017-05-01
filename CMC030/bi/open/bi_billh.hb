	!
	! File Layout for: BI.BI_BILLH on 21-May-01
	!
	! Insurance Journal Header
	!

	RECORD BI_BILLH_CDD
		! Element = INSURED
		!   Description = Insured number
		STRING INSURED = 10
		! Element = PATIENT
		!   Description = Patient Number
		STRING PATIENT = 10
		! Element = STATIONMAN
		!   Description = Station man (operator)
		STRING STATIONMAN = 10
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element = DATE
		!   Description = Invoice Date (YYYYMMDD)
		STRING INVDATE = 8
	END RECORD
