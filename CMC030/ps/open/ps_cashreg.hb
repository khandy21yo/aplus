	!
	! File Layout for: PS.PS_CASHREG on 21-May-01
	!
	! Cash Register Description Table
	!

	RECORD PS_CASHREG_CDD
		! Element = CASHREG
		!   Description = Cash Register Number
		STRING CASHREG = 4
		! Element =
		!   Description = Description
		STRING DESCR = 20
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = NOTES
		!   Description = Notes
		STRING NOTES(1) = 40
		! Element =
		!   Description = Last Invoice Number
		STRING LAST_INVNUM = 8
		! Element = ACCOUNT
		!   Description = Petty Cash Account Number
		STRING PETTYCASH = 18
	END RECORD
