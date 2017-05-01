	!
	! File Layout for: OE.OE_CREASON on 02-Jul-03
	!
	! Reason Code Description
	!

	RECORD OE_CREASON_CDD
		! Element =
		!   Description =
		STRING CREASON = 2
		! Element =
		!   Description = DESCRIPTION
		STRING DESCR = 40
		! Element = TAXABLE
		!   Description = Taxable Flag
		STRING TAXABLE =    1
		! Element =
		!   Description = Unused space
		STRING CR_ACCT =   17
	END RECORD
