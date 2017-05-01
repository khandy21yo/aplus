	!
	! File Layout for: OE.OE_SALESTAX on 21-May-01
	!
	! Sales Tax Code Table
	!

	RECORD OE_SALESTAX_CDD
		! Element = TAXCODE
		!   Description = Tax code
		STRING TAXCODE = 2
		! Element = DESCRIPTION
		!   Description = Description
		STRING JURISDICTION = 20
		! Element = PERCENTAGE
		!   Description = State Tax Percentage
		GFLOAT STATETAX
		! Element = ACCOUNT
		!   Description = GL Account Number for State
		STRING STATEACC = 18
		! Element = PERCENTAGE
		!   Description = City Sales Tax Percentage
		GFLOAT CITYTAX
		! Element = ACCOUNT
		!   Description = GL Account Number for City Tax
		STRING CITYACC = 18
		! Element = PERCENTAGE
		!   Description = County Sales TAx Percentage
		GFLOAT COUNTYTAX
		! Element = ACCOUNT
		!   Description = GL Account Number for County Tax
		STRING COUNTYACC = 18
	END RECORD
