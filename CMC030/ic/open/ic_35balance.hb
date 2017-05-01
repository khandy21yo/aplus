	!
	! File Layout for: IC.IC_35BALANCE on 21-May-01
	!
	! Inventory Product Balance File
	!

	RECORD IC_35BALANCE_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = TRANSTYPE
		!   Description = Transaction type code
		STRING TRANSTYPE = 2
		! Element =
		!   Description = Beginning Balance
		GFLOAT BBALANCE
		! Element =
		!   Description = Posted Quantity
		GFLOAT PBALANCE
		! Element =
		!   Description = Running Balance (journals)
		GFLOAT RBALANCE
		! Element =
		!   Description = Control Balance
		GFLOAT CBALANCE
	END RECORD
