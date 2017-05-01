	!
	! File Layout for: IC.IC_JOURCOUNT on 21-May-01
	!
	! Inventory Cycle Count Entry Journal
	!

	RECORD IC_JOURCOUNT_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Quantity count
		GFLOAT QUANTITY
		! Element =
		!   Description = Cycle Count Control Number
		STRING CONTROL = 6
	END RECORD
