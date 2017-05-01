	!
	! File Layout for: IC.IC_STOREMAP on 21-May-01
	!
	! Cycle Count Map by Day
	!

	RECORD IC_STOREMAP_CDD
		! Element = PRODUCT_NUM
		!   Description = Product number
		STRING PRODUCT_NUM = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Bin location
		STRING BIN(3) = 6
		! Element =
		!   Description = ABC flag
		STRING ABC = 1
		! Element =
		!   Description = Cycle daily map
		STRING CYCLE_MAP(11) = 4
	END RECORD
