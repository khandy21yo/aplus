	!
	! File Layout for: IC.IC_BINMAP on 21-May-01
	!
	! Product Bin and Level Location
	!

	RECORD IC_BINMAP_CDD
		! Element = PRODUCT
		!   Description = Product number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Bin location
		STRING BIN(3) = 6
		! Element =
		!   Description = Safety stock
		GFLOAT SAFETY
		! Element =
		!   Description = Maximum stock level
		GFLOAT MAXLEVEL
		! Element =
		!   Description = ABC flag
		STRING ABC = 1
		! Element =
		!   Description = Cycle count map
		STRING CYCLEMAP = 8
	END RECORD
