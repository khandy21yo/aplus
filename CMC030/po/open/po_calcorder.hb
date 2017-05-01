	!
	! File Layout for: PO.PO_CALCORDER on 21-May-01
	!
	! Calculate PO Order Journal
	!

	RECORD PO_CALCORDER_CDD
		! Element = VENDOR
		!   Description = Vendor Number
		STRING VENDOR = 10
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = 10 Quanities used in calculating order
		GFLOAT QUANITY(10)
		! Element =
		!   Description = Quanity to order
		GFLOAT ORDER
		! Element =
		!   Description = Cost from vendor
		GFLOAT COST
	END RECORD
