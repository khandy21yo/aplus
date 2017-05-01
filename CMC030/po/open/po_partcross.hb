	!
	! File Layout for: PO.PO_PARTCROSS on 21-May-01
	!
	! Vendor Part Cross Reference
	!

	RECORD PO_PARTCROSS_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = VENDOR
		!   Description = Vendor Number
		STRING VENDOR = 10
		! Element = PRODUCT
		!   Description = Product Number
		STRING VENPROD = 14
		! Element = UOM
		!   Description = Unit of measurement
		STRING VENUOM = 2
		! Element =
		!   Description = Vendor Conversion Factor
		GFLOAT VENFAC
		! Element =
		!   Description = Our conversion vactor
		GFLOAT FACTOR
		! Element =
		!   Description = Product Description
		STRING DESCR = 40
		! Element =
		!   Description = Lead Time
		WORD LEAD
		! Element =
		!   Description = Minimum order quantity
		GFLOAT MINQTY
		! Element =
		!   Description = Priority Code
		STRING PRIORITY = 1
	END RECORD
