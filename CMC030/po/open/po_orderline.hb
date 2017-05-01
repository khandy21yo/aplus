	!
	! File Layout for: PO.PO_ORDERLINE on 21-May-01
	!
	! PO journal line file
	!

	RECORD PO_ORDERLINE_CDD
		! Element = PO
		!   Description = Purchase order number
		STRING PO = 10
		! Element = LINE
		!   Description = Line
		STRING PO_LINE = 4
		! Element = PRODUCT
		!   Description = Our Product Number
		STRING OUR_PRODUCT = 14
		! Element = UOM
		!   Description = Our Unit of measurement
		STRING OUR_UOM = 2
		! Element = PRODUCT
		!   Description = Vendors Product Number
		STRING VEN_PRODUCT = 14
		! Element = DESCRIPTION
		!   Description = Product Description
		STRING DESCRIPTION = 40
		! Element =
		!   Description = Expected price
		GFLOAT VEN_PRICE
	END RECORD
