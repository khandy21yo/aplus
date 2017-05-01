	!
	! File Layout for: OE.OE_SHIPLINE on 21-May-01
	!
	! Shipping Journal Line File
	!

	RECORD OE_SHIPLINE_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Line Number
		STRING LIN = 4
		! Element =
		!   Description = Quantity to Ship
		GFLOAT SHPQTY
		! Element =
		!   Description = Canceled Quantity
		GFLOAT CANCELQTY
	END RECORD
