	!
	! File Layout for: OE.OE_INVLINE on 21-May-01
	!
	! Order Invoice Journal Line File
	!

	RECORD OE_INVLINE_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Line Number
		STRING LIN = 4
		! Element =
		!   Description = Quantity (shipped) Invoiced
		GFLOAT INVQTY
		! Element =
		!   Description = Quantity Cancelled
		GFLOAT CANCELQTY
		! Element =
		!   Description = Sales Price per Unit
		GFLOAT PRICE
		! Element =
		!   Description = Discount Percentage
		GFLOAT DISCOUNT
		! Element =
		!   Description = Unit Cost
		GFLOAT COST
		! Element =
		!   Description = Promo Amount Off
		GFLOAT PROMO
		! Element =
		!   Description = Misc Line Charges
		GFLOAT MISCH
		! Element =
		!   Description = Line Notes
		STRING NOTES = 40
	END RECORD
