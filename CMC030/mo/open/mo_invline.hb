	!
	! File Layout for: MO.MO_INVLINE on 21-May-01
	!
	! Manufacturing Order Invoice Line Journal File
	!

	RECORD MO_INVLINE_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Line Number
		STRING OLINE = 4
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
	END RECORD
