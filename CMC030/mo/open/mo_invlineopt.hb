	!
	! File Layout for: MO.MO_INVLINEOPT on 21-May-01
	!
	! Manufacturing Invoice Line Options File
	!

	RECORD MO_INVLINEOPT_CDD
		! Element =
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Order Line Number
		STRING OLINE = 4
		! Element =
		!   Description = Order Line Option Number
		STRING OPTLINE = 4
		! Element =
		!   Description = Quantity Invoiced (Shipped)
		GFLOAT INVQTY
		! Element =
		!   Description = Quantity Cancelled
		GFLOAT CANCELQTY
		! Element =
		!   Description = Sales Price Per Unit
		GFLOAT PRICE
		! Element =
		!   Description = Discount Percentage
		GFLOAT DISCOUNT
		! Element =
		!   Description = Unit Cost
		GFLOAT COST
	END RECORD
