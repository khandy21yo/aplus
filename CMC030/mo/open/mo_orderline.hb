	!
	! File Layout for: MO.MO_ORDERLINE on 21-May-01
	!
	! Manufacturing Orders Model Lines
	!

	RECORD MO_ORDERLINE_CDD
		! Element =
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Model line Number
		STRING LIN = 4
		! Element =
		!   Description = Make of Dealer's Model
		STRING MAKE = 10
		! Element =
		!   Description = Year of Make
		STRING YEAR = 4
		! Element =
		!   Description = Type of Make
		STRING MTYPE = 2
		! Element =
		!   Description = Size of Make
		STRING MSIZE = 4
		! Element =
		!   Description = Model Code
		STRING MODELCODE = 4
		! Element =
		!   Description = Inventory Product
		STRING PRODUCT = 14
		! Element =
		!   Description = Quantity Ordered
		GFLOAT ORDQTY
		! Element =
		!   Description = Unit Price
		GFLOAT PRICE
		! Element =
		!   Description = Unit Cost
		GFLOAT COST
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING REQDATE = 8
		! Element =
		!   Description = Quantity to ship
		GFLOAT SHPQTY
		! Element =
		!   Description = Quantity on back order
		GFLOAT BCKQTY
		! Element = NOTES
		!   Description = Line notes
		STRING NOTES(1) = 40
		! Element =
		!   Description = Discount
		GFLOAT DISCOUNT
		! Element =
		!   Description = Serial Number
		STRING IDNUM = 10
	END RECORD
