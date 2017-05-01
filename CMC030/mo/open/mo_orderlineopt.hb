	!
	! File Layout for: MO.MO_ORDERLINEOPT on 21-May-01
	!
	! Manufacturing Order Line File
	!

	RECORD MO_ORDERLINEOPT_CDD
		! Element =
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Model record line number
		STRING LIN = 4
		! Element =
		!   Description = Option Group
		STRING OPTGROUP = 2
		! Element =
		!   Description = Option Code
		STRING OPTN = 4
		! Element =
		!   Description = Order Quantity
		GFLOAT ORDQTY
		! Element =
		!   Description = Cost Per Unit
		GFLOAT COST
		! Element =
		!   Description = Price Per Unit
		GFLOAT PRICE
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Option Description
		STRING OPTDESCR = 40
		! Element =
		!   Description = Option Line number
		STRING LINOPT = 4
		! Element =
		!   Description = Quantity to ship
		GFLOAT SHPQTY
		! Element =
		!   Description = Quantity on backorder
		GFLOAT BCKQTY
		! Element = MAKE
		!   Description = Make
		STRING MAKE = 10
		! Element =
		!   Description = Model Code
		STRING MODELCODE = 4
	END RECORD
