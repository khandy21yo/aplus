	!
	! File Layout for: OE.OE_ORDERLINE on 21-May-01
	!
	! Sales Order Line File
	!

	RECORD OE_ORDERLINE_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Quantity Ordered
		GFLOAT ORDQTY
		! Element =
		!   Description = Quantity to Ship
		GFLOAT SHPQTY
		! Element =
		!   Description = Unit Price
		GFLOAT PRICE
		! Element =
		!   Description = Discount Percentage
		GFLOAT DISCOUNT
		! Element =
		!   Description = Unit Cost
		GFLOAT COST
		! Element = DATE
		!   Description = Request Date (YYYYMMDD)
		STRING REQDATE = 8
		! Element =
		!   Description = Promo Amount
		GFLOAT PROMO
		! Element =
		!   Description = Miscellaneous Charges
		GFLOAT MISCH
		! Element =
		!   Description = Quantity on Backorder
		GFLOAT BCKQTY
		! Element = NOTES
		!   Description = Notes
		STRING NOTES1 = 40
		! Element = NOTES
		!   Description = Notes
		STRING NOTES2 = 30
		! Element = SUBACCT
		!   Description = Sub account (job number)/Serial Number
		STRING SUBACCT = 10
		! Element = LINE
		!   Description = Line number
		STRING LIN = 4
		! Element =
		!   Description = Miscellaneous Charges (2)
		GFLOAT MISCH2
	END RECORD
