	!
	! File Layout for: OE.OE_CREDITLINE on 21-May-01
	!
	! Credit Memo Line File
	!

	RECORD OE_CREDITLINE_CDD
		! Element =
		!   Description = Memo Number
		STRING MEMONUM = 8
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Total Credited Quantity
		GFLOAT CREDQTY
		! Element =
		!   Description = Qty Returned back into Inventory
		GFLOAT INVQTY
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
		!   Description = Miscellaneous Charges
		GFLOAT MISC
		! Element =
		!   Description = Reason Code
		STRING REASON = 2
	END RECORD
