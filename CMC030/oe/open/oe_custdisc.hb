	!
	! File Layout for: OE.OE_CUSTDISC on 21-May-01
	!
	! Customer Product Discount
	!

	RECORD OE_CUSTDISC_CDD
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Product Number
		STRING PRODUCT = 20
		! Element =
		!   Description = Wildcard Product Type
		STRING PRODTYPE = 20
		! Element =
		!   Description = Wildcard Product Category
		STRING PRODCAT = 20
		! Element = PRICETYPE
		!   Description = Price type
		STRING PRICETYPE = 2
		! Element = DISCOUNT
		!   Description = Discount percentage
		GFLOAT DISCOUNT
	END RECORD
