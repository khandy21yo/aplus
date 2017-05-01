	!
	! File Layout for: OE.OE_PRODPROMO on 21-May-01
	!
	! Product Promotion
	!

	RECORD OE_PRODPROMO_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = REFNO
		!   Description = Reference number
		STRING REFPROMO = 16
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSTOMER = 10
		! Element = CUSTYPE
		!   Description = Customer Type
		STRING CUSTYPE = 2
		! Element =
		!   Description = Customer Category
		STRING CUSTCAT = 4
		! Element = SALESMAN
		!   Description = Salesperson number
		STRING SALESMAN = 10
		! Element =
		!   Description = Promo Dollar Amount
		GFLOAT PROMODOLL
		! Element =
		!   Description = Promo Percentage
		GFLOAT PROMOPERC
	END RECORD
