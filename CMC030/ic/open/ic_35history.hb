	!
	! File Layout for: IC.IC_35HISTORY on 21-May-01
	!
	! Inventory Transaction History File
	!

	RECORD IC_35HISTORY_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Transaction type
		STRING TRANSTYPE = 2
		! Element = CROSSREF
		!   Description = Cross Reference
		STRING CROSSREF = 10
		! Element =
		!   Description = Subaccount
		STRING SUBACCT = 10
		! Element =
		!   Description = Period Quantity
		GFLOAT PQUANTITY(12)
		! Element =
		!   Description = Amount of Price
		GFLOAT PRICEAMT(12)
		! Element =
		!   Description = Amount of Cost of Sale
		GFLOAT COSTAMT(12)
	END RECORD
