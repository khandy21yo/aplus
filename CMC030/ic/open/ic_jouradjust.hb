	!
	! File Layout for: IC.IC_JOURADJUST on 21-May-01
	!
	! Inventory Adjustment Journal
	!

	RECORD IC_JOURADJUST_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Adjusted quantity
		GFLOAT QUANTITY
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
