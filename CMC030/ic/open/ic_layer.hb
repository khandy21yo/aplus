	!
	! File Layout for: IC.IC_LAYER on 21-May-01
	!
	! Product Cost Layer
	!

	RECORD IC_LAYER_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING TRANSDATE = 8
		! Element =
		!   Description = Inventory cost
		GFLOAT COST
		! Element =
		!   Description = Quantity
		GFLOAT QUANTITY
		! Element =
		!   Description = Check number
		STRING CHECK = 6
		! Element =
		!   Description = Vendor Name
		STRING VENDORALF = 15
		! Element =
		!   Description = Invoice Number
		STRING INVOICE = 15
		! Element = DATE
		!   Description = Post Date (YYYYMMDD)
		STRING POSTDATE = 8
		! Element = TIME
		!   Description = Post Time (HHMMSS)
		STRING POSTTIME = 6
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
	END RECORD
