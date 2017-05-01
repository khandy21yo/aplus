	!
	! File Layout for: IC.IC_HISTORY on 21-May-01
	!
	! Product Balance History
	!

	RECORD IC_HISTORY_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Period (YYYYPP)
		STRING PERIOD = 6
		! Element =
		!   Description = Transaction type
		STRING TRANSTYPE = 2
		! Element =
		!   Description = Beginning Quantity
		GFLOAT BQUANTITY
		! Element =
		!   Description = Period Posted Quantity
		GFLOAT PQUANTITY
		! Element = DATE
		!   Description = Post date
		STRING POSTDATE = 8
		! Element = TIME
		!   Description = Post time
		STRING POSTTIME = 6
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
		! Element =
		!   Description = Amount of Sale
		GFLOAT SALEAMT
		! Element =
		!   Description = Amount of Cost of Sale
		GFLOAT COSTAMT
	END RECORD
