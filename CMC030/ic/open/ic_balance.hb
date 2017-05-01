	!
	! File Layout for: IC.IC_BALANCE on 21-May-01
	!
	! Product Balance File
	!

	RECORD IC_BALANCE_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING PERIOD = 6
		! Element =
		!   Description = Record type
		STRING RECTYPE = 2
		! Element =
		!   Description = Transaction class
		STRING CLASS = 2
		! Element =
		!   Description = Quantity
		GFLOAT QUANTITY
		! Element = DATE
		!   Description = Post date (YYYYMMDD)
		STRING POSTDATE = 8
		! Element = TIME
		!   Description = Post time (HHMMSS)
		STRING POSTTIME = 6
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
	END RECORD
