	!
	! File Layout for: PP.PP_MONTHLY on 21-May-01
	!
	! Pacific Pride Monthly Transaction File
	!

	RECORD PP_MONTHLY_CDD
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = CARD
		!   Description = Pacific Pride Vehicle Card Number
		STRING VEHICLE = 8
		! Element = CARD
		!   Description = Pacific Pride Driver Card Number
		STRING DRIVER = 8
		! Element = DATE
		!   Description = Transaction Date (YYYYMMDD)
		STRING TRANDATE = 8
		! Element = TIME
		!   Description = Transaction Time (HHMMSS)
		STRING TRANTIME = 6
		! Element =
		!   Description = Host Number
		STRING HOST = 4
		! Element =
		!   Description = Site Number
		STRING SITE = 4
		! Element =
		!   Description = Site Type
		STRING STYPE = 1
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = UOM
		!   Description = Unit of measurement
		STRING UOM = 2
		! Element =
		!   Description = Quantity Sold
		GFLOAT QUANTITY
		! Element =
		!   Description = Odometer reading
		GFLOAT ODOM
		! Element =
		!   Description =
		STRING SLTYPE = 1
		! Element =
		!   Description =
		STRING FTYPE = 1
		! Element =
		!   Description = Selling Price
		GFLOAT SELLPRICE
		! Element =
		!   Description = Transaction Cost
		GFLOAT TRANCOST
		! Element =
		!   Description = Misc. keyboard entry
		STRING MISCKEYB = 9
		! Element =
		!   Description =
		STRING TRNTYPE = 2
		! Element =
		!   Description = Discount
		STRING DISCOUNT = 4
		! Element = DATE
		!   Description = icb Date (YYYYMMDD)
		STRING ICBDATE = 8
		! Element =
		!   Description = Transaction number
		STRING TRNNUM = 5
		! Element =
		!   Description = Sales Tax Rate
		GFLOAT STAXRATE
		! Element =
		!   Description = Pump Number
		STRING PUMP = 2
		! Element =
		!   Description =
		STRING BUYFRAN = 4
		! Element = DATE
		!   Description = Capture Date (YYYYMMDD)
		STRING CAPDATE = 8
		! Element = TIME
		!   Description = Capture Time (HHMMSS)
		STRING CAPTIME = 6
		! Element =
		!   Description =
		STRING POSTBNUM = 4
		! Element =
		!   Description =
		STRING TRANSOURCE = 1
		! Element =
		!   Description =
		STRING EDITACT = 1
		! Element =
		!   Description =
		STRING JULIANDAY = 3
		! Element =
		!   Description =
		STRING RSTATION = 1
		! Element = STATE
		!   Description = State
		STRING STATE = 2
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
		! Element =
		!   Description = PP Customer Number
		STRING IDENTITY = 8
	END RECORD
