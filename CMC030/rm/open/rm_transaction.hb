	!
	! File Layout for: RM.RM_TRANSACTION on 21-May-01
	!
	! Restaurant Transaction File
	!

	RECORD RM_TRANSACTION_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING TRANSDATE = 8
		! Element = TRANSTYPE
		!   Description = Transaction type code
		STRING TRANSTYPE = 2
		! Element =
		!   Description = Quantity
		GFLOAT QUANTITY
		! Element =
		!   Description = Price
		GFLOAT PRICE
		! Element =
		!   Description = Cost
		GFLOAT COST
		! Element = STATIONMAN
		!   Description = Station man (operator)
		STRING STATIONMAN = 10
		! Element = DATE
		!   Description = Post date
		STRING POSTDATE = 8
		! Element = TIME
		!   Description = Post time
		STRING POSTTIME = 6
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
	END RECORD
