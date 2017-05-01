	!
	! File Layout for: AD.AD_REGUNIT on 21-May-01
	!
	! Depreciation Unit Register
	!

	RECORD AD_REGUNIT_CDD
		! Element = ASSET_NUM
		!   Description = Asset number
		STRING ASSET_NUM = 10
		! Element = DEP_OBJECT
		!   Description = Depreciation object
		STRING DEP_OBJECT = 1
		! Element = PERIOD
		!   Description = Period (YYYYPP)
		STRING PERIOD = 6
		! Element = DATE
		!   Description = Date
		STRING ACTION_DATE = 8
		! Element =
		!   Description = Station man
		STRING STATIONMAN = 10
		! Element =
		!   Description = Quantity, units
		GFLOAT QUANTITY
		! Element = DATE
		!   Description = Post date
		STRING POST_DATE = 8
		! Element = TIME
		!   Description = Time (HHMMSS)
		STRING POST_TIME = 6
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
	END RECORD
