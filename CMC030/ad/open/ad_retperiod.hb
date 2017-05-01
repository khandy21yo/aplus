	!
	! File Layout for: AD.AD_RETPERIOD on 21-May-01
	!
	! Retired Asset Last Period
	!

	RECORD AD_RETPERIOD_CDD
		! Element = ASSET_NUM
		!   Description = Asset number
		STRING ASSET_NUM = 10
		! Element = DEP_OBJECT
		!   Description = Depreciation object
		STRING DEP_OBJECT = 1
		! Element = PERIOD
		!   Description = Period (YYYYPP)
		STRING PERIOD = 6
	END RECORD
