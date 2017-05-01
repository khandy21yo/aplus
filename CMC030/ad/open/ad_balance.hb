	!
	! File Layout for: AD.AD_BALANCE on 21-May-01
	!
	! Asset Depreciation Balances
	!

	RECORD AD_BALANCE_CDD
		! Element = ASSET_NUM
		!   Description = Asset number
		STRING ASSET_NUM = 10
		! Element = DEP_OBJECT
		!   Description = Depreciation object
		STRING DEP_OBJECT = 1
		! Element =
		!   Description = Depreciation status (active,retired..)
		STRING DEP_STATUS = 1
		! Element =
		!   Description = Total depreciated dollars
		GFLOAT AMOUNT_CTD
		! Element =
		!   Description = Total units
		GFLOAT UNIT_CTD
		! Element = PERIOD
		!   Description = Last period updated
		STRING LASTPER = 6
	END RECORD
