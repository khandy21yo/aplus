	!
	! File Layout for: AD.AD_HISTORY on 21-May-01
	!
	! Asset Depreciation History
	!

	RECORD AD_HISTORY_CDD
		! Element = ASSET_NUM
		!   Description = Asset number
		STRING ASSET_NUM = 10
		! Element = DEP_OBJECT
		!   Description = Depreciation object
		STRING DEP_OBJECT = 1
		! Element = DEP_STATUS
		!   Description = Prior the asset activity status
		STRING DEP_STATUS = 1
		! Element =
		!   Description = Depreciation amount
		GFLOAT AMOUNT_HIS
		! Element =
		!   Description = Depreciation units
		GFLOAT UNIT_HIS
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING PERIOD = 6
	END RECORD
