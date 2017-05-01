	!
	! File Layout for: AD.AD_CALCULATION on 21-May-01
	!
	! Asset Depreciation Ledger
	!

	RECORD AD_CALCULATION_CDD
		! Element = ASSET_NUM
		!   Description = Asset number
		STRING ASSET_NUM = 10
		! Element = DEP_OBJECT
		!   Description = Depreciation object
		STRING DEP_OBJECT = 1
		! Element =
		!   Description = Depreciation status (active,retired)
		STRING DEP_STATUS = 1
		! Element =
		!   Description = Current depreciated dollars
		GFLOAT AMOUNT_CUR
		! Element =
		!   Description = Current units
		GFLOAT UNIT_CUR
	END RECORD
