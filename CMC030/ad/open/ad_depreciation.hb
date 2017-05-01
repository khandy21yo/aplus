	!
	! File Layout for: AD.AD_DEPRECIATION on 21-May-01
	!
	! Asset Depreciation File
	!

	RECORD AD_DEPRECIATION_CDD
		! Element = ASSET_NUM
		!   Description = Asset number
		STRING ASSET_NUM = 10
		! Element = DEP_OBJECT
		!   Description = Depreciation object
		STRING DEP_OBJECT = 1
		! Element = DEPCLASS
		!   Description = Depreciation class code
		STRING DEPCLASS = 4
	END RECORD
