	!
	! File Layout for: AD.AD_METHOD on 21-May-01
	!
	! Depreciation Method Description
	!

	RECORD AD_METHOD_CDD
		! Element = DEP_METHOD
		!   Description = Depreciation method
		STRING DEP_METHOD = 4
		! Element =
		!   Description = Method description
		STRING DESCRIPTION = 40
		! Element =
		!   Description = Method calculation code
		STRING CALCULATION = 2
	END RECORD
