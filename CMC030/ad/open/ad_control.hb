	!
	! File Layout for: AD.AD_CONTROL on 21-May-01
	!
	! Asset Depreciation Control File
	!

	RECORD AD_CONTROL_CDD
		! Element = DEP_OBJECT
		!   Description = Depreciation object to the GL
		STRING DEP_OBJECT = 1
		! Element =
		!   Description = Last period updated
		STRING LASTPER = 6
	END RECORD
