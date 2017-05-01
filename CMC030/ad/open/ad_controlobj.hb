	!
	! File Layout for: AD.AD_CONTROLOBJ on 21-May-01
	!
	! Object Control File
	!

	RECORD AD_CONTROLOBJ_CDD
		! Element = DEP_OBJECT
		!   Description = Depreciation object
		STRING DEP_OBJECT = 1
		! Element = ERA
		!   Description = Era code
		STRING ERA = 2
		! Element =
		!   Description = Last period updated
		STRING LASTPER = 6
		! Element =
		!   Description = Last period depreciated
		STRING LASTDEP = 6
		! Element = STATUS_FLAG
		!   Description = Status flag in the control files
		STRING STATUS_FLAG = 1
	END RECORD
