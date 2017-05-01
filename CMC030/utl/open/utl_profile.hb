	!
	! File Layout for: UTL.UTL_PROFILE on 21-May-01
	!
	! Company Profile
	!

	RECORD UTL_PROFILE_CDD
		! Element =
		!   Description = Company name for menu
		STRING MENU_NAME = 30
		! Element =
		!   Description = Company name for report
		STRING REP_NAME = 30
		! Element = LOCATION
		!   Description = Main Office Location number
		STRING MAINLOCATION = 4
		! Element = LOCATION
		!   Description = Default Location number
		STRING DEFLOCATION = 4
	END RECORD
