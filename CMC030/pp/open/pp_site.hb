	!
	! File Layout for: PP.PP_SITE on 21-May-01
	!
	! Pacific Pride Site File
	!

	RECORD PP_SITE_CDD
		! Element =
		!   Description = Host #
		STRING HOST = 4
		! Element =
		!   Description = Site Code
		STRING SITE = 4
		! Element =
		!   Description = site Type
		STRING STYPE = 1
		! Element = NAME
		!   Description = Site Name
		STRING SNAME = 30
		! Element = ADDRESS
		!   Description = Address Line
		STRING ADDRESS = 25
		! Element = CITY
		!   Description = City
		STRING CITY = 15
		! Element = STATE
		!   Description = State
		STRING STATE = 2
		! Element = ZIP
		!   Description = Zip code
		STRING ZIP = 10
		! Element =
		!   Description = Local Sale Location
		STRING LOCSALE = 3
		! Element =
		!   Description = Foreign Sale Location
		STRING FORSALE = 3
		! Element =
		!   Description = Foreign Purchase Location
		STRING FORPUR = 3
	END RECORD
