	!
	! File Layout for: WP.WP_CLOSEJOUR on 21-May-01
	!
	! Job Close Journal Header
	!

	RECORD WP_CLOSEJOUR_CDD
		! Element = JOB
		!   Description = Job number
		STRING JOB = 10
		! Element = DATE
		!   Description = Closing Date (YYYYMMDD)
		STRING CLOSEDATE = 8
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
		! Element =
		!   Description = STD Inventory Parts
		GFLOAT STDPARTS
		! Element =
		!   Description = Actual Inventory Parts
		GFLOAT ACTPARTS
		! Element =
		!   Description = STD Raw Material
		GFLOAT STDRAWMAT
		! Element =
		!   Description = Actual raw Material
		GFLOAT ACTRAWMAT
		! Element =
		!   Description = STD Labor
		GFLOAT STDLABOR
		! Element =
		!   Description = Actual labor
		GFLOAT ACTLABOR
		! Element =
		!   Description = Standard Burden
		GFLOAT STDBURDEN
		! Element =
		!   Description = Actual Burden
		GFLOAT ACTBURDEN
		! Element =
		!   Description = Variance Flag
		STRING VARFLAG = 1
	END RECORD
