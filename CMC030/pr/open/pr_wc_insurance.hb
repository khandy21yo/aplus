	!
	! File Layout for: PR.PR_WC_INSURANCE on 21-May-01
	!
	! Workman Comp Insurance Rate
	!

	RECORD PR_WC_INSURANCE_CDD
		! Element =
		!   Description =
		STRING CODE = 6
		! Element =
		!   Description =
		STRING STATE = 2
		! Element =
		!   Description = Insurance type (liability ins, etc.)
		STRING INS_TYPE = 2
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING EFFDAT = 8
		! Element =
		!   Description = Method (1-hour, 2-day)
		STRING METHOD = 1
		! Element =
		!   Description = Employee rate
		GFLOAT EMPLE_RATE
		! Element =
		!   Description = Employer rate
		GFLOAT EMPLR_RATE
		! Element =
		!   Description =
		GFLOAT MAXQHOURS
	END RECORD
