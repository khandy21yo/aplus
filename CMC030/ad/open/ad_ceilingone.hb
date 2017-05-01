	!
	! File Layout for: AD.AD_CEILINGONE on 21-May-01
	!
	! One Dimensional Optional Ceiling Table
	!

	RECORD AD_CEILINGONE_CDD
		! Element = OPTTABLE
		!   Description = Depreciation optional table code
		STRING OPTTABLE = 6
		! Element = DATE
		!   Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
		! Element =
		!   Description = Deprecition year
		STRING DEP_YEAR = 2
		! Element =
		!   Description = Ceiling amount
		GFLOAT CEILING
	END RECORD
