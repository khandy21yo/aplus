	!
	! File Layout for: AD.AD_TABLEONE on 21-May-01
	!
	! One Dimensional Optional Depreciation Table
	!

	RECORD AD_TABLEONE_CDD
		! Element = OPTTABLE
		!   Description = Depreciation optional table code
		STRING OPTTABLE = 6
		! Element = DATE
		!   Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
		! Element =
		!   Description = Number of years
		STRING YEARS = 4
		! Element =
		!   Description = Deprecition year
		STRING DEP_YEAR = 2
		! Element =
		!   Description = Percentage
		WORD PERCENTAGE
	END RECORD
