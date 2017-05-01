	!
	! File Layout for: AD.AD_TABLETWO on 21-May-01
	!
	! Two Dimensional Optional Depreciation Table
	!

	RECORD AD_TABLETWO_CDD
		! Element = OPTTABLE
		!   Description = Depreciation optional table code
		STRING OPTTABLE = 6
		! Element = DATE
		!   Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
		! Element =
		!   Description = Number of year
		STRING YEARS = 4
		! Element =
		!   Description = Depreciation years
		STRING DEP_YEAR = 2
		! Element =
		!   Description = Percentage
		WORD PERCENTAGE(12)
	END RECORD
