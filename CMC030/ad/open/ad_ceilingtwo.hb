	!
	! File Layout for: AD.AD_CEILINGTWO on 21-May-01
	!
	! Two Dimensional Optional Ceiling Table
	!

	RECORD AD_CEILINGTWO_CDD
		! Element = OPTTABLE
		!   Description = Depreciation optional table code
		STRING OPTTABLE = 6
		! Element = DATE
		!   Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
		! Element =
		!   Description = Depreciation years
		STRING DEP_YEAR = 2
		! Element =
		!   Description = Ceiling amount
		GFLOAT CEILING(12)
	END RECORD
