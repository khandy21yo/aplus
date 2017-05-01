	!
	! File Layout for: AD.AD_TABLE on 21-May-01
	!
	! Optional Depreciation Tables
	!

	RECORD AD_TABLE_CDD
		! Element = OPTTABLE
		!   Description = Depreciation optional table code
		STRING OPTTABLE = 6
		! Element = DATE
		!   Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
		! Element =
		!   Description = Depreciation life
		STRING YEARS = 4
		! Element =
		!   Description = Dimension (1 or 2)
		STRING DIMEN = 1
	END RECORD
