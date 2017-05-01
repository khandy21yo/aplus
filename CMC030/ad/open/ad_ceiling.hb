	!
	! File Layout for: AD.AD_CEILING on 21-May-01
	!
	! Cost Recovery Ceiling Table
	!

	RECORD AD_CEILING_CDD
		! Element = CEILTABLE
		!   Description = Ceiling table code
		STRING CEILTABLE = 6
		! Element = DATE
		!   Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
		! Element =
		!   Description = Dimension (1 or 2)
		STRING DIMEN = 1
	END RECORD
