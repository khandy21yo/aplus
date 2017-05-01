	!
	! File Layout for: BI.BI_RATE on 21-May-01
	!
	! Medical Service Rate
	!

	RECORD BI_RATE_CDD
		! Element = CPT
		!   Description = Current Procedural Terminology Code
		STRING CPT = 5
		! Element = DATE
		!   Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
		! Element =
		!   Description = Flat Rate
		GFLOAT RATE
		! Element = RATETABLE
		!   Description = CPT Time Rate Table
		STRING RATETABLE = 6
	END RECORD
