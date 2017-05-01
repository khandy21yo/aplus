	!
	! File Layout for: BI.BI_RATELINE on 21-May-01
	!
	! CPT Time Rate Line File
	!

	RECORD BI_RATELINE_CDD
		! Element = RATETABLE
		!   Description = CPT Time Rate Table
		STRING RATETABLE = 6
		! Element =
		!   Description = Time Interval
		STRING INTERVAL = 2
		! Element =
		!   Description = Rate
		GFLOAT RATE
	END RECORD
