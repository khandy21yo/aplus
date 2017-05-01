	!
	! File Layout for: BM.BM_CONTROL on 21-May-01
	!
	! BOM Control File
	!

	RECORD BM_CONTROL_CDD
		! Element =
		!   Description = STD Burden Hourly Rate
		GFLOAT BURDENRATE
		! Element =
		!   Description = Component Types
		STRING PRODTYPE = 20
		! Element =
		!   Description = STD Labor Hourly Rate
		GFLOAT LABORRATE
		! Element =
		!   Description = Raw Material Type
		STRING RMAT = 20
		! Element =
		!   Description = Burden percentage of Labor
		GFLOAT BURDENPERC
	END RECORD
