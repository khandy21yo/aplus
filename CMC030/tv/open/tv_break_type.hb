	!
	! File Layout for: TV.TV_BREAK_TYPE on 21-May-01
	!
	! TV Break Type File
	!

	RECORD TV_BREAK_TYPE_CDD
		! Element =
		!   Description = Break type (LB, SB, NB, etc.)
		STRING BTYPE = 2
		! Element =
		!   Description = Description
		STRING DESCR = 20
		! Element =
		!   Description = Flag (0-avail, 1-no avail, 2-noavl/int)
		STRING BFLAG = 1
	END RECORD
