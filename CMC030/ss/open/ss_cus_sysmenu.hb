	!
	! File Layout for: SS.SS_CUS_SYSMENU on 21-May-01
	!
	! System Menu
	!

	RECORD SS_CUS_SYSMENU_CDD
		! Element =
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Tape Number
		STRING TAPE = 2
		! Element = DATE
		!   Description = Installation Date (YYYYMMDD)
		STRING INSDAT = 8
		! Element =
		!   Description = System Name
		STRING SYSTEM = 2
		! Element =
		!   Description = System Menu Number
		STRING MENNUM = 6
	END RECORD
