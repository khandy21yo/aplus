	!
	! File Layout for: UTL.UTL_SET on 21-May-01
	!
	! CMC Utility Set File
	!

	RECORD UTL_SET_CDD
		! Element =
		!   Description = Program name
		STRING PROGRAMNAME = 39
		! Element =
		!   Description = Item number
		STRING ITEM = 6
		! Element =
		!   Description = System name
		STRING SYSTEM = 2
		! Element = YESNO
		!   Description = Yes or No Flag for Undefined Input
		STRING ALLOWUND = 1
		! Element =
		!   Description = Unused
		STRING UNUSED = 3
		! Element =
		!   Description = Hard/Soft/Field default
		STRING HARD = 1
		! Element =
		!   Description = Data
		STRING SDATA = 30
		! Element =
		!   Description = Data Format
		STRING FDATA = 30
		! Element =
		!   Description = Unused Fiels
		STRING UNUSED2 = 4
	END RECORD
