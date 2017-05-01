	!
	! File Layout for: TK.TK_MODINFO on 21-May-01
	!
	! AUTHOR/MODIFICATION INFORMATION ABOUT A PROGRAM
	!

	RECORD TK_MODINFO_CDD
		! Element =
		!   Description = Program name
		STRING PROGNAME = 40
		! Element = SYSTEMID
		!   Description = System id.
		STRING SYSTEM = 6
		! Element =
		!   Description = Name of Author/Modifier
		STRING PROGRAMMER = 40
		! Element = DATE
		!   Description = Modification Date (YYYYMMDD)
		STRING MODDATE = 8
		! Element =
		!   Description = Author/Modifier Flag
		STRING MODFLAG = 1
		! Element =
		!   Description = Description of modification
		STRING MODDESCR(20) = 80
	END RECORD
