	!
	! File Layout for: TK.TK_MODDATE on 21-May-01
	!
	! Check Modification History Dates
	!

	RECORD TK_MODDATE_CDD
		! Element =
		!   Description = Program Editor's Name
		STRING PROG_EDITOR = 30
		! Element =
		!   Description = Date Program was Writen
		STRING DATE = 8
		! Element =
		!   Description = Name of Program that is being checked
		STRING PROG_NAME = 40
		! Element =
		!   Description = Program Editor who Modified program
		STRING MOD_EDITOR = 30
		! Element =
		!   Description = Date Program was Modified
		STRING MOD_DATE = 8
		! Element =
		!   Description = Description of Modification
		STRING MOD_DESCRIPTION = 80
		! Element =
		!   Description = Counter to keep modifications sorted
		STRING XX_FIELD = 2
	END RECORD
