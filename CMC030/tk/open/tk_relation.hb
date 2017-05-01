	!
	! File Layout for: TK.TK_RELATION on 21-May-01
	!
	! Module Relation File
	!

	RECORD TK_RELATION_CDD
		! Element = MODNAME
		!   Description = Module name
		STRING PARENT = 39
		! Element = MODNAME
		!   Description = Module name
		STRING CHILD = 39
		! Element =
		!   Description = Number submodules in the module
		WORD QUANTITY
		! Element =
		!   Description = Defining reference
		STRING DEFREF = 1
		! Element = DATE
		!   Description = Creating date
		STRING CDATE = 8
		! Element = TIME
		!   Description = Creating time
		STRING CTIME = 6
	END RECORD
