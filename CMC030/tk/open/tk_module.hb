	!
	! File Layout for: TK.TK_MODULE on 21-May-01
	!
	! Module Description
	!

	RECORD TK_MODULE_CDD
		! Element =
		!   Description = Module name
		STRING MODNAME = 39
		! Element =
		!   Description =
		STRING DESCRIPTION = 60
		! Element =
		!   Description =
		STRING CATEGORY = 6
		! Element =
		!   Description = Module extension
		STRING EXTENSION = 10
		! Element =
		!   Description = Language module is written in
		STRING LANGUAGE = 8
		! Element =
		!   Description = Directory Location
		STRING DIRECTORY = 39
		! Element =
		!   Description =
		STRING MODTYPE = 4
		! Element = MODNUM
		!   Description = Module id number
		STRING MODNUM = 6
		! Element = DATE
		!   Description = Creation date
		STRING CDATE = 8
		! Element = TIME
		!   Description = Creation time
		STRING CTIME = 6
		! Element =
		!   Description = Is module sharable
		STRING SHAREABLE = 1
	END RECORD
