	!
	! File Layout for: TK.TK_FILE on 21-May-01
	!
	! File Structure Description
	!

	RECORD TK_FILE_CDD
		! Element = FILESTRNAME
		!   Description = Record structure name
		STRING STRUCT = 39
		! Element =
		!   Description = Sequence number
		STRING SEQUENCE = 3
		! Element =
		!   Description = Field name
		STRING FLDNAME = 39
		! Element =
		!   Description = Description
		STRING DESCRIPTION = 60
		! Element =
		!   Description = Database
		STRING DATABASE = 2
		! Element =
		!   Description = Field classifier
		STRING CLASSIFIER = 20
		! Element =
		!   Description = Data array (y/n)
		STRING DATAARRAY = 1
		! Element =
		!   Description = Date type
		STRING DATETYPE = 20
		! Element =
		!   Description = Size
		LONG DATASIZE
		! Element = DATE
		!   Description = Creating date
		STRING CDATE = 8
		! Element = TIME
		!   Description = Creating time
		STRING CTIME = 6
	END RECORD
