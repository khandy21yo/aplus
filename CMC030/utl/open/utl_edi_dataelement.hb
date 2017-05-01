	!
	! File Layout for: UTL.UTL_EDI_DATAELEMENT on 21-May-01
	!
	! EDI Data Element Table
	!

	RECORD UTL_EDI_DATAELEMENT_CDD
		! Element = CODE3
		!   Description = Data element reference number
		STRING REFERENCE = 6
		! Element = TITLE
		!   Description = Data element title
		STRING TITLE = 60
		! Element =
		!   Description = Minumum Length
		WORD MMIN
		! Element =
		!   Description = Maximum Length
		WORD MMAX
		! Element = TYPE
		!   Description = Type
		STRING TTYP = 2
	END RECORD
