	!
	! File Layout for: AP.AP_1099_TABLE on 21-May-01
	!
	! 1099 Definition Table
	!

	RECORD AP_1099_TABLE_CDD
		! Element =
		!   Description = Table code
		STRING CODE = 2
		! Element =
		!   Description = Table Description
		STRING DESCR = 20
		! Element =
		!   Description = Base amount
		GFLOAT BASEAMT
		! Element =
		!   Description = Form Number
		STRING FRMNUM = 1
		! Element =
		!   Description = Form location
		STRING FRMLOC = 2
	END RECORD
