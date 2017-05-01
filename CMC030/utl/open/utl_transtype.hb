	!
	! File Layout for: UTL.UTL_TRANSTYPE on 21-May-01
	!
	! Transaction Type Description
	!

	RECORD UTL_TRANSTYPE_CDD
		! Element = TRANSTYPE
		!   Description = Transaction type code
		STRING CODE = 2
		! Element =
		!   Description = Transaction type description
		STRING DESCRIPTION = 20
		! Element =
		!   Description = Classification
		STRING CLASS = 2
		! Element =
		!   Description = Transaction sign
		STRING TRANSSIGN = 1
	END RECORD
