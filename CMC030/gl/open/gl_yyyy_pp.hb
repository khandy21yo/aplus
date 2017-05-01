	!
	! File Layout for: GL.GL_YYYY_PP on 21-May-01
	!
	! General Ledger Transaction File
	!

	RECORD GL_YYYY_PP_CDD
		! Element =
		!   Description =
		STRING ACCT = 18
		! Element =
		!   Description =
		STRING SOURCE = 4
		! Element =
		!   Description =
		STRING REFNO = 16
		! Element =
		!   Description =
		STRING TRANDAT = 8
		! Element =
		!   Description =
		STRING DESCR = 30
		! Element =
		!   Description =
		GFLOAT AMOUNT
		! Element =
		!   Description =
		STRING XREFNO = 10
		! Element =
		!   Description =
		STRING POSTIM = 6
		! Element =
		!   Description =
		STRING POSDAT = 8
		! Element =
		!   Description =
		STRING CKNO = 6
		! Element =
		!   Description =
		STRING TRANKEY = 6
		! Element =
		!   Description =
		STRING SUBACC = 10
		! Element =
		!   Description =
		STRING OPERATION = 8
		! Element =
		!   Description =
		GFLOAT UNITS
		! Element =
		!   Description =
		GFLOAT HOURS
		! Element =
		!   Description =
		STRING UPDSTA = 2
		! Element =
		!   Description =
		STRING BTHNUM = 6
	END RECORD
