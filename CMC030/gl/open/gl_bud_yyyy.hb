	!
	! File Layout for: GL.GL_BUD_YYYY on 21-May-01
	!
	! Budget File for Fiscal Year
	!

	RECORD GL_BUD_YYYY_CDD
		! Element = ACCOUNT
		!   Description = General Ledger account number
		STRING ACCT = 18
		! Element = AMOUNT
		!   Description = Dollar amount
		GFLOAT DOLLAR(13)
		! Element = UNIT
		!   Description = Unit amount
		GFLOAT UNIT(13)
		! Element =
		!   Description = Budget hours
		GFLOAT HOUR(13)
	END RECORD
