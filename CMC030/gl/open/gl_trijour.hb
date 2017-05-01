	!
	! File Layout for: GL.GL_TRIJOUR on 21-May-01
	!
	! Tri-Spur Journal
	!

	RECORD GL_TRIJOUR_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Transaction Date (YYYYMMDD)
		STRING TRANDATE = 8
		! Element = DOLLAR
		!   Description = Dollar Amounts
		GFLOAT AMOUNT(20)
		! Element = ACCOUNT
		!   Description = General Ledger Account Numbers
		STRING ACCOUNT(20) = 18
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION(20) = 20
	END RECORD
