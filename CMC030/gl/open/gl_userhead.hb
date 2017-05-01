	!
	! File Layout for: GL.GL_USERHEAD on 21-May-01
	!
	! User Defined Journal Header
	!

	RECORD GL_USERHEAD_CDD
		! Element =
		!   Description = Journal Code
		STRING JCODE = 4
		! Element = DEPOSIT
		!   Description = Deposit number
		STRING DEPOSIT = 6
		! Element = DATE
		!   Description = Journal Date (YYYYMMDD)
		STRING JDATE = 8
	END RECORD
