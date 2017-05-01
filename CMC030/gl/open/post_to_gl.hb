	!
	! File Layout for: GL.POST_TO_GL on 21-May-01
	!
	! Batch Control File
	!

	RECORD POST_TO_GL_CDD
		! Element =
		!   Description = Account number
		STRING ACCT = 18
		! Element =
		!   Description = Description from chart
		STRING DESCR = 40
		! Element =
		!   Description = Account type
		STRING ACCTYPE = 2
		! Element =
		!   Description = Real account balance
		GFLOAT BEGBAL
		! Element =
		!   Description = Amount of credits
		GFLOAT CREDIT
		! Element =
		!   Description = Amount of debits
		GFLOAT DEBIT
		! Element =
		!   Description = Number of units
		GFLOAT UNITS
		! Element =
		!   Description = Number of hours
		GFLOAT HOURS
		! Element =
		!   Description = Undefined Account (Y/N)
		STRING UNDEFINED_ACCT = 1
	END RECORD
