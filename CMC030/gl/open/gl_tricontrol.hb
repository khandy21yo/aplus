	!
	! File Layout for: GL.GL_TRICONTROL on 21-May-01
	!
	! Tri-Spur Journal Control File
	!

	RECORD GL_TRICONTROL_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION(20) = 20
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT(20) = 18
		! Element =
		!   Description = Flags
		STRING FLAG(20) = 1
	END RECORD
