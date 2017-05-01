	!
	! File Layout for: GL.GL_USERLIST on 21-May-01
	!
	! List of Accounts Allowed for Specified Users
	!

	RECORD GL_USERLIST_CDD
		! Element =
		!   Description = User Name
		STRING USER = 16
		! Element = ACCOUNT
		!   Description = Allowed General Ledger Account Number
		STRING ACCOUNT = 18
		! Element =
		!   Description = Access Flag
		STRING FLAG = 1
	END RECORD
