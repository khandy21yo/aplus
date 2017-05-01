	!
	! File Layout for: GL.GL_VENCOL on 21-May-01
	!
	! User Defined Report Table
	!

	RECORD GL_VENCOL_CDD
		! Element =
		!   Description = Record Key
		STRING RECKEY = 8
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING FROMPER = 6
		! Element =
		!   Description = Column Title "A"
		STRING COL_TITLEA(10) = 12
		! Element =
		!   Description = Column Title "B"
		STRING COL_TITLEB(10) = 12
		! Element =
		!   Description = Account Wildcard
		STRING COL_ACCOUNT(10) = 45
		! Element =
		!   Description = Store/Detail Code Flag
		STRING COL_FLAG = 1
		! Element =
		!   Description = Title
		STRING COL_TITLE = 40
	END RECORD
