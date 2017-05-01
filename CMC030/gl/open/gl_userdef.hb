	!
	! File Layout for: GL.GL_USERDEF on 21-May-01
	!
	! User Defined GL Journal Definition File
	!

	RECORD GL_USERDEF_CDD
		! Element =
		!   Description = Journal Code
		STRING JCODE = 4
		! Element =
		!   Description = Line Number
		STRING JLINE = 4
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element =
		!   Description = AR/AP Posting flag
		STRING ARPFLAG = 2
		! Element = YESNO
		!   Description = Input units?
		STRING UNITFLAG = 1
		! Element = XREF
		!   Description = Cross Reference
		STRING XREF = 10
		! Element = YESNO
		!   Description = Duplicate entries allowed
		STRING DUPLCT = 1
		! Element =
		!   Description = +/- Numeric Sign
		STRING SIGNED = 1
	END RECORD
