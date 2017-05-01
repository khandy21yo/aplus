	!
	! File Layout for: GL.GL_USERJOUR on 21-May-01
	!
	! General Ledget User Defined Journal
	!

	RECORD GL_USERJOUR_CDD
		! Element =
		!   Description = Journal Code
		STRING JCODE = 4
		! Element =
		!   Description = Journal line
		STRING JLINE = 4
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element =
		!   Description = Dollar Amount
		GFLOAT DOLLARS
		! Element =
		!   Description = Units
		GFLOAT UNITS
		! Element = XREF
		!   Description = Customer/Vendor number
		STRING XREF = 10
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVNUM = 8
	END RECORD
