	!
	! File Layout for: GL.GL_MASK on 21-May-01
	!
	! General Ledger Mask
	!

	RECORD GL_MASK_CDD
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT_MASK = 18
		! Element =
		!   Description = Description
		STRING DESCR = 40
		! Element =
		!   Description = Cash Flow code
		STRING CASH_FLOW = 4
		! Element =
		!   Description = Work capital code
		STRING WORK_CAPT = 4
		! Element =
		!   Description = Financial type code
		STRING FIN_TYPE = 10
	END RECORD
