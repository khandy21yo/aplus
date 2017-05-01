	!
	! File Layout for: GL.GL_OBJECT on 21-May-01
	!
	! General Ledger Object Mask File
	!

	RECORD GL_OBJECT_CDD
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING OBJ_MASK = 18
		! Element = DESCRIPTION6
		!   Description = Description
		STRING DESCR = 40
		! Element =
		!   Description = Account Type
		STRING ACCT_TYPE = 1
		! Element =
		!   Description = Summary Flag
		STRING SUMM_FLAG = 1
		! Element =
		!   Description = Cash Flow
		STRING CASH_FLOW = 4
		! Element =
		!   Description = Work Capitol
		STRING WORK_CAPT = 4
		! Element =
		!   Description = Financial Type
		STRING FIN_TYPE = 10
	END RECORD
