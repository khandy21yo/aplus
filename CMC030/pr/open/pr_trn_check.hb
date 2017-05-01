	!
	! File Layout for: PR.PR_TRN_CHECK on 21-May-01
	!
	! Payroll Check Journal
	!

	RECORD PR_TRN_CHECK_CDD
		! Element = EMPNUM
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element = DATE
		!   Description = Date
		STRING PR_END_DATE = 8
		! Element = CHKNUM
		!   Description = Check number
		STRING CHECK = 6
		! Element = DATE
		!   Description = Date
		STRING CHECK_DATE = 8
		! Element =
		!   Description = Pay frequency
		WORD PAYFREQ
		! Element =
		!   Description = Update flag
		WORD UPDATE_FLAG
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
	END RECORD
