	!
	! File Layout for: GL.GL_PERIOD on 21-May-01
	!
	! General Ledger Period File
	!

	RECORD GL_PERIOD_CDD
		! Element =
		!   Description =
		STRING PERIOD(13) = 20
		! Element =
		!   Description =
		WORD LASTPERCLO
		! Element =
		!   Description =
		WORD FPFY
		! Element =
		!   Description =
		STRING YEAR = 4
		! Element =
		!   Description =
		STRING BTHNUM = 6
		! Element =
		!   Description =
		GFLOAT SUMMARYTOTAL
		! Element =
		!   Description =
		STRING SUMMARYACCT = 18
		! Element =
		!   Description =
		WORD NEWYEAR
		! Element =
		!   Description = End date in period
		STRING ENDDATE(13) = 4
		! Element =
		!   Description = 1-Closing, 2-Resetting
		STRING CLOSEFLAG = 1
	END RECORD
