	!
	! File Layout for: WP.WP_CLOSELINE on 21-May-01
	!
	! WIP Closing Variance Journal
	!

	RECORD WP_CLOSELINE_CDD
		! Element = JOB
		!   Description = Job number
		STRING JOB = 10
		! Element =
		!   Description = Line Flag
		STRING LFLAG = 1
		! Element =
		!   Description = Variance Class
		STRING VCLASS = 4
		! Element = ACCOUNT
		!   Description = Variance Account Number
		STRING VACCT = 18
		! Element =
		!   Description = Variance Amount
		GFLOAT VAMOUNT
	END RECORD
