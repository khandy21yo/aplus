	!
	! File Layout for: PR.PR_CONTROL on 21-May-01
	!
	! Payroll Control File
	!

	RECORD PR_CONTROL_CDD
		! Element =
		!   Description = Year of last purge
		STRING YEAR = 4
		! Element = DATE
		!   Description = Last folder Date Posted
		STRING POST_DATE = 8
		! Element = DATE
		!   Description = Last update/reverse Date
		STRING UR_DATE = 8
		! Element =
		!   Description = Update/reverse counter
		WORD UR_COUNT
		! Element =
		!   Description = Close flag
		STRING CLOSEFLAG = 1
		! Element = FLAG
		!   Description = Apply oh to Department or Subacct (D/S)
		STRING OH_APPLY_FLAG = 1
	END RECORD
