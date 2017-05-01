	!
	! File Layout for: TV.TV_SKED_WORK on 21-May-01
	!
	! File Used to Hold Commercials to Schedule
	!

	RECORD TV_SKED_WORK_CDD
		! Element =
		!   Description = Priority
		WORD PRIORITY
		! Element = TV_FRMNUM
		!   Description = Form Number
		STRING FRMNUM = 8
		! Element = TV_CUSNUM
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Schedule number
		STRING SKED_NUM = 2
	END RECORD
