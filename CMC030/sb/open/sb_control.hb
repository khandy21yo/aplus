	!
	! File Layout for: SB.SB_CONTROL on 21-May-01
	!
	! Subaccount Control File
	!

	RECORD SB_CONTROL_CDD
		! Element = SYSTEM
		!   Description = Software System code
		STRING SYSTEM = 2
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP) closed
		STRING PERIOD = 6
		! Element = CONTROLFLAG
		!   Description = Status flag in the control files
		STRING CONTROLFLAG = 1
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING CDATE = 8
		! Element = TIME
		!   Description = Time (HHMMSS)
		STRING CTIME = 6
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
		! Element = SUBJECT
		!   Description = Subaccount subject
		STRING SUBJECT = 1
		! Element =
		!   Description = Default number
		STRING DEFNUMBER = 10
	END RECORD
