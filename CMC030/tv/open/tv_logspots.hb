	!
	! File Layout for: TV.TV_LOGSPOTS on 21-May-01
	!
	! Scheduled Spots
	!

	RECORD TV_LOGSPOTS_CDD
		! Element =
		!   Description = Customer number
		STRING CUSNUM = 10
		! Element =
		!   Description = Form number
		STRING FRMNUM = 8
		! Element =
		!   Description = Schedule number
		STRING SKEDNUM = 2
		! Element =
		!   Description = Date
		STRING SCH_DATE = 8
		! Element =
		!   Description = Time
		STRING SCH_TIME = 6
		! Element =
		!   Description = Rate
		GFLOAT RATE
		! Element =
		!   Description = Type (0-commercial, 1-fill)
		STRING SCH_TYPE = 1
		! Element =
		!   Description = Spots flag (N-not run, R-run, S-sch)
		STRING SPOTS_FLAG = 1
		! Element =
		!   Description = Spots code (MG-make good, etc.)
		STRING SPOTS_CODE = 2
		! Element =
		!   Description = Length
		STRING LENGTH = 6
		! Element =
		!   Description = Agency number
		STRING AGENCY_NUM = 20
		! Element =
		!   Description = Cart number
		STRING CARTNUM = 10
		! Element =
		!   Description = Cut number
		STRING CUTNUM = 2
		! Element =
		!   Description = From time slot
		STRING FROM_TIME_SLOT = 8
		! Element =
		!   Description = To time slot
		STRING TO_TIME_SLOT = 8
		! Element =
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element =
		!   Description = Invoice date
		STRING INVDAT = 8
		! Element =
		!   Description = Post date
		STRING POSTDATE = 8
		! Element =
		!   Description = Actual run time
		STRING RUN_TIME = 6
		! Element =
		!   Description = Log class
		STRING CLASS = 4
		! Element =
		!   Description = Conflict code
		STRING CONFLICT = 8
		! Element =
		!   Description = Description/comment
		STRING DESCR = 30
		! Element =
		!   Description = Sequence number
		STRING SEQNUM = 2
	END RECORD
