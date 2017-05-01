	!
	! File Layout for: RM.RM_HISTORY on 21-May-01
	!
	! Restaurant Sales and Labor History
	!

	RECORD RM_HISTORY_CDD
		! Element =
		!   Description = Record type
		STRING CATEGORY = 2
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Date (MMDDYYYY)
		STRING ACTION_DATE = 8
		! Element = TIME
		!   Description = Time (HHMMSS)
		STRING TIME_FROM = 6
		! Element = TIME
		!   Description = Time (HHMMSS)
		STRING TIME_TO = 6
		! Element =
		!   Description = Rate
		GFLOAT RATE
		! Element =
		!   Description = Store quantity/amount flag
		STRING REC_TYPE = 1
		! Element =
		!   Description = Amount/Quantity
		WORD AMOUNT_QTY(47)
	END RECORD
