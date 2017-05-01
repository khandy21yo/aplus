	!
	! File Layout for: PR.PR_TRN_PAY on 21-May-01
	!
	! Payroll Pay Journal
	!

	RECORD PR_TRN_PAY_CDD
		! Element = EMPNUM
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element = DATE
		!   Description = Payroll end Date
		STRING PR_END_DATE = 8
		! Element =
		!   Description = Employee Skill
		STRING EMP_SKILL = 6
		! Element =
		!   Description = Employee Grade
		STRING EMP_GRADE = 2
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element = SUBACC
		!   Description = Sub account (job number)
		STRING SUBACC = 10
		! Element = OPERATION
		!   Description = Operation
		STRING OPER = 8
		! Element = LOCATION
		!   Description = Location
		STRING LOCATION = 4
		! Element =
		!   Description = Department
		STRING DEPT = 6
		! Element = WORK_CENTER
		!   Description = Work Center
		STRING WORK_CENTER = 4
		! Element =
		!   Description = Union code
		STRING UNION = 2
		! Element =
		!   Description = Pay type (P-time, O-other earnings)
		STRING PTYPE = 1
		! Element =
		!   Description = Rate type (H-hourly, S-salary,P-Piece)
		STRING RTYPE = 1
		! Element =
		!   Description = Earnings code
		STRING CODE = 2
		! Element =
		!   Description = Piece rate
		GFLOAT PIECE_RATE
		! Element =
		!   Description = Hourly rate
		GFLOAT HOUR_RATE
		! Element =
		!   Description = Regular hours
		GFLOAT REG_HR
		! Element =
		!   Description = Overtime hours
		GFLOAT OVT_HR
		! Element =
		!   Description = Number of piece producted
		GFLOAT PIECE
		! Element =
		!   Description = Overtime factor
		WORD FACTOR
		! Element =
		!   Description = Gross pay
		GFLOAT GROSS
		! Element =
		!   Description = Tax package code
		STRING TAX_PKG = 2
		! Element =
		!   Description = Batch entry flag
		STRING BATCH_ENTRY = 2
		! Element =
		!   Description = Update flag
		WORD UPDATE_FLAG
		! Element = BATCH
		!   Description = Seq # for labor performance
		STRING SEQNUM = 6
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
		! Element = DATE
		!   Description = Work Date (YYYYMMDD)
		STRING WORKDATE = 8
		! Element =
		!   Description = Regular Time By Day * 100
		WORD REGULAR(6)
		! Element =
		!   Description = Overtime By Day * 100
		WORD OVERTIME(6)
		! Element = ASSET
		!   Description = Asset number
		STRING EQUIPMENT = 10
		! Element =
		!   Description = Hours of use on equipment
		GFLOAT EQUIPHOUR
	END RECORD
