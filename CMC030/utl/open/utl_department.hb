	!
	! File Layout for: UTL.UTL_DEPARTMENT on 21-May-01
	!
	! Department Profile
	!

	RECORD UTL_DEPARTMENT_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DEPT_NUM
		!   Description = Department number
		STRING DEPT_NUM = 6
		! Element =
		!   Description = Department name
		STRING DESCRIPTION = 40
		! Element = DEPGROUP
		!   Description = Department group number
		STRING DEPGROUP = 2
		! Element = PHONE
		!   Description = Phone number
		STRING PHONE = 10
		! Element =
		!   Description = Department Supervisor
		STRING SUPERVISOR = 30
	END RECORD
