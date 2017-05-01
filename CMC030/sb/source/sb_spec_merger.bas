1	!++
	! Special one-time program to merge KINGB subaccount
	! files together to see if it will show up all of the
	! salesmans commission information
	!
	! Author:
	!
	!	03/20/92 - Kevin Handy
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--


	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)


	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT) SB_SUBACCOUNT_CDD SB_SUBACCOUNT

	ON ERROR GOTO 19000

100	!======================================================================
	! SB_SUBACCOUNT file (open read only)
	!======================================================================

	SB_SUBACCOUNT.CH% = 5%
	SB_SUBACCOUNT.DEV$ = "$DISK2:[ICKBI]"

	SB_SUBACCOUNT.NAME$ = SB_SUBACCOUNT.DEV$+"SB_SUBACCOUNT.MAS"

	OPEN SB_SUBACCOUNT.NAME$ FOR INPUT AS FILE SB_SUBACCOUNT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP SB_SUBACCOUNT, &
		PRIMARY KEY &
		( &
			SB_SUBACCOUNT::SUBJECT, &
			SB_SUBACCOUNT::SUBACCOUNT &
		), &
		ALTERNATE KEY &
		( &
			SB_SUBACCOUNT::SUBJECT, &
			SB_SUBACCOUNT::TTYPE, &
			SB_SUBACCOUNT::SUBACCOUNT &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			SB_SUBACCOUNT::SUBJECT, &
			SB_SUBACCOUNT::CLASS, &
			SB_SUBACCOUNT::SUBACCOUNT &
		)	CHANGES, &
		ACCESS READ, ALLOW MODIFY


200	!======================================================================
	! SB_SUBACCOUNT file (open read only)
	!======================================================================

	SB_SUBACCOUNT.CH% = 6%
	SB_SUBACCOUNT.DEV$ = "$DISK2:[APKB1]"

	SB_SUBACCOUNT.NAME$ = SB_SUBACCOUNT.DEV$+"SB_SUBACCOUNT.MAS"

	OPEN SB_SUBACCOUNT.NAME$ FOR INPUT AS FILE SB_SUBACCOUNT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP SB_SUBACCOUNT, &
		PRIMARY KEY &
		( &
			SB_SUBACCOUNT::SUBJECT, &
			SB_SUBACCOUNT::SUBACCOUNT &
		), &
		ALTERNATE KEY &
		( &
			SB_SUBACCOUNT::SUBJECT, &
			SB_SUBACCOUNT::TTYPE, &
			SB_SUBACCOUNT::SUBACCOUNT &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			SB_SUBACCOUNT::SUBJECT, &
			SB_SUBACCOUNT::CLASS, &
			SB_SUBACCOUNT::SUBACCOUNT &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

	RESET #5%

1000	GET #5%

2000	PUT #6%

	PRINT "*ADDED*"

	GOTO 1000

3000	CLOSE 5%, 6%

	GOTO 32767

19000	!

	SELECT ERL

	CASE 1000%
		RESUME 3000

	CASE 2000%
		PRINT "*NOT ADDED*"
		RESUME 1000

	END SELECT

	ON ERROR GOTO 0

32767	END
