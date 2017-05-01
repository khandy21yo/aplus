1	option size = (integer long, real gfloat)

	%include "source:[sb.open]sb_balance.hb"
	map (sb_balance) sb_balance_cdd sb_balance

100	%include "source:[sb.open]sb_balance.opn"

200	!======================================================================
	! SB_BALANCE file (open read only)
	!======================================================================

	CALL ASSG_CHANNEL(SB_BALANCE_NEW.CH%, STAT%)
	SB_BALANCE.DEV$ = "DKa0:"

	SB_BALANCE.NAME$ = SB_BALANCE.DEV$ + "SB_BALANCE.HIS"

	OPEN SB_BALANCE.NAME$ FOR OUTPUT AS FILE SB_BALANCE_NEW.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP SB_BALANCE, &
		extendsize 102%, &
		bucketsize 40%, &
		buffer 64%, &
		PRIMARY KEY &
		( &
			SB_BALANCE::SYSTEM, &
			SB_BALANCE::SUBACCOUNT, &
			SB_BALANCE::OPERATION, &
			SB_BALANCE::ACCOUNT, &
			SB_BALANCE::PERIOD &
		), &
		ALTERNATE KEY &
		( &
			SB_BALANCE::PERIOD, &
			SB_BALANCE::SYSTEM, &
			SB_BALANCE::SUBACCOUNT &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW NONE


1000	reset #sb_balance.ch%

1100	get #sb_balance.ch%

1200	put #sb_balance_new.ch%

	c% = c% + 1%

	if c% >= 100%
	then
		print ".";
		c% = 0%
		print if ccpos(0%) >= 50%
	end if

	goto 1100

32767	end
