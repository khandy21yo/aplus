1	! Quick and dirty for LL to look up the MO orders that
	! have the wrong location stuck on them.

	option size = (integer long, real gfloat)

	on error goto 19000

100	%include "source:[oe.open]oe_regheader.hb"
	map (oe_regheader) oe_regheader_cdd oe_regheader

	%include "source:[mo.open]mo_regline.hb"
	map (mo_regline) mo_regline_cdd mo_regline

200	%include "source:[oe.open]oe_regheader.opn"

210	%include "source:[mo.open]mo_regline.opn"

1000	reset #oe_regheader.ch%

1100	get #oe_regheader.ch%

	!
	! ok if for location 010
	!
	goto 1100 if oe_regheader::location = "010"

	!
	! ok if starts with a J
	!
	goto 1100 if left(oe_regheader::ordnum, 1%) = "J"

1200	!
	! See if there are any lines for it in MO
	!
	get #mo_regline.ch%, key #0% eq oe_regheader::ordnum

2000	print oe_regheader::ordnum; " "; oe_regheader::location

	goto 1100

9000	goto 32767

19000	!
	! Trap errors
	!
	select erl

	case 1100%
		resume 9000 if err = 11%

	case 1200%
		resume 1100

	end select

	on error goto 0

32767	end
