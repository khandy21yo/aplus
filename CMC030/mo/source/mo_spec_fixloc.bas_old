1	option size = (integer long, real gfloat)

	%include "source:[oe.open]oe_regheader.hb"
	map (oe_regheader) oe_regheader_cdd oe_regheader

	%include "source:[sb.open]sb_subaccount.hb"
	%include "source:[jc.open]jc_job.hb"
	map (sb_subaccount) sb_subaccount_cdd sb_subaccount
	map (sb_subaccount) jc_job_cdd jc_job

	%include "source:[wp.open]wp_reqregister.hb"
	map (wp_reqregister) wp_reqregister_cdd wp_reqregister

	%include "source:[ic.open]ic_transaction.hb"
	map (ic_transaction) ic_transaction_cdd ic_transaction

	on error goto 19000

100	%include "source:[oe.open]oe_regheader.mod"

110	%include "source:[sb.open]sb_subaccount.mod"

120	%include "source:[wp.open]wp_reqregister.mod"

130	yyyypp$ = "199308"
	%include "source:[ic.open]ic_transaction.mod"
	ic_transaction_199308.ch% = ic_transaction.ch%
	ic_transaction.ch% = 0%

140	yyyypp$ = "199309"
	%include "source:[ic.open]ic_transaction.mod"
	ic_transaction_199309.ch% = ic_transaction.ch%
	ic_transaction.ch% = 0%

1000	!
	! Get the order number to play with
	!
	linput "MO Num: "; ORDERNUM$

	ordernum$ = edit$(left(ordernum$, 10%), -1%)

	orderleft$ = ordernum$ + space$(10%-len(ordernum$))
	ordernum$ = space$(10%-len(ordernum$)) + ordernum$

	goto 5000

2000	!
	! Do OE_REGHEADER
	!
	oe_regheader.count% = 0%
	get #oe_regheader.ch%, key #0% eq ORDERNUM$

2010	while oe_regheader::ordnum = ordernum$

		oe_regheader::location = "010"

		update #oe_regheader.ch%
		oe_regheader.count% = oe_regheader.count% + 1%

2020		get #oe_regheader.ch%

	next

3000	!
	! Do SB_SUBACCOUNT
	!
	sb_subaccount.count% = 0%
	get #sb_subaccount.ch%, key #0% eq "J" + ORDERLEFT$

3010	while sb_subaccount::subaccount = orderleft$

		jc_job::location = "010"

		update #sb_subaccount.ch%
		sb_subaccount.count% = sb_subaccount.count%  + 1%

3020		get #sb_subaccount.ch%

	next

4000	!
	! Do WP_REQREGISTER
	!
	wp_reqregister.count% = 0%
	get #wp_reqregister.ch%, key #0% eq ORDERLEFT$

4010	while wp_reqregister::job = orderleft$

		wp_reqregister::location = "010"

		update #wp_reqregister.ch%
		wp_reqregister.count% = wp_reqregister.count% + 1%

4020		get #wp_reqregister.ch%

	next

5000	!
	! Do IC_TRANSACTIOn_199308
	!
	ic_transaction_199308.count% = 0%
	get #ic_transaction_199308.ch%, key #2% eq ORDERLEFT$

5010	while ic_transaction::subaccount = orderleft$

		if trm$(ic_transaction::location) <> "010"
		then
			ic_transaction::location = "010"

			delete #ic_transaction_199308.ch%
			put #ic_transaction_199308.ch%
			ic_transaction_199308.count% = ic_transaction_199308.count% + 1%
		end if

5020		get #ic_transaction_199308.ch%

	next

6000	!
	! Do IC_TRANSACTIOn_199308
	!
	ic_transaction_199309.count% = 0%
	get #ic_transaction_199309.ch%, key #2% eq ORDERLEFT$

6010	while ic_transaction::subaccount = orderleft$

		if trm$(ic_transaction::location) <> "010"
		then
			ic_transaction::location = "010"

			delete #ic_transaction_199309.ch%
			put #ic_transaction_199309.ch%
			ic_transaction_199309.count% = ic_transaction_199309.count% + 1%
		end if

6020		get #ic_transaction_199309.ch%

	next

7000	!

9000	print ordernum$; &
		oe_regheader.count%; &
		sb_subaccount.count%; &
		wp_reqregister.count%; &
		ic_transaction_199308.count%; &
		ic_transaction_199309.count%

	goto 1000

19000	select erl
	case 2000%
		resume 3000

	case 2020%
		resume 3000

	case 3000%
		resume 4000

	case 3020%
		resume 4000

	case 4000%
		resume 5000

	case 4020%
		resume 5000

	case 5000%
		resume 6000

	case 5020%
		resume 6000

	case 6000%
		resume 7000

	case 6020%
		resume 7000

	end select

	on error goto 0

32767	end
