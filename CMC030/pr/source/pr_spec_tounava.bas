1	! Move folders from available to unavailable
	!
	! Author:
	!
	!	02/07/95 - Kevin Handy
	!
	option size = (integer long, real gfloat)

	%include "source:[pr.open]pr_emp_accrual.hb"
	map (pr_emp_accrual) pr_emp_accrual_cdd pr_emp_accrual

	%include "source:[pr.open]pr_his_pay.hb"
	map (pr_his_pay) pr_his_pay_cdd pr_his_pay

	on error goto 19000

100	!
	! Open accrual file
	!
	%include "source:[pr.open]pr_emp_accrual.mod"

200	!
	! Open Folder
	!
	print "Payroll Date";
	linput batch_no$

	%include "source:[pr.open]pr_his_pay.opn"

300	input "Accrual Code"; Accrual$

1000	!
	! Loop through folder, looking for accrual items
	!
	reset #pr_his_pay.ch%

1100	get #pr_his_pay.ch%, regardless

	goto 1100 if pr_his_pay::ptype <> "A"
	goto 1100 if pr_his_pay::code <> Accrual$

1200	!
	! Search for code in accrual register
	!
	get #pr_emp_accrual.ch%, &
		key #0% eq pr_his_pay::empnum + pr_his_pay::code

	pr_emp_accrual::hoursuna = pr_emp_accrual::hoursuna + &
		(pr_his_pay::reg_hr + pr_his_pay::ovt_hr)

	pr_emp_accrual::hoursava = pr_emp_accrual::hoursava - &
		(pr_his_pay::reg_hr + pr_his_pay::ovt_hr)

	update #pr_emp_accrual.ch%

	print pr_emp_accrual::empnum; " "; pr_emp_accrual::atype

	goto 1100

9000	!
	close pr_emp_accrual.ch%
	close pr_his_pay.ch%

	goto 32767

19000	!
	select erl

	case 1100%
		resume 9000 if err = 11%

	case 1200%
		resume 1100

	end select

	on error goto 0

32767	end
