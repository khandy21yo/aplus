1	  EXTEND
10	  DIM FLAGS%(8191%) &

20	  PRINT "10 ITERATIONS." &
	\ FOR M% = 1% TO 10% &
		\ CNT% = 0% &
		\ FLAGS%(I%) = -1% FOR I%= 0% TO 8190% &
		\ FOR I%=0% TO 8190% &
			\ IF FLAGS%(I%) &
			  THEN	  PRIME% = I% + I% + 3% &
!				\ PRINT PRIME% &
				\ K% = I% + PRIME% &
				\ WHILE K% <= 8190% &
					\ FLAGS%(K%) = 0% &
					\ K% = K% + PRIME% &
				\ NEXT &
				\ CNT% = CNT% + 1% &

30		NEXT I% &
	\ NEXT M% &
	\ PRINT CNT%, "PRIMES." &
	\ END
