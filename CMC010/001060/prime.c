/*
** Find all the primes in the integers 3 through 5000
*/
main()
  {
  int i;
  int n;
  n = 2;

  while (++n <= 5000)	/* n is prime candidate */
    {
    i = 1;
    while (++i < n )
      if (n % i == 0)	/* no remainder, n is not prime */
        break;
      if (i == n)	/* never divided evenly, n is prime */
        printf("%d\n", n);
    }
  }
