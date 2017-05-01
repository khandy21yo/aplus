DEFINE RECORD CDD$TOP.SA.SA_SALCUST

        DESCRIPTION IS /*Customer Sales File*/.

        SA_SALCUST_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = PostDate (YYYYMMDD) */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Post Time (HHMMSS) */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END SA_SALCUST_CDD STRUCTURE.

END SA_SALCUST.
