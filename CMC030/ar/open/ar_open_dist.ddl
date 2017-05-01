DEFINE RECORD CDD$TOP.AR.AR_OPEN_DIST

        DESCRIPTION IS /*Accounts Receivable Open Distribution File*/.

        AR_OPEN_DIST_CDD STRUCTURE.

        /* Element = INVNUM
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Line number */
        SLINE                   DATATYPE IS TEXT SIZE IS 3.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element = SUBACC
        Description = Sub account (job number) */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quanity */
        QTY                     DATATYPE IS G_FLOATING.

        /* Element =
        Description = Line type */
        LTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Tax type */
        TAXTYP                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 26.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        UPDATED                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Working Staff # */
        STAFF_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        POST_DATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Time (HHMMSS) */
        POST_TIME               DATATYPE IS TEXT SIZE IS 6.

        END AR_OPEN_DIST_CDD STRUCTURE.

END AR_OPEN_DIST.
