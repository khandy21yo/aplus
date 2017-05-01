DEFINE RECORD CDD$TOP.AR.AR_SJL

        DESCRIPTION IS /*Sales Journal (Lines)*/.

        AR_SJL_CDD STRUCTURE.

        /* Element = INVNUM
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

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

        END AR_SJL_CDD STRUCTURE.

END AR_SJL.
