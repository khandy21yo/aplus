DEFINE RECORD CDD$TOP.AR.AR_CRJL

        DESCRIPTION IS /*Cash Receipts Journal (Line)*/.

        AR_CRJL_CDD STRUCTURE.

        /* Element =
        Description = Receipt number */
        RECNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Line number */
        LLINE                   DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Transaction Type */
        TRATYP                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Salesman Number */
        SALNUM                  DATATYPE IS TEXT SIZE IS 10.

        END AR_CRJL_CDD STRUCTURE.

END AR_CRJL.
