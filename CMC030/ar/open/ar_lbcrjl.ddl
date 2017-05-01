DEFINE RECORD CDD$TOP.AR.AR_LBCRJL

        DESCRIPTION IS /*Cash Receipts Journal (Line)*/.

        AR_LBCRJL_CDD STRUCTURE.

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

        /* Element = MATTER
        Description = Matter Number */
        MATTER_NUM              DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Flag for Type of allocation */
        ALLOCATE                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Staff Number */
        STAFF                   DATATYPE IS TEXT SIZE IS 10.

        END AR_LBCRJL_CDD STRUCTURE.

END AR_LBCRJL.
