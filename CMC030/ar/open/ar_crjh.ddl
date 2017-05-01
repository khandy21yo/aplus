DEFINE RECORD CDD$TOP.AR.AR_CRJH

        DESCRIPTION IS /*Cash Receipts Journal (Header)*/.

        AR_CRJH_CDD STRUCTURE.

        /* Element =
        Description = Receipt number */
        RECNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = CUSNUM
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Check Number */
        CHECK                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Deposit number */
        DEPOSIT                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Transaction date */
        TRADAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Amount */
        AMNT                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Transaction Type */
        TRATYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 25.

        END AR_CRJH_CDD STRUCTURE.

END AR_CRJH.
