DEFINE RECORD CDD$TOP.AR.AR_LBCRJH

        DESCRIPTION IS /*Cash Receipts Journal*/.

        AR_LBCRJH_CDD STRUCTURE.

        /* Element =
        Description = Receipt Number */
        RECNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = CHECK
        Description = Check number */
        CHECK                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = DEPOSIT
        Description = Deposit number */
        DEPOSIT                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Transaction Date */
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

        /* Element = INVOICE
        Description = Apply to first Invoice Number */
        INVOICE                 DATATYPE IS TEXT SIZE IS 8.

        END AR_LBCRJH_CDD STRUCTURE.

END AR_LBCRJH.
