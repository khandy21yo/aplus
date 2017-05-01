DEFINE RECORD CDD$TOP.OS.OS_JOURNAL

        DESCRIPTION IS /*Main Journal Header*/.

        OS_JOURNAL_CDD STRUCTURE.

        /* Element = TRANKEY
        Description = Transaction key */
        TRANS                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSTOMER                DATATYPE IS TEXT SIZE IS 10.

        /* Element = INVOICE
        Description = Invoice number */
        INVOICE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Transaction Date (YYYYMMDD) */
        TRANDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Sales Tax */
        SALESTAX                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Prepayment */
        PAYMENT                 DATATYPE IS G_FLOATING.

        /* Element = DEPOSIT
        Description = Deposit number */
        DEPOSIT                 DATATYPE IS TEXT SIZE IS 6.

        END OS_JOURNAL_CDD STRUCTURE.

END OS_JOURNAL.
