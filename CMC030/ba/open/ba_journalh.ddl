DEFINE RECORD CDD$TOP.BA.BA_JOURNALH

        DESCRIPTION IS /*Agency Journal Header*/.

        BA_JOURNALH_CDD STRUCTURE.

        /* Element =
        Description = Billing Number */
        BILLNUM                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = CUSTOMER
        Description = Agency Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = INVOICE
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Transaction Date */
        TRADAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = From payroll date (YYYYMMDD) */
        FROMDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = To payroll date (YYYYMMDD) */
        TODATE                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Non productive operations */
        OPERATIONS              DATATYPE IS TEXT SIZE IS 20.

        END BA_JOURNALH_CDD STRUCTURE.

END BA_JOURNALH.
