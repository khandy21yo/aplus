DEFINE RECORD CDD$TOP.BI.BI_CONTROL

        DESCRIPTION IS /*Billing to Insurance Control File*/.

        BI_CONTROL_CDD STRUCTURE.

        /* Element = INVOICE
        Description = Invoice number */
        INVOICE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = AR General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END BI_CONTROL_CDD STRUCTURE.

END BI_CONTROL.
