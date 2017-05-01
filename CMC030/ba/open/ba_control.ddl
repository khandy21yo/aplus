DEFINE RECORD CDD$TOP.BA.BA_CONTROL

        DESCRIPTION IS /*Billing to Agency Control File*/.

        BA_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = Last Period Closed */
        LASTPERCLOSE            DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Year closed */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = INVOICE
        Description = Invoice number */
        INV_NUM                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Billing Number */
        BILLNUM                 DATATYPE IS TEXT SIZE IS 10.

        END BA_CONTROL_CDD STRUCTURE.

END BA_CONTROL.
