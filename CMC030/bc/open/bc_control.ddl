DEFINE RECORD CDD$TOP.BC.BC_CONTROL

        DESCRIPTION IS /*Billing to Customer Control File*/.

        BC_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = Last period closed */
        LASTPERCLOSE            DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Year closed */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = INVOICE
        Description = Invoice number */
        INV_NUM                 DATATYPE IS TEXT SIZE IS 8.

        END BC_CONTROL_CDD STRUCTURE.

END BC_CONTROL.
