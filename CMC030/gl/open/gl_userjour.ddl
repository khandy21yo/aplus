DEFINE RECORD CDD$TOP.GL.GL_USERJOUR

        DESCRIPTION IS /*General Ledget User Defined Journal*/.

        GL_USERJOUR_CDD STRUCTURE.

        /* Element =
        Description = Journal Code */
        JCODE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Journal line */
        JLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Dollar Amount */
        DOLLARS                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Units */
        UNITS                   DATATYPE IS G_FLOATING.

        /* Element = XREF
        Description = Customer/Vendor number */
        XREF                    DATATYPE IS TEXT SIZE IS 10.

        /* Element = INVOICE
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        END GL_USERJOUR_CDD STRUCTURE.

END GL_USERJOUR.
