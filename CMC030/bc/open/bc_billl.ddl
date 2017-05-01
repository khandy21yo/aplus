DEFINE RECORD CDD$TOP.BC.BC_BILLL

        DESCRIPTION IS /*Billing to Customer Billing Journal Line*/.

        BC_BILLL_CDD STRUCTURE.

        /* Element =
        Description = Order Number */
        ORDER                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Line Number */
        LINENO                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Amount Ordered */
        ORDAMT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount Shipped */
        SHPAMT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount Back-Ordered */
        BOAMT                   DATATYPE IS G_FLOATING.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element = UOM
        Description = Units of measure code */
        UNITME                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Unit Price */
        UNIPRI                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Total Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Line Type */
        LTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Tax Type */
        TAXTYP                  DATATYPE IS TEXT SIZE IS 1.

        END BC_BILLL_CDD STRUCTURE.

END BC_BILLL.
