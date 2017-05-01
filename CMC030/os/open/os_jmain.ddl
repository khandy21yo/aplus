DEFINE RECORD CDD$TOP.OS.OS_JMAIN

        DESCRIPTION IS /*Main Part Screen*/.

        OS_JMAIN_CDD STRUCTURE.

        /* Element = TRANKEY
        Description = Transaction key */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line */
        JLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = QUANTITY
        Description = Quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Qty Invoiced */
        INVOICED                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Qty Completed */
        COMPLETED               DATATYPE IS G_FLOATING.

        /* Element = PRICE
        Description = Price Per */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        END OS_JMAIN_CDD STRUCTURE.

END OS_JMAIN.
