DEFINE RECORD CDD$TOP.OS.OS_JLINE

        DESCRIPTION IS /*Lowest line level journal*/.

        OS_JLINE_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Transaction key */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line */
        JLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = LINE
        Description = Line */
        LLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = CATEGORY
        Description = Category for this item */
        CATEGORY                DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT
        Description = Product Number Selected */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = QUANTITY
        Description = Quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quoted price */
        PRICE                   DATATYPE IS G_FLOATING.

        END OS_JLINE_CDD STRUCTURE.

END OS_JLINE.
