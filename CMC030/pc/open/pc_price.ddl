DEFINE RECORD CDD$TOP.PC.PC_PRICE

        DESCRIPTION IS /*Product Price*/.

        PC_PRICE_CDD STRUCTURE.

        /* Element = PRODUCT_NUM
        Description = Product number */
        PRODUCT_NUM             DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = PCTYPE
        Description = Price cost type */
        PCTYPE                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = DATE
        Description = Date (MMDDYYYY) */
        XDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Time (HHMMSS) */
        XTIME                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = AMOUNT
        Description = Dollar amount. */
        PRICECOST               DATATYPE IS G_FLOATING.

        END PC_PRICE_CDD STRUCTURE.

END PC_PRICE.
