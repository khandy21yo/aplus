DEFINE RECORD CDD$TOP.OS.OS_BOM_LINE

        DESCRIPTION IS /*Bill of Material of Signs*/.

        OS_BOM_LINE_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = CATEGORY
        Description = Category to fill out */
        CATEGORY                DATATYPE IS TEXT SIZE IS 4.

        /* Element = QUANTITY
        Description = Default Quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element = YESNO
        Description = Multiple entries allowed */
        MULTIPLE                DATATYPE IS TEXT SIZE IS 1.

        /* Element = YESNO
        Description = At least one required? */
        REQUIRED                DATATYPE IS TEXT SIZE IS 1.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END OS_BOM_LINE_CDD STRUCTURE.

END OS_BOM_LINE.
