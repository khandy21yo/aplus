DEFINE RECORD CDD$TOP.OE.OE_CATEGORY

        DESCRIPTION IS /*Sales Order Category Description Table*/.

        OE_CATEGORY_CDD STRUCTURE.

        /* Element =
        Description = Category */
        ORDCAT                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 30.

        END OE_CATEGORY_CDD STRUCTURE.

END OE_CATEGORY.
