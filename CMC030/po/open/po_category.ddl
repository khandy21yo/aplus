DEFINE RECORD CDD$TOP.PO.PO_CATEGORY

        DESCRIPTION IS /*Category Definitions File*/.

        PO_CATEGORY_CDD STRUCTURE.

        /* Element = CODE
        Description = Code */
        CODE                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END PO_CATEGORY_CDD STRUCTURE.

END PO_CATEGORY.
