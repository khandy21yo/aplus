DEFINE RECORD CDD$TOP.BI.BI_CATEGORY

        DESCRIPTION IS /*CPT Category*/.

        BI_CATEGORY_CDD STRUCTURE.

        /* Element = CATEGORY
        Description = CPT Category */
        CATEGORY                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        END BI_CATEGORY_CDD STRUCTURE.

END BI_CATEGORY.
