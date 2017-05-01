DEFINE RECORD CDD$TOP.BM.BM_RELATION

        DESCRIPTION IS /*Product Structure File*/.

        BM_RELATION_CDD STRUCTURE.

        /* Element = PRODUCT_NUM
        Description = Product number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Item number */
        ITEMNUM                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT_NUM
        Description = Product component */
        COMPONENT               DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Operation */
        OPERATION               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Scrap percentage */
        SCRAP                   DATATYPE IS SIGNED WORD.

        END BM_RELATION_CDD STRUCTURE.

END BM_RELATION.
