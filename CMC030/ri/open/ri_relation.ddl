DEFINE RECORD CDD$TOP.RI.RI_RELATION

        DESCRIPTION IS /*Product Relation*/.

        RI_RELATION_CDD STRUCTURE.

        /* Element = PRODUCT_NUM
        Description = Product number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Item number */
        ITEMNUM                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT_NUM
        Description = Product number */
        INGREDIENT              DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Operation */
        OPERATION               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Scrap percentage */
        SCRAP                   DATATYPE IS SIGNED WORD.

        END RI_RELATION_CDD STRUCTURE.

END RI_RELATION.
