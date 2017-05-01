DEFINE RECORD CDD$TOP.PD.PD_PRODUCT

        DESCRIPTION IS /*Product Description*/.

        PD_PRODUCT_CDD STRUCTURE.

        /* Element = PRODUCT_NUM
        Description = Product number */
        PRODUCT_NUM             DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Product type */
        PROD_TYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Category */
        CATEGORY                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Unit of measure */
        UOM                     DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Unused... */
        PACK                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Label */
        LABEL                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = COSTMETHOD
        Description = Costing method */
        METHOD                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = On set Date */
        BDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Activity Status */
        SSTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = End Date */
        EDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Secondary Code */
        SECONDARY_CODE          DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Weight of one unit */
        WEIGHT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Pack unit of measure */
        BOMUOM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Product units in one pack */
        PRODUCT_FACTOR          DATATYPE IS G_FLOATING.

        END PD_PRODUCT_CDD STRUCTURE.

END PD_PRODUCT.
