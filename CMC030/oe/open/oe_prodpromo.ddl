DEFINE RECORD CDD$TOP.OE.OE_PRODPROMO

        DESCRIPTION IS /*Product Promotion*/.

        OE_PRODPROMO_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = REFNO
        Description = Reference number */
        REFPROMO                DATATYPE IS TEXT SIZE IS 16.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSTOMER                DATATYPE IS TEXT SIZE IS 10.

        /* Element = CUSTYPE
        Description = Customer Type */
        CUSTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Customer Category */
        CUSTCAT                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = SALESMAN
        Description = Salesperson number */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Promo Dollar Amount */
        PROMODOLL               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Promo Percentage */
        PROMOPERC               DATATYPE IS G_FLOATING.

        END OE_PRODPROMO_CDD STRUCTURE.

END OE_PRODPROMO.
