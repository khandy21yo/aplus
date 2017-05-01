DEFINE RECORD CDD$TOP.OE.OE_CUSTDISC

        DESCRIPTION IS /*Customer Product Discount*/.

        OE_CUSTDISC_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Wildcard Product Type */
        PRODTYPE                DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Wildcard Product Category */
        PRODCAT                 DATATYPE IS TEXT SIZE IS 20.

        /* Element = PRICETYPE
        Description = Price type */
        PRICETYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = DISCOUNT
        Description = Discount percentage */
        DISCOUNT                DATATYPE IS G_FLOATING.

        END OE_CUSTDISC_CDD STRUCTURE.

END OE_CUSTDISC.
