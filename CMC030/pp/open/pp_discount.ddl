DEFINE RECORD CDD$TOP.PP.PP_DISCOUNT

        DESCRIPTION IS /*Discount Table*/.

        PP_DISCOUNT_CDD STRUCTURE.

        /* Element =
        Description = Discount Code (A,I??) */
        CODE                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Method (V=Volume,N=Nonvolume) */
        METHOD                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Over Amount */
        OVER                    ARRAY 0:9 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Rate */
        RATE                    ARRAY 0:9 DATATYPE IS G_FLOATING.

        END PP_DISCOUNT_CDD STRUCTURE.

END PP_DISCOUNT.
