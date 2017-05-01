DEFINE RECORD CDD$TOP.PP.PP_CARD

        DESCRIPTION IS /*Pacific Pride Card Number File*/.

        PP_CARD_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Card Number */
        CARD                    DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Card Type (1=drv,2=2nd drv,3=veh) */
        CTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Beginning Odometer Reading */
        ODOMETER                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Pacific Pride Customer Number */
        SYSCUS                  DATATYPE IS TEXT SIZE IS 08.

        /* Element =
        Description = Discount Code */
        DISCOUNT                DATATYPE IS TEXT SIZE IS 4.

        END PP_CARD_CDD STRUCTURE.

END PP_CARD.
