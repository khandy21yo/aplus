DEFINE RECORD CDD$TOP.TV.TV_CART_JOUR

        DESCRIPTION IS /*Journal of Cuts in Cart*/.

        TV_CART_JOUR_CDD STRUCTURE.

        /* Element = TV_CARTNUM
        Description = Cart number */
        CARTNUM                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Cut number */
        CUTNUM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description of cut */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END TV_CART_JOUR_CDD STRUCTURE.

END TV_CART_JOUR.
