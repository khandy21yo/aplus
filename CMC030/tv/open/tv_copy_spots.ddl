DEFINE RECORD CDD$TOP.TV.TV_COPY_SPOTS

        DESCRIPTION IS /*TV Copy Spots*/.

        TV_COPY_SPOTS_CDD STRUCTURE.

        /* Element =
        Description = Form number */
        FRMNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Sequential number */
        SEQNUM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Spot number */
        SPOT_NUM                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = House cart number */
        CART_NUM                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Agency cart number */
        AGENCY_CART             DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 20.

        END TV_COPY_SPOTS_CDD STRUCTURE.

END TV_COPY_SPOTS.
