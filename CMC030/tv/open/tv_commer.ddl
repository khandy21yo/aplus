DEFINE RECORD CDD$TOP.TV.TV_COMMER

        DESCRIPTION IS /*Commercial Order Header*/.

        TV_COMMER_CDD STRUCTURE.

        /* Element = TV_FRMNUM
        Description = Form Number */
        FRMNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = TV_CUSNUM
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Commercial CLASS (PO-political, etc.) */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Source (LV-live, etc.) */
        SOURCE                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Priority (1-5) */
        PRIORITY                DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Start date */
        START_DATE              DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = End date */
        END_DATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Cancelled (Y,N) */
        CANCELLED               DATATYPE IS TEXT SIZE IS 1.

        /* Element = TV_CUSNUM
        Description = Agency Number */
        AGENCY_NUM              DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Agency Commission Percentage */
        AGENCY_PCT              DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Date billed */
        DATE_BILLED             DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Match code */
        MATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = TV_CUSNUM
        Description = Rep number */
        REP_NUM                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Rep commission rate */
        REP_PCT                 DATATYPE IS SIGNED WORD.

        /* Element = TV_SALNUM
        Description = Salesperson number */
        SALES_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Salesman Commission */
        SALES_PCT               DATATYPE IS SIGNED WORD.

        /* Element = NAME
        Description = Name */
        CONTACT                 DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Billing type */
        BILL_TYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Spot seperation */
        SPOT_SEP                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Product Seperation */
        PROD_SEP                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = PO Number */
        PO                      DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Bill flag. 1-Normal. 2-Flat rate. */
        BILL_FLAG               DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Confirmation printed (Y/N) */
        CONFIRM                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Confirmation number */
        MOD_NUM                 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Single Conflict code for this order */
        CONFLICT                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END TV_COMMER_CDD STRUCTURE.

END TV_COMMER.
