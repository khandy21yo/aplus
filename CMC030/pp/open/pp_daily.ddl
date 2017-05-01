DEFINE RECORD CDD$TOP.PP.PP_DAILY

        DESCRIPTION IS /*Daily Transaction File*/.

        PP_DAILY_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = CARD
        Description = Pacific Pride Vehicle Card Number */
        VEHICLE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = CARD
        Description = Pacific Pride Driver Card Number */
        DRIVER                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Transaction Date (YYYYMMDD) */
        TRANDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Transaction Time (HHMMSS) */
        TRANTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Host Number */
        HOST                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Site Number */
        SITE                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Site Type */
        STYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = UOM
        Description = Unit of measurement */
        UOM                     DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Quantity Sold */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Odometer reading */
        ODOM                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        SLTYPE                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        FTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Selling Price */
        SELLPRICE               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Transaction Cost */
        TRANCOST                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Misc. keyboard entry */
        MISCKEYB                DATATYPE IS TEXT SIZE IS 9.

        /* Element =
        Description = */
        TRNTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Discount */
        DISCOUNT                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = icb Date (YYYYMMDD) */
        ICBDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Transaction number */
        TRNNUM                  DATATYPE IS TEXT SIZE IS 5.

        /* Element =
        Description = Sales Tax Rate */
        STAXRATE                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Pump Number */
        PUMP                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        BUYFRAN                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Capture Date (YYYYMMDD) */
        CAPDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Capture Time (HHMMSS) */
        CAPTIME                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        POSTBNUM                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = */
        TRANSOURCE              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        EDITACT                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        JULIANDAY               DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = */
        RSTATION                DATATYPE IS TEXT SIZE IS 1.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = PP Cust Number */
        IDENTITY                DATATYPE IS TEXT SIZE IS 8.

        END PP_DAILY_CDD STRUCTURE.

END PP_DAILY.
