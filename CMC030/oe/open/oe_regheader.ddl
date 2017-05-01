DEFINE RECORD CDD$TOP.OE.OE_REGHEADER

        DESCRIPTION IS /*Sales Order Register Header File*/.

        OE_REGHEADER_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = ORDTYPE
        Description = Sales Order Type */
        ORDTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Order Category */
        ORDCAT                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Order Date */
        ORDDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = ASTATUS
        Description = Activity status */
        ASTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Date */
        SDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Shipping Name */
        SHIPNAM                 DATATYPE IS TEXT SIZE IS 46.

        /* Element = SHIPLIN
        Description = Shipping location number */
        SHIPLIN                 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Address, line 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Address, line 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Address, line 3 */
        ADD3                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = City */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Zip Code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = DEPOSIT
        Description = Deposit number */
        DEPOSIT                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Customer PO. (Obsolete remainder) */
        OLDCUSTPO               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Ship Via */
        SHIPVIA                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Terms */
        TERMS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Discount Percentage */
        DISC                    DATATYPE IS G_FLOATING.

        /* Element = TAXCODE
        Description = Tax code */
        TAXCODE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = TAXFLAG
        Description = Tax Flag */
        TAXFLAG                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Commission amount */
        COMMAMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Salesmen */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Commission for salesmen */
        SALCOMM                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount Paid */
        AMTPAID                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Packing List Release Number */
        SHIPNO                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = NOTES
        Description = Notes */
        NOTES                   ARRAY 0:2 DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Purchase order number */
        CUSTPO                  DATATYPE IS TEXT SIZE IS 20.

        END OE_REGHEADER_CDD STRUCTURE.

END OE_REGHEADER.
