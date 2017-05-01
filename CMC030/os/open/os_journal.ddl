DEFINE RECORD CDD$TOP.OS.OS_JOURNAL

        DESCRIPTION IS /*Main Journal Header*/.

        OS_JOURNAL_CDD STRUCTURE.

        /* Element =
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Order Date */
        ORDDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Order Type */
        ORDTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Order Category */
        ORDCAT                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Order Discount */
        DISC                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Miscellaneous Charges */
        MISC                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Shipping Name */
        SHIPNAM                 DATATYPE IS TEXT SIZE IS 50.

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
        Description = Date */
        SHIPDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Ship Via */
        SHIPVIA                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Terms */
        TERMS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Taxes */
        SALESTAX                DATATYPE IS G_FLOATING.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Commission amount */
        COMMAMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Commission percentage */
        COMMPERC                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Salesmen */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Misc Charges Reason Code */
        CREASON                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Commission for salesmen */
        SALCOMM                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Handling Amount */
        HANDLING                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Paid Amount */
        AMTPAID                 DATATYPE IS G_FLOATING.

        /* Element = CHECK
        Description = Check number */
        CHECK                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Notes */
        NOTES                   ARRAY 0:2 DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Miscellaneous Account */
        MISCACCT                DATATYPE IS TEXT SIZE IS 18.

        /* Element = DATE
        Description = Transaction Date (YYYYMMDD) */
        TRANDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Transaction Time (HHMMSS) */
        TRANTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Invoice Number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Freight Number */
        FREIGHT                 DATATYPE IS G_FLOATING.

        /* Element = TAXCODE
        Description = Tax code */
        TAXCODE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = TAXFLAG
        Description = Tax Flag */
        TAXFLAG                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = LINE
        Description = Shipping Line Address Code */
        SHIPLIN                 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Number of payments */
        PAYMNT                  DATATYPE IS SIGNED WORD.

        /* Element = FLAG
        Description = Register Flag - Order Exists in Register */
        REG_FLAG                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Purchase order number */
        CUSTPO                  DATATYPE IS TEXT SIZE IS 20.

        END OS_JOURNAL_CDD STRUCTURE.

END OS_JOURNAL.
