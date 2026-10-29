$PACKAGE SBLDevAA
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
SUBROUTINE E.AA.SCHEDULE.PROJECTOR.SB(SCHED.ARR)
*******************************************************************************
* Purpose:
*   Project AA schedule rows for an enquiry and return only the fields required
*   by the SB enquiry view.
*
* Input:
*   Enquiry selections:
*     ARRANGEMENT.ID
*     SIM.REF       optional
*     DATE.FROM     optional
*     DATE.TO       optional
*
* Output:
*   SCHED.ARR multivalue rows, caret-delimited:
*     V1  D.SCHEDULE.DATE
*     V2  D.DEFER.DATE
*     V3  TOT.DUE
*     V4  TOT.CAP
*     V5  TOTAL.PRINCIPAL
*     V6  TOTAL.INTEREST
*     V7  TOTAL.CHARGE field carries DEF.TAX.AMT for SB enquiry display
*     V8  OUTSTANDING
*     V9  TOTAL.TAX
*     V10 TOTAL.PYMT
*     V11 DEFERRED.INTEREST
*     V12 DEFERRED.TAX
*     V13 LOANORGFEE      Loan origination fee
*     V14 EARLYREPAYFEE   Early repayment fee
*     V15 FIREINS         Fire insurance
*     V16 LIFEINSURFEE    Life insurance
*     V17 LOANORGFEE-STMPDUTY / any property ending -STMPDUTY
*     V18 INSURANCEFEE
*     V19 L.TOT.CH        Original TOTAL.CHARGE value from projector
*     V20 PRIME.RATE      Latest BASIC.INTEREST rate for floating index/ccy
*     V21 STMPDUTYFEE
*
* Notes:
*   Deferred interest and tax are actual billed values only: the routine matches
*   the schedule date to generated bills on AA.ACCOUNT.DETAILS, reads
*   AA.BILL.DETAILS, and sums PRINCIPALINT / stamp-duty-tax properties when
*   BILL.STATUS is not SETTLED and BILL.PAY.DATE is before TODAY.
*******************************************************************************

    $USING AA.PaymentSchedule
    $USING AA.ProductFramework
    $USING EB.Reports
    $INSERT I_COMMON
    $INSERT I_F.COMPANY
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.INTEREST
    $INSERT I_F.BASIC.INTEREST

    GOSUB INITIALISE
    IF NOT(ARR.ID) THEN RETURN

    GOSUB PROJECT.SCHEDULE
    GOSUB GET.PRIME.RATE
    GOSUB BUILD.BILLED.DEFERRED.INDEX
    GOSUB BUILD.OUTPUT

RETURN

*******************************************************************************
INITIALISE:

    SCHED.ARR = ''

    ARR.ID = ''
    SIM.REF = ''
    CYCLE.DATE = ''

    TOT.PAYMENT = ''
    DUE.DATES = ''
    DEFER.DATES = ''
    DUE.TYPES = ''
    DUE.METHODS = ''
    DUE.TYPE.AMTS = ''
    DUE.PROPS = ''
    DUE.PROP.AMTS = ''
    DUE.OUTS = ''
    DEF.DATE.LIST = ''
    DEF.INT.LIST = ''
    DEF.TAX.LIST = ''
    PRIME.RATE = ''

    ENQ.SEL = EB.Reports.getEnqSelection()

    LOCATE 'ARRANGEMENT.ID' IN ENQ.SEL<2,1> SETTING ARR.POS THEN
        ARR.ID = ENQ.SEL<4,ARR.POS>
    END

    LOCATE 'SIM.REF' IN ENQ.SEL<2,1> SETTING SIM.POS THEN
        SIM.REF = ENQ.SEL<4,SIM.POS>
    END

    LOCATE 'DATE.FROM' IN ENQ.SEL<2,1> SETTING DT.FROM.POS THEN
        CYCLE.DATE = ENQ.SEL<4,DT.FROM.POS>
    END

    LOCATE 'DATE.TO' IN ENQ.SEL<2,1> SETTING DT.TO.POS THEN
        CYCLE.DATE := @FM:ENQ.SEL<4,DT.TO.POS>
    END

RETURN

*******************************************************************************
PROJECT.SCHEDULE:

    AA.PaymentSchedule.ScheduleProjector(ARR.ID,SIM.REF,'',CYCLE.DATE,TOT.PAYMENT,DUE.DATES,DEFER.DATES,DUE.TYPES,DUE.METHODS,DUE.TYPE.AMTS,DUE.PROPS,DUE.PROP.AMTS,DUE.OUTS)

    IF NOT(DUE.DATES) AND DEFER.DATES THEN DUE.DATES = DEFER.DATES

RETURN

*******************************************************************************
GET.PRIME.RATE:

    PRIME.RATE = ''

    RATE.MNE = R.COMPANY(EB.COM.MNEMONIC)
    GOSUB OPEN.RATE.FILES

    R.RATE.ARR = '' ; RATE.ERR = ''
    CALL F.READ(FN.RATE.ARR,ARR.ID,R.RATE.ARR,F.RATE.ARR,RATE.ERR)

    IF NOT(R.RATE.ARR) AND RATE.MNE THEN
        RATE.MNE = ''
        GOSUB OPEN.RATE.FILES
        R.RATE.ARR = '' ; RATE.ERR = ''
        CALL F.READ(FN.RATE.ARR,ARR.ID,R.RATE.ARR,F.RATE.ARR,RATE.ERR)
    END

    IF NOT(R.RATE.ARR) THEN RETURN

    ARR.CCY = R.RATE.ARR<AA.ARR.CURRENCY>
    IF NOT(ARR.CCY) THEN RETURN

    R.RATE.XREF = '' ; RATE.ERR = ''
    CALL F.READ(FN.RATE.XREF,ARR.ID,R.RATE.XREF,F.RATE.XREF,RATE.ERR)
    IF NOT(R.RATE.XREF) THEN RETURN

    RATE.PROPS = R.RATE.XREF<1>
    RATE.UPD.DATES = R.RATE.XREF<5>

    LOCATE 'PRINCIPALINT' IN RATE.PROPS<1,1> SETTING RATE.INT.POS ELSE RATE.INT.POS = ''
    IF NOT(RATE.INT.POS) THEN RETURN

    RATE.INT.DATES = RAISE(RATE.UPD.DATES<1,RATE.INT.POS>)
    LAST.RATE.INT.DATE = RATE.INT.DATES<1,1>
    IF NOT(LAST.RATE.INT.DATE) THEN RETURN

    RATE.INT.ID = ARR.ID:'-PRINCIPALINT-':LAST.RATE.INT.DATE
    R.RATE.INT = '' ; RATE.ERR = ''
    CALL F.READ(FN.RATE.INT,RATE.INT.ID,R.RATE.INT,F.RATE.INT,RATE.ERR)
    IF NOT(R.RATE.INT) THEN RETURN

    FLOATING.INDEX = R.RATE.INT<AA.INT.FLOATING.INDEX>
    IF NOT(FLOATING.INDEX) THEN FLOATING.INDEX = R.RATE.INT<AA.INT.PERIODIC.INDEX>
    IF NOT(FLOATING.INDEX) THEN RETURN

    BI.PREFIX = FLOATING.INDEX:ARR.CCY
    BI.LIST = '' ; BI.NO = '' ; BI.ERR = ''
    SEL.BI = 'SELECT ':FN.BASIC.INTEREST:' WITH @ID LIKE ':BI.PREFIX:'... BY @ID'
    CALL EB.READLIST(SEL.BI,BI.LIST,'',BI.NO,BI.ERR)
    IF BI.NO LT 1 THEN RETURN

    BI.ID = BI.LIST<BI.NO>
    R.BI = '' ; BI.ERR = ''
    CALL F.READ(FN.BASIC.INTEREST,BI.ID,R.BI,F.BASIC.INTEREST,BI.ERR)
    IF R.BI THEN PRIME.RATE = R.BI<EB.BIN.INTEREST.RATE>

RETURN

*******************************************************************************
OPEN.RATE.FILES:

    IF RATE.MNE THEN
        FN.RATE.ARR = 'F':RATE.MNE:'.AA.ARRANGEMENT'
        FN.RATE.XREF = 'F':RATE.MNE:'.AA.ARRANGEMENT.DATED.XREF'
        FN.RATE.INT = 'F':RATE.MNE:'.AA.ARR.INTEREST'
    END ELSE
        FN.RATE.ARR = 'F.AA.ARRANGEMENT'
        FN.RATE.XREF = 'F.AA.ARRANGEMENT.DATED.XREF'
        FN.RATE.INT = 'F.AA.ARR.INTEREST'
    END

    F.RATE.ARR = '' ; CALL OPF(FN.RATE.ARR,F.RATE.ARR)
    F.RATE.XREF = '' ; CALL OPF(FN.RATE.XREF,F.RATE.XREF)
    F.RATE.INT = '' ; CALL OPF(FN.RATE.INT,F.RATE.INT)

    FN.BASIC.INTEREST = 'F.BASIC.INTEREST'
    F.BASIC.INTEREST = ''
    CALL OPF(FN.BASIC.INTEREST,F.BASIC.INTEREST)

RETURN

*******************************************************************************
BUILD.OUTPUT:

    TOT.DTES = DCOUNT(DUE.DATES,@FM)

    FOR DCNT = 1 TO TOT.DTES
        DUE.DATE = DUE.DATES<DCNT>
        IF NOT(DUE.DATE) THEN CONTINUE

        DEFER.DATE = DEFER.DATES<DCNT>
        CURRENT.OS = DUE.OUTS<DCNT>
        TOT.PAYM = TOT.PAYMENT<DCNT>

        GOSUB SPLIT.PROJECTED.AMOUNTS
        GOSUB GET.DEFERRED.FOR.DATE

        IF NOT(TOT.PAYM) THEN TOT.PAYM = TOT.DUE.PAYM
        OUT.PRIME.RATE = ''
        IF DCNT EQ 1 THEN OUT.PRIME.RATE = PRIME.RATE

        SCHED.ARR<-1> = DUE.DATE:'^':DEFER.DATE:'^':TOT.DUE.PAYM:'^':TOT.CAP.PAYM:'^':TOT.PRIN.PAYM:'^':TOT.INT.PAYM:'^':DEF.TAX.AMT:'^':CURRENT.OS:'^':TOT.TAX.PAYM:'^':TOT.PAY.PAYM:'^':DEF.INT.AMT:'^':DEF.TAX.AMT:'^':LOANORG.FEE:'^':EARLYREPAY.FEE:'^':FIRE.INS:'^':LIFEINSUR.FEE:'^':LOANORG.STMPDUTY:'^':INSURANCE.FEE:'^':TOT.CHG.PAYM:'^':OUT.PRIME.RATE:'^':STMPDUTY.FEE
    NEXT DCNT

RETURN

*******************************************************************************
SPLIT.PROJECTED.AMOUNTS:

    TOT.CAP.PAYM = 0
    TOT.INT.PAYM = 0
    TOT.PRIN.PAYM = 0
    TOT.CHG.PAYM = 0
    TOT.DUE.PAYM = 0
    TOT.TAX.PAYM = 0
    TOT.PAY.PAYM = 0
    DEF.INT.AMT = 0
    DEF.TAX.AMT = 0
    LOANORG.FEE = 0
    EARLYREPAY.FEE = 0
    FIRE.INS = 0
    LIFEINSUR.FEE = 0
    LOANORG.STMPDUTY = 0
    INSURANCE.FEE = 0
    STMPDUTY.FEE = 0

    TOT.PAY.TYPE = DCOUNT(DUE.TYPES<DCNT>,@VM)
    IF TOT.PAY.TYPE LT 1 THEN TOT.PAY.TYPE = 1

    FOR PAY.CNT = 1 TO TOT.PAY.TYPE
        PROP.LIST = DUE.PROPS<DCNT,PAY.CNT>
        PROP.LIST = RAISE(PROP.LIST)

        PROP.CLS.LIST = ''
        AA.ProductFramework.GetPropertyClass(PROP.LIST,PROP.CLS.LIST)

        TOT.PROP = DCOUNT(PROP.LIST,@VM)
        IF TOT.PROP LT 1 THEN TOT.PROP = 1

        FOR PROP.CNT = 1 TO TOT.PROP
            PROP.NAME = PROP.LIST<1,PROP.CNT>
            PROP.NAME.UPPER = OCONV(PROP.NAME,'MCU')
            IF NOT(PROP.NAME.UPPER) THEN CONTINUE

            PROP.CLS = PROP.CLS.LIST<1,PROP.CNT>
            PROP.AMT = DUE.PROP.AMTS<DCNT,PAY.CNT,PROP.CNT> + 0

            IF LEN(PROP.NAME.UPPER) GE 9 THEN
                IF PROP.NAME.UPPER[LEN(PROP.NAME.UPPER)-8,9] EQ '-STMPDUTY' THEN LOANORG.STMPDUTY += PROP.AMT
            END
            IF PROP.NAME.UPPER EQ 'STMPDUTYFEE' THEN STMPDUTY.FEE += PROP.AMT

            TAX.SIGN = 1
            TAX.PROP.POS = ''
            IS.TAX = 0

            IF INDEX(PROP.NAME.UPPER,'TAX',1) OR INDEX(PROP.NAME.UPPER,'DUTY',1) THEN
                IS.TAX = 1
            END

            IF PROP.CLS EQ '' AND INDEX(PROP.NAME.UPPER,'-',1) THEN
                BASE.PROP = FIELD(PROP.NAME.UPPER,'-',1)
                LOCATE BASE.PROP IN PROP.LIST<1,1> SETTING TAX.PROP.POS THEN
                    IS.TAX = 1
                    PROP.CLS = PROP.CLS.LIST<1,TAX.PROP.POS>
                    TAX.SIGN = -1
                END
            END

            DUE.METHOD = DUE.METHODS<DCNT,PAY.CNT,PROP.CNT>
            IF TAX.PROP.POS THEN DUE.METHOD = DUE.METHODS<DCNT,PAY.CNT,TAX.PROP.POS>

            BEGIN CASE
                CASE IS.TAX
                    TOT.TAX.PAYM += PROP.AMT
                CASE PROP.CLS EQ 'ACCOUNT'
                    TOT.PRIN.PAYM += PROP.AMT
                CASE PROP.CLS EQ 'INTEREST'
                    TOT.INT.PAYM += PROP.AMT
                CASE PROP.CLS EQ 'CHARGE'
                    TOT.CHG.PAYM += PROP.AMT
                    GOSUB ADD.TARGET.CHARGE
            END CASE

            BEGIN CASE
                CASE DUE.METHOD EQ 'DUE' OR DUE.METHOD EQ 'INFO'
                    TOT.DUE.PAYM += PROP.AMT
                CASE DUE.METHOD EQ 'CAPITALISE'
                    TOT.CAP.PAYM += PROP.AMT * TAX.SIGN
                CASE DUE.METHOD EQ 'PAY'
                    TOT.PAY.PAYM += PROP.AMT * TAX.SIGN
            END CASE

        NEXT PROP.CNT
    NEXT PAY.CNT

RETURN

*******************************************************************************
ADD.TARGET.CHARGE:

    BEGIN CASE
        CASE PROP.NAME.UPPER EQ 'LOANORGFEE'
            LOANORG.FEE += PROP.AMT
        CASE PROP.NAME.UPPER EQ 'EARLYREPAYFEE'
            EARLYREPAY.FEE += PROP.AMT
        CASE PROP.NAME.UPPER EQ 'FIREINS'
            FIRE.INS += PROP.AMT
        CASE PROP.NAME.UPPER EQ 'LIFEINSURFEE'
            LIFEINSUR.FEE += PROP.AMT
        CASE PROP.NAME.UPPER EQ 'INSURANCEFEE'
            INSURANCE.FEE += PROP.AMT
    END CASE

RETURN

*******************************************************************************
BUILD.BILLED.DEFERRED.INDEX:

    ERR = ''
    R.AA.ACC.DET = AA.PaymentSchedule.AccountDetails.Read(ARR.ID,ERR)
    IF NOT(R.AA.ACC.DET) THEN RETURN

    BILL.IDS = R.AA.ACC.DET<AA.PaymentSchedule.AccountDetails.AdBillId>
    BILL.PAY.DATES = R.AA.ACC.DET<AA.PaymentSchedule.AccountDetails.AdBillPayDate>
    BILL.DATES = R.AA.ACC.DET<AA.PaymentSchedule.AccountDetails.AdBillDate>
    BILL.TYPES = R.AA.ACC.DET<AA.PaymentSchedule.AccountDetails.AdBillType>
    BILL.SEEN = ''

    BILL.VM.CNT = DCOUNT(BILL.IDS,@VM)
    IF BILL.VM.CNT LT 1 THEN BILL.VM.CNT = 1

    FOR BILL.VM.POS = 1 TO BILL.VM.CNT
        MATCH.DATE = BILL.PAY.DATES<1,BILL.VM.POS>
        IF NOT(MATCH.DATE) THEN MATCH.DATE = BILL.DATES<1,BILL.VM.POS>
        IF NOT(MATCH.DATE) THEN CONTINUE
        IF MATCH.DATE GE TODAY THEN CONTINUE

        BILL.SM.CNT = DCOUNT(BILL.IDS<1,BILL.VM.POS>,@SM)
        IF BILL.SM.CNT LT 1 THEN BILL.SM.CNT = 1

        FOR BILL.SM.POS = 1 TO BILL.SM.CNT
            BILL.ID = BILL.IDS<1,BILL.VM.POS,BILL.SM.POS>
            IF NOT(BILL.ID) THEN BILL.ID = BILL.IDS<1,BILL.VM.POS>
            IF NOT(BILL.ID) THEN CONTINUE

            BILL.TYPE = BILL.TYPES<1,BILL.VM.POS,BILL.SM.POS>
            IF NOT(BILL.TYPE) THEN BILL.TYPE = BILL.TYPES<1,BILL.VM.POS>
            IF OCONV(BILL.TYPE,'MCU') EQ 'DISBURSEMENT' THEN CONTINUE

            LOCATE BILL.ID IN BILL.SEEN<1> SETTING BILL.SEEN.POS THEN CONTINUE
            BILL.SEEN<1,-1> = BILL.ID

            BILL.DEF.INT = 0
            BILL.DEF.TAX = 0
            GOSUB READ.BILL.DEFERRED.AMOUNTS
            IF BILL.DEF.INT OR BILL.DEF.TAX THEN GOSUB ADD.BILL.DEFERRED.TO.INDEX
        NEXT BILL.SM.POS
    NEXT BILL.VM.POS

RETURN

*******************************************************************************
ADD.BILL.DEFERRED.TO.INDEX:

    LOCATE MATCH.DATE IN DEF.DATE.LIST<1> SETTING DEF.POS THEN
        DEF.INT.LIST<1,DEF.POS> += BILL.DEF.INT
        DEF.TAX.LIST<1,DEF.POS> += BILL.DEF.TAX
    END ELSE
        DEF.DATE.LIST<1,-1> = MATCH.DATE
        DEF.POS = DCOUNT(DEF.DATE.LIST<1>,@VM)
        DEF.INT.LIST<1,DEF.POS> = BILL.DEF.INT
        DEF.TAX.LIST<1,DEF.POS> = BILL.DEF.TAX
    END

RETURN

*******************************************************************************
GET.DEFERRED.FOR.DATE:

    LOCATE DUE.DATE IN DEF.DATE.LIST<1> SETTING DEF.POS THEN
        DEF.INT.AMT = DEF.INT.LIST<1,DEF.POS>
        DEF.TAX.AMT = DEF.TAX.LIST<1,DEF.POS>
    END

RETURN

*******************************************************************************
READ.BILL.DEFERRED.AMOUNTS:

    ERR = ''
    R.AA.BILL.DET = AA.PaymentSchedule.BillDetails.Read(BILL.ID,ERR)

    IF NOT(R.AA.BILL.DET) THEN
        NORMALISED.BILL.ID = BILL.ID
        CONVERT '/' TO '' IN NORMALISED.BILL.ID
        IF NORMALISED.BILL.ID NE BILL.ID THEN
            ERR = ''
            R.AA.BILL.DET = AA.PaymentSchedule.BillDetails.Read(NORMALISED.BILL.ID,ERR)
        END
    END

    IF NOT(R.AA.BILL.DET) THEN RETURN

    BILL.PROPS = R.AA.BILL.DET<AA.PaymentSchedule.BillDetails.BdProperty>
    IF NOT(BILL.PROPS) THEN BILL.PROPS = R.AA.BILL.DET<AA.PaymentSchedule.BillDetails.BdPayProperty>

    BILL.OS.AMTS = R.AA.BILL.DET<AA.PaymentSchedule.BillDetails.BdOsPropAmount>
    BILL.REPAY.AMTS = R.AA.BILL.DET<AA.PaymentSchedule.BillDetails.BdRepayAmount>
    BILL.STATUSES = R.AA.BILL.DET<AA.PaymentSchedule.BillDetails.BdBillStatus>
    BILL.ALL.STATUS = OCONV(BILL.STATUSES,'MCU')
    IF INDEX(BILL.ALL.STATUS,'SETTLED',1) AND NOT(INDEX(BILL.ALL.STATUS,'DUE',1)) THEN RETURN

    PROP.VM.CNT = DCOUNT(BILL.PROPS,@VM)
    IF PROP.VM.CNT LT 1 THEN PROP.VM.CNT = 1

    FOR PROP.VM.POS = 1 TO PROP.VM.CNT
        PROP.SM.CNT = DCOUNT(BILL.PROPS<1,PROP.VM.POS>,@SM)
        IF PROP.SM.CNT LT 1 THEN PROP.SM.CNT = 1

        FOR PROP.SM.POS = 1 TO PROP.SM.CNT
            BILL.PROP = BILL.PROPS<1,PROP.VM.POS,PROP.SM.POS>
            IF NOT(BILL.PROP) THEN BILL.PROP = BILL.PROPS<1,PROP.VM.POS>
            BILL.PROP = OCONV(BILL.PROP,'MCU')
            IF NOT(BILL.PROP) THEN CONTINUE

            BILL.AMT = BILL.OS.AMTS<1,PROP.VM.POS,PROP.SM.POS>
            IF NOT(BILL.AMT) THEN BILL.AMT = BILL.OS.AMTS<1,PROP.VM.POS>
            IF NOT(BILL.AMT) THEN BILL.AMT = BILL.REPAY.AMTS<1,PROP.VM.POS,PROP.SM.POS>
            IF NOT(BILL.AMT) THEN BILL.AMT = BILL.REPAY.AMTS<1,PROP.VM.POS>
            BILL.AMT += 0
            IF NOT(BILL.AMT) THEN CONTINUE

            BILL.STATUS = BILL.STATUSES<1,PROP.VM.POS,PROP.SM.POS>
            IF NOT(BILL.STATUS) THEN BILL.STATUS = BILL.STATUSES<1,PROP.VM.POS>
            BILL.STATUS = OCONV(BILL.STATUS,'MCU')

            IF BILL.STATUS EQ 'SETTLED' THEN CONTINUE

            BEGIN CASE
                CASE BILL.PROP EQ 'PRINCIPALINT'
                    BILL.DEF.INT += BILL.AMT
                CASE INDEX(BILL.PROP,'STMPDUTY',1) OR INDEX(BILL.PROP,'STAMPDUTY',1) OR INDEX(BILL.PROP,'TAX',1)
                    BILL.DEF.TAX += BILL.AMT
            END CASE
        NEXT PROP.SM.POS
    NEXT PROP.VM.POS

RETURN

END
