$PACKAGE SBLDevAA
SUBROUTINE SB.REPAYMENT.AMOUNT
*******************************************************************************
* Purpose : Return the amount of the latest generated non-disbursement AA bill.
*
* Input   : EB.Reports.getOData() = AA arrangement id.
* Output  : EB.Reports.setOData() = AA.BILL.DETAILS OR.TOTAL.AMOUNT.
*
* This is bill-based only.  It does not project future schedules.
*******************************************************************************

    $USING EB.Reports
    $USING AA.PaymentSchedule

    ARR.ID = EB.Reports.getOData()
    LAST.BILL.AMT = 0

    IF NOT(ARR.ID) THEN
        EB.Reports.setOData(LAST.BILL.AMT)
        RETURN
    END

    ERR = ''
    R.AA.ACC.DET = AA.PaymentSchedule.AccountDetails.Read(ARR.ID,ERR)
    IF NOT(R.AA.ACC.DET) THEN
        EB.Reports.setOData(LAST.BILL.AMT)
        RETURN
    END

    BILL.IDS = R.AA.ACC.DET<AA.PaymentSchedule.AccountDetails.AdBillId>
    BILL.PAY.DATES = R.AA.ACC.DET<AA.PaymentSchedule.AccountDetails.AdBillPayDate>
    BILL.DATES = R.AA.ACC.DET<AA.PaymentSchedule.AccountDetails.AdBillDate>
    BILL.TYPES = R.AA.ACC.DET<AA.PaymentSchedule.AccountDetails.AdBillType>

    SELECTED.BILL.ID = ''
    SELECTED.DATE = ''
    BILL.SEEN = ''

    GOSUB SELECT.LAST.BILL

    IF SELECTED.BILL.ID THEN
        GOSUB READ.SELECTED.BILL
    END

    EB.Reports.setOData(LAST.BILL.AMT)

RETURN

*******************************************************************************
SELECT.LAST.BILL:

    VM.CNT = DCOUNT(BILL.IDS,@VM)
    IF VM.CNT LT 1 THEN VM.CNT = 1

    FOR VM.POS = 1 TO VM.CNT
        SM.CNT = DCOUNT(BILL.IDS<1,VM.POS>,@SM)
        IF SM.CNT LT 1 THEN SM.CNT = 1

        FOR SM.POS = 1 TO SM.CNT
            BILL.ID = BILL.IDS<1,VM.POS,SM.POS>
            IF NOT(BILL.ID) THEN BILL.ID = BILL.IDS<1,VM.POS>
            IF NOT(BILL.ID) THEN CONTINUE

            LOCATE BILL.ID IN BILL.SEEN<1> SETTING SEEN.POS THEN CONTINUE
            BILL.SEEN<-1> = BILL.ID

            BILL.TYPE = BILL.TYPES<1,VM.POS,SM.POS>
            IF NOT(BILL.TYPE) THEN BILL.TYPE = BILL.TYPES<1,VM.POS>
            IF OCONV(BILL.TYPE,'MCU') EQ 'DISBURSEMENT' THEN CONTINUE

            BILL.DATE = BILL.PAY.DATES<1,VM.POS,SM.POS>
            IF NOT(BILL.DATE) THEN BILL.DATE = BILL.PAY.DATES<1,VM.POS>
            IF NOT(BILL.DATE) THEN BILL.DATE = BILL.DATES<1,VM.POS,SM.POS>
            IF NOT(BILL.DATE) THEN BILL.DATE = BILL.DATES<1,VM.POS>

            IF (NOT(SELECTED.BILL.ID)) OR (BILL.DATE GE SELECTED.DATE) THEN
                SELECTED.BILL.ID = BILL.ID
                SELECTED.DATE = BILL.DATE
            END
        NEXT SM.POS
    NEXT VM.POS

RETURN

*******************************************************************************
READ.SELECTED.BILL:

    ERR = ''
    R.AA.BILL.DET = AA.PaymentSchedule.BillDetails.Read(SELECTED.BILL.ID,ERR)

    IF NOT(R.AA.BILL.DET) THEN
        NORMALISED.BILL.ID = SELECTED.BILL.ID
        CONVERT '/' TO '' IN NORMALISED.BILL.ID
        IF NORMALISED.BILL.ID NE SELECTED.BILL.ID THEN
            ERR = ''
            R.AA.BILL.DET = AA.PaymentSchedule.BillDetails.Read(NORMALISED.BILL.ID,ERR)
        END
    END

    IF NOT(R.AA.BILL.DET) THEN RETURN

    BILL.TYPE = R.AA.BILL.DET<AA.PaymentSchedule.BillDetails.BdBillType>
    IF OCONV(BILL.TYPE,'MCU') EQ 'DISBURSEMENT' THEN RETURN

    LAST.BILL.AMT = R.AA.BILL.DET<AA.PaymentSchedule.BillDetails.BdOrTotalAmount> + 0

RETURN

END