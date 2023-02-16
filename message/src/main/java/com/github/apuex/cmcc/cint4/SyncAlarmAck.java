package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 告警同步确认
 */
public class SyncAlarmAck extends Message {
    public SyncAlarmAck
    ( EnumResult Result
    ) {
        super(EnumPKType.SYNC_ALARM_ACK);
        this.Result = Result;
    }

    public SyncAlarmAck
    ( int SerialNo
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.SYNC_ALARM_ACK);
        this.Result = Result;
    }

    public EnumResult Result; // 报文返回结果
}

