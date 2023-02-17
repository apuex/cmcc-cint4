package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 告警同步确认
 */
public class SyncAlarmAck extends Message { 
    public SyncAlarmAck() {
        super(EnumPKType.SYNC_ALARM_ACK);
    }

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

    public static void encode(ByteBuffer buf, SyncAlarmAck v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        buf.putInt(v.Result.getValue());
        buf.putShort(v.CRC16);
    }

    public static SyncAlarmAck decode(ByteBuffer buf) {
        SyncAlarmAck v = new SyncAlarmAck();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        v.Result = EnumResult.fromValue(buf.getInt());
        v.CRC16 = buf.getShort();
        return v;
    }

    public EnumResult Result; // 报文返回结果
}

